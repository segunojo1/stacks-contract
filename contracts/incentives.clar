;; governance.clar
;; Handles platform governance and incentive mechanisms for the API marketplace

(define-data-var platform-fee-percentage uint u5)  ;; 5% platform fee
(define-data-var min-proposal-stake uint u500000000)  ;; 500 STX
(define-data-var proposal-voting-period uint u1008)  ;; 7 days (144 blocks per day)
(define-data-var next-proposal-id uint u0)

;; Platform treasury
(define-data-var treasury-balance uint u0)

;; Proposal types
(define-constant PROPOSAL-TYPE-PARAMETER u1)  ;; For changing platform parameters
(define-constant PROPOSAL-TYPE-FEATURE u2)    ;; For adding new features
(define-constant PROPOSAL-TYPE-UPGRADE u3)    ;; For contract upgrades
(define-constant PROPOSAL-TYPE-FUND u4)       ;; For treasury fund allocation

;; Proposal states
(define-constant PROPOSAL-STATE-ACTIVE u1)
(define-constant PROPOSAL-STATE-PASSED u2)
(define-constant PROPOSAL-STATE-REJECTED u3)
(define-constant PROPOSAL-STATE-EXECUTED u4)

;; Maps
(define-map proposals
  uint  ;; proposal-id
  {
    title: (string-ascii 64),
    description: (string-ascii 512),
    proposer: principal,
    proposal-type: uint,
    parameters: (string-utf8 1024),  ;; JSON string with parameters
    stake: uint,
    created-at: uint,
    expires-at: uint,
    state: uint,
    yes-votes: uint,
    no-votes: uint,
    executed-at: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,  ;; true = yes, false = no
    weight: uint,
    timestamp: uint
  }
)

(define-map staking-positions
  principal
  {
    amount: uint,
    lock-period: uint,  ;; in blocks
    locked-until: uint,
    voting-power: uint
  }
)

(define-map developer-reputation
  principal
  {
    api-count: uint,
    total-users: uint,
    quality-score: uint,  ;; 0-100
    rewards-earned: uint
  }
)

;; Public functions

;; Staking
(define-public (stake-tokens (amount uint) (lock-period uint))
  (let (
    (current-time block-height)
    (existing-stake (default-to { amount: u0, lock-period: u0, locked-until: u0, voting-power: u0 }
                               (map-get? staking-positions tx-sender)))
    ;; Calculate voting power based on amount and lock period
    (voting-power (calculate-voting-power amount lock-period))
  )
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update staking position
    (map-set staking-positions tx-sender {
      amount: (+ (get amount existing-stake) amount),
      lock-period: lock-period,
      locked-until: (+ current-time lock-period),
      voting-power: (+ (get voting-power existing-stake) voting-power)
    })
    
    (ok voting-power)))

(define-public (unstake-tokens)
  (let (
    (current-time block-height)
    (position (unwrap! (map-get? staking-positions tx-sender) (err u404)))
  )
    ;; Check if lock period has expired
    (asserts! (>= current-time (get locked-until position)) (err u403))
    
    ;; Transfer tokens back to user
    (try! (as-contract (stx-transfer? (get amount position) tx-sender tx-sender)))
    
    ;; Clear staking position
    (map-delete staking-positions tx-sender)
    
    (ok (get amount position))))

;; Proposals
(define-public (create-proposal 
    (title (string-ascii 64))
    (description (string-ascii 512))
    (proposal-type uint)
    (parameters (string-utf8 1024)))
  (let (
    (stake (var-get min-proposal-stake))
    (current-time block-height)
    (proposal-id (var-get next-proposal-id))
    (position (unwrap! (map-get? staking-positions tx-sender) (err u403)))
  )
    ;; Check if user has enough voting power
    (asserts! (>= (get voting-power position) stake) (err u403))
    
    ;; Validate proposal type
    (asserts! (or (is-eq proposal-type PROPOSAL-TYPE-PARAMETER)
                  (is-eq proposal-type PROPOSAL-TYPE-FEATURE)
                  (is-eq proposal-type PROPOSAL-TYPE-UPGRADE)
                  (is-eq proposal-type PROPOSAL-TYPE-FUND))
             (err u400))
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      proposal-type: proposal-type,
      parameters: parameters,
      stake: stake,
      created-at: current-time,
      expires-at: (+ current-time (var-get proposal-voting-period)),
      state: PROPOSAL-STATE-ACTIVE,
      yes-votes: u0,
      no-votes: u0,
      executed-at: u0
    })
    
    ;; Increment proposal ID
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err u404)))
    (current-time block-height)
    (position (unwrap! (map-get? staking-positions tx-sender) (err u403)))
    (voting-power (get voting-power position))
  )
    ;; Check if proposal is still active
    (asserts! (< current-time (get expires-at proposal)) (err u403))
    (asserts! (is-eq (get state proposal) PROPOSAL-STATE-ACTIVE) (err u403))
    
    ;; Check if user has already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) (err u409))
    
    ;; Record vote
    (map-set votes { proposal-id: proposal-id, voter: tx-sender } {
      vote: vote,
      weight: voting-power,
      timestamp: current-time
    })
    
    ;; Update proposal vote counts
    (if vote
        (map-set proposals proposal-id 
          (merge proposal { yes-votes: (+ (get yes-votes proposal) voting-power) }))
        (map-set proposals proposal-id 
          (merge proposal { no-votes: (+ (get no-votes proposal) voting-power) })))
    
    (ok voting-power)))

(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err u404)))
    (current-time block-height)
  )
    ;; Check if voting period has ended
    (asserts! (>= current-time (get expires-at proposal)) (err u403))
    (asserts! (is-eq (get state proposal) PROPOSAL-STATE-ACTIVE) (err u403))
    
    ;; Determine if proposal passed
    (if (> (get yes-votes proposal) (get no-votes proposal))
        (map-set proposals proposal-id (merge proposal { state: PROPOSAL-STATE-PASSED }))
        (map-set proposals proposal-id (merge proposal { state: PROPOSAL-STATE-REJECTED })))
    
    (ok true)))

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err u404)))
    (current-time block-height)
  )
    ;; Check if proposal passed
    (asserts! (is-eq (get state proposal) PROPOSAL-STATE-PASSED) (err u403))
    
    ;; Execute based on proposal type
    (match (get proposal-type proposal)
      PROPOSAL-TYPE-PARAMETER (try! (execute-parameter-change proposal-id))
      PROPOSAL-TYPE-FEATURE (try! (execute-feature-addition proposal-id))
      PROPOSAL-TYPE-UPGRADE (try! (execute-contract-upgrade proposal-id))
      PROPOSAL-TYPE-FUND (try! (execute-fund-allocation proposal-id))
      (err u400))
    
    ;; Mark proposal as executed
    (map-set proposals proposal-id 
      (merge proposal { 
        state: PROPOSAL-STATE-EXECUTED,
        executed-at: current-time
      }))
    
    (ok true)))

;; Platform fee collection and distribution
(define-public (collect-platform-fee (amount uint))
  (begin
    ;; This would be called by other contracts when processing payments
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)))

(define-public (distribute-rewards)
  (let (
    (treasury (var-get treasury-balance))
    ;; In a real implementation, we would calculate reward distribution
    ;; based on developer reputation, user activity, etc.
  )
    ;; For demonstration, we're not implementing the full distribution logic
    (ok true)))

;; Developer reputation
(define-public (update-developer-reputation
    (developer principal)
    (api-count uint)
    (total-users uint)
    (quality-score uint))
  (begin
    ;; Only authorized contracts should call this
    (asserts! (is-contract-caller) (err u403))
    
    (let ((current-rep (default-to { api-count: u0, total-users: u0, quality-score: u50, rewards-earned: u0 }
                                  (map-get? developer-reputation developer))))
      (map-set developer-reputation developer {
        api-count: api-count,
        total-users: total-users,
        quality-score: quality-score,
        rewards-earned: (get rewards-earned current-rep)
      }))
    
    (ok true)))

(define-public (reward-developer (developer principal) (amount uint))
  (begin
    ;; Only authorized contracts should call this
    (asserts! (is-contract-caller) (err u403))
    
    (let ((current-rep (default-to { api-count: u0, total-users: u0, quality-score: u50, rewards-earned: u0 }
                                  (map-get? developer-reputation developer))))
      (map-set developer-reputation developer
        (merge current-rep { rewards-earned: (+ (get rewards-earned current-rep) amount) })))
    
    (ok true)))

;; Private functions
(define-private (calculate-voting-power (amount uint) (lock-period uint))
  ;; Voting power increases with both amount and lock period
  ;; Formula: amount * (1 + lock_period / max_lock_period)
  (let (
    (max-lock-period u52560)  ;; 365 days in blocks
    (lock-bonus (/ (* amount lock-period) max-lock-period))
  )
    (+ amount lock-bonus)))

(define-private (is-contract-caller)
  ;; Check if caller is another contract
  ;; In a real implementation, we would check against a list of authorized contracts
  true)

;; Proposal execution functions
(define-private (execute-parameter-change (proposal-id uint))
  (let ((proposal (unwrap-panic (map-get? proposals proposal-id))))
    ;; In a real implementation, we would parse the parameters and update
    ;; platform parameters accordingly
    (ok true)))

(define-private (execute-feature-addition (proposal-id uint))
  (let ((proposal (unwrap-panic (map-get? proposals proposal-id))))
    ;; In a real implementation, this might trigger off-chain processes
    ;; to deploy new features
    (ok true)))

(define-private (execute-contract-upgrade (proposal-id uint))
  (let ((proposal (unwrap-panic (map-get? proposals proposal-id))))
    ;; In a real implementation, this would handle contract upgrades
    ;; potentially using contract versioning patterns
    (ok true)))

(define-private (execute-fund-allocation (proposal-id uint))
  (let ((proposal (unwrap-panic (map-get? proposals proposal-id))))
    ;; In a real implementation, this would transfer funds from treasury
    ;; to proposed recipients
    (ok true)))

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter }))

(define-read-only (get-staking-position (user principal))
  (map-get? staking-positions user))

(define-read-only (get-developer-reputation (developer principal))
  (map-get? developer-reputation developer))

(define-read-only (get-platform-parameters)
  {
    fee-percentage: (var-get platform-fee-percentage),
    min-proposal-stake: (var-get min-proposal-stake),
    proposal-voting-period: (var-get proposal-voting-period),
    treasury-balance: (var-get treasury-balance)
  })

(define-read-only (get-active-proposals)
  ;; In a real implementation, we would return a list of active proposals
  ;; For simplicity, we're returning a placeholder
  (ok "Active proposals would be listed here"))