;; payment-gateway.clar
;; Handles payments for API usage, including micropayments

(define-map payment-channels
  { user: principal, api-id: uint }
  {
    balance: uint,
    last-update: uint,
    total-spent: uint
  }
)

(define-map api-balances
  uint  ;; api-id
  uint  ;; balance
)

(define-public (fund-channel (api-id uint) (amount uint))
  (let ((api (unwrap! (contract-call? .api-registry get-api api-id) (err u404))))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (let ((channel (default-to { balance: u0, last-update: block-height, total-spent: u0 }
                               (map-get? payment-channels { user: tx-sender, api-id: api-id }))))
      (map-set payment-channels
        { user: tx-sender, api-id: api-id }
        (merge channel { balance: (+ (get balance channel) amount) })
      ))
    (ok true)))

(define-public (process-payment
    (user principal)
    (api-id uint)
    (amount uint))
  (let (
    (api (unwrap! (contract-call? .api-registry get-api api-id) (err u404)))
    (channel (unwrap! (map-get? payment-channels { user: user, api-id: api-id }) (err u404)))
  )
    ;; Check if user has sufficient balance
    (asserts! (>= (get balance channel) amount) (err u402))
    
    ;; Only API owner can call this function
    (asserts! (is-eq tx-sender (get owner api)) (err u403))
    
    ;; Update user's payment channel
    (map-set payment-channels
      { user: user, api-id: api-id }
      (merge channel {
        balance: (- (get balance channel) amount),
        last-update: block-height,
        total-spent: (+ (get total-spent channel) amount)
      })
    )
    
    ;; Update API owner's balance
    (let ((api-balance (default-to u0 (map-get? api-balances api-id))))
      (map-set api-balances api-id (+ api-balance amount)))
    
    (ok true)))

(define-public (withdraw-earnings (api-id uint))
  (let (
    (api (unwrap! (contract-call? .api-registry get-api api-id) (err u404)))
    (balance (default-to u0 (map-get? api-balances api-id)))
  )
    ;; Only API owner can withdraw
    (asserts! (is-eq tx-sender (get owner api)) (err u403))
    (asserts! (> balance u0) (err u400))
    
    ;; Transfer funds to API owner
    (try! (as-contract (stx-transfer? balance tx-sender (get owner api))))
    
    ;; Reset balance
    (map-set api-balances api-id u0)
    
    (ok balance)))

(define-public (withdraw-channel-funds (api-id uint))
  (let (
    (channel (unwrap! (map-get? payment-channels { user: tx-sender, api-id: api-id }) (err u404)))
    (balance (get balance channel))
  )
    (asserts! (> balance u0) (err u400))
    
    ;; Transfer funds back to user
    (try! (as-contract (stx-transfer? balance tx-sender tx-sender)))
    
    ;; Update channel
    (map-set payment-channels
      { user: tx-sender, api-id: api-id }
      (merge channel { balance: u0 })
    )
    
    (ok balance)))

(define-read-only (get-channel-balance (user principal) (api-id uint))
  (match (map-get? payment-channels { user: user, api-id: api-id })
    channel (some (get balance channel))
    none))