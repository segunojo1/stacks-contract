;; oracle-network.clar
;; Provides validated external data to API consumers

(define-data-var oracle-fee uint u1000000)  ;; 1 STX

(define-map oracle-data-feeds
  { feed-id: (string-ascii 64) }
  {
    owner: principal,
    description: (string-ascii 256),
    update-frequency: uint,  ;; in blocks
    last-updated: uint,
    value: (string-utf8 1024),
    fee: uint
  }
)

(define-map oracle-providers
  principal
  {
    reputation: uint,    ;; 0-100
    stake: uint,
    feeds-count: uint
  }
)

(define-public (register-as-provider (stake uint))
  (let ((min-stake u100000000))  ;; 100 STX
    (asserts! (>= stake min-stake) (err u400))
    
    ;; Transfer stake to contract
    (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
    
    ;; Register provider
    (map-set oracle-providers tx-sender {
      reputation: u50,  ;; Start with neutral reputation
      stake: stake,
      feeds-count: u0
    })
    
    (ok true)))

(define-public (create-data-feed
    (feed-id (string-ascii 64))
    (description (string-ascii 256))
    (update-frequency uint)
    (initial-value (string-utf8 1024))
    (fee uint))
  (let ((provider (unwrap! (map-get? oracle-providers tx-sender) (err u403))))
    ;; Create feed
    (map-set oracle-data-feeds
      { feed-id: feed-id }
      {
        owner: tx-sender,
        description: description,
        update-frequency: update-frequency,
        last-updated: block-height,
        value: initial-value,
        fee: fee
      }
    )
    
    ;; Update provider's feeds count
    (map-set oracle-providers tx-sender
      (merge provider { feeds-count: (+ (get feeds-count provider) u1) })
    )
    
    (ok true)))

(define-public (update-feed-value
    (feed-id (string-ascii 64))
    (new-value (string-utf8 1024)))
  (let ((feed (unwrap! (map-get? oracle-data-feeds { feed-id: feed-id }) (err u404))))
    ;; Only feed owner can update
    (asserts! (is-eq tx-sender (get owner feed)) (err u403))
    
    ;; Update feed
    (map-set oracle-data-feeds
      { feed-id: feed-id }
      (merge feed {
        value: new-value,
        last-updated: block-height
      })
    )
    
    (ok true)))

(define-public (read-feed (feed-id (string-ascii 64)))
  (let (
    (feed (unwrap! (map-get? oracle-data-feeds { feed-id: feed-id }) (err u404)))
    (fee (get fee feed))
  )
    ;; Pay the fee
    (try! (stx-transfer? fee tx-sender (get owner feed)))
    
    ;; Return the value
    (ok (get value feed))))

(define-read-only (get-feed-info (feed-id (string-ascii 64)))
  (map-get? oracle-data-feeds { feed-id: feed-id }))

(define-read-only (get-provider-info (provider principal))
  (map-get? oracle-providers provider))