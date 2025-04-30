;; usage-analytics.clar
;; Records and analyzes API usage patterns

(define-map api-usage-stats
  uint  ;; api-id
  {
    total-calls: uint,
    unique-users: uint,
    first-used: uint,
    last-used: uint
  }
)

(define-map user-usage-stats
  { user: principal, api-id: uint }
  {
    total-calls: uint,
    first-used: uint,
    last-used: uint
  }
)

(define-map user-api-set
  uint  ;; api-id
  (list 100 principal)  ;; List of users who have used this API
)

(define-public (record-usage
    (user principal)
    (api-id uint))
  (let (
    (current-block (get-block-height))
    (api-stats (default-to { total-calls: u0, unique-users: u0, first-used: current-block, last-used: current-block }
                           (map-get? api-usage-stats api-id)))
    (user-stats (default-to { total-calls: u0, first-used: current-block, last-used: current-block }
                            (map-get? user-usage-stats { user: user, api-id: api-id })))
    (users-list (default-to (list) (map-get? user-api-set api-id)))
  )
    ;; Update API stats
    (map-set api-usage-stats api-id
      (merge api-stats {
        total-calls: (+ (get total-calls api-stats) u1),
        last-used: current-block,
        unique-users: (if (is-some (index-of users-list user))
                         (get unique-users api-stats)
                         (+ (get unique-users api-stats) u1))
      })
    )
    
    ;; Update user stats
    (map-set user-usage-stats { user: user, api-id: api-id }
      (merge user-stats {
        total-calls: (+ (get total-calls user-stats) u1),
        last-used: current-block
      })
    )
    
    ;; Add user to set if not already present
    (if (is-none (index-of users-list user))
        (map-set user-api-set api-id (unwrap-panic (as-max-len? (append users-list user) u100)))
        true)
    
    (ok true)))

(define-read-only (get-api-stats (api-id uint))
  (map-get? api-usage-stats api-id))

(define-read-only (get-user-stats (user principal) (api-id uint))
  (map-get? user-usage-stats { user: user, api-id: api-id }))

(define-read-only (get-top-apis (limit uint))
  (let ((api-count (var-get .api-registry next-api-id)))
    (filter top-apis-by-usage (range u0 (min limit api-count)))))

(define-private (top-apis-by-usage (api-id uint))
  (match (map-get? api-usage-stats api-id)
    stats (> (get total-calls stats) u0)
    false))