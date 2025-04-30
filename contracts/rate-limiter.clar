;; rate-limiter.clar
;; Implements rate limiting for API calls

(define-map rate-limits
  uint  ;; api-id
  {
    calls-per-block: uint,
    calls-per-day: uint
  }
)

(define-map usage-counters
  { user: principal, api-id: uint }
  {
    last-block: uint,
    block-count: uint,
    day-start: uint,
    day-count: uint
  }
)

(define-constant blocks-per-day u144)  ;; Assuming 10-minute blocks, 144 blocks per day

(define-public (set-rate-limits
    (api-id uint)
    (calls-per-block uint)
    (calls-per-day uint))
  (let ((api (unwrap! (contract-call? .api-registry get-api api-id) (err u404))))
    (asserts! (is-eq tx-sender (get owner api)) (err u403))
    (map-set rate-limits api-id {
      calls-per-block: calls-per-block,
      calls-per-day: calls-per-day
    })
    (ok true)))

(define-public (check-rate-limit
    (user principal)
    (api-id uint))
  (let (
    (current-block (get-block-height))
    (limits (default-to { calls-per-block: u100, calls-per-day: u1000 } 
                        (map-get? rate-limits api-id)))
    (counters (default-to { last-block: u0, block-count: u0, day-start: u0, day-count: u0 }
                          (map-get? usage-counters { user: user, api-id: api-id })))
  )
    ;; Check if we're in a new block
    (if (is-eq (get last-block counters) current-block)
        ;; Same block, increment counter
        (let ((new-block-count (+ (get block-count counters) u1)))
          (asserts! (<= new-block-count (get calls-per-block limits)) (err u429))
          (map-set usage-counters 
            { user: user, api-id: api-id }
            (merge counters { block-count: new-block-count })
          ))
        ;; New block, reset block counter
        (map-set usage-counters 
          { user: user, api-id: api-id }
          (merge counters { last-block: current-block, block-count: u1 })
        ))
    
    ;; Check if we're in a new day
    (if (> current-block (+ (get day-start counters) blocks-per-day))
        ;; New day, reset day counter
        (map-set usage-counters 
          { user: user, api-id: api-id }
          (merge (get-counters user api-id) { day-start: current-block, day-count: u1 })
        )
        ;; Same day, increment counter
        (let ((new-day-count (+ (get day-count (get-counters user api-id)) u1)))
          (asserts! (<= new-day-count (get calls-per-day limits)) (err u429))
          (map-set usage-counters 
            { user: user, api-id: api-id }
            (merge (get-counters user api-id) { day-count: new-day-count })
          )))
    
    (ok true)))

(define-private (get-counters (user principal) (api-id uint))
  (default-to { last-block: u0, block-count: u0, day-start: u0, day-count: u0 }
             (map-get? usage-counters { user: user, api-id: api-id })))