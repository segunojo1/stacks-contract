;; subscription-manager.clar
;; Manages subscription-based access to APIs

(define-map subscriptions
  { user: principal, api-id: uint }
  {
    start-time: uint,
    end-time: uint,
    calls-remaining: uint,
    calls-limit: uint
  }
)

(define-map api-owners uint principal)

(define-public (subscribe
    (api-id uint)
    (duration-blocks uint)
    (calls-limit uint))
  (let (
    (api-price (unwrap! (contract-call? .api-registry get-api-price api-id) (err u404)))
    (total-cost (* api-price calls-limit))
    (current-block (get-block-height))
    (end-block (+ current-block duration-blocks))
  )
    (asserts! (> calls-limit u0) (err u400))
    (asserts! (> duration-blocks u0) (err u400))
    
    ;; Get API owner from registry
    (let ((api (unwrap! (contract-call? .api-registry get-api api-id) (err u404))))
      ;; Transfer funds to API owner
      (try! (stx-transfer? total-cost tx-sender (get owner api)))
      
      ;; Create or update subscription
      (map-set subscriptions 
        { user: tx-sender, api-id: api-id }
        {
          start-time: current-block,
          end-time: end-block,
          calls-remaining: calls-limit,
          calls-limit: calls-limit
        }
      )
      (ok true))))

(define-public (record-api-call
    (user principal)
    (api-id uint))
  (let (
    (sub (unwrap! (map-get? subscriptions { user: user, api-id: api-id }) (err u404)))
    (current-block (get-block-height))
  )
    ;; Verify subscription is valid
    (asserts! (<= current-block (get end-time sub)) (err u403))
    (asserts! (> (get calls-remaining sub) u0) (err u429))
    
    ;; Update subscription
    (map-set subscriptions
      { user: user, api-id: api-id }
      (merge sub { calls-remaining: (- (get calls-remaining sub) u1) })
    )
    (ok true)))

(define-read-only (check-subscription
    (user principal)
    (api-id uint))
  (match (map-get? subscriptions { user: user, api-id: api-id })
    sub (let ((current-block (get-block-height)))
          (if (and (<= current-block (get end-time sub))
                  (> (get calls-remaining sub) u0))
              (ok true)
              (err u403)))
    (err u404)))