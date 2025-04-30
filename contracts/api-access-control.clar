;; api-access.clar
;; Handles authentication and authorization for API calls

(define-map api-keys
  { user: principal, api-id: uint }
  { key: (buff 32), created-at: uint }
)

(define-map access-grants
  { grantor: principal, grantee: principal, api-id: uint }
  { active: bool, created-at: uint }
)

(define-public (generate-api-key (api-id uint))
  (let (
    (user-ok (unwrap! (contract-call? .subscription-manager check-subscription tx-sender api-id) (err u403)))
    (random-bytes (get-block-info? header-hash (- block-height u1)))
    (current-time block-height)
  )
    (asserts! (is-some random-bytes) (err u500))
    (map-set api-keys
      { user: tx-sender, api-id: api-id }
      { key: (unwrap-panic random-bytes), created-at: current-time }
    )
    (ok true)))

(define-public (revoke-api-key (api-id uint))
  (begin
    (map-delete api-keys { user: tx-sender, api-id: api-id })
    (ok true)))

(define-public (grant-access
    (grantee principal)
    (api-id uint))
  (let ((user-ok (unwrap! (contract-call? .subscription-manager check-subscription tx-sender api-id) (err u403))))
    (map-set access-grants
      { grantor: tx-sender, grantee: grantee, api-id: api-id }
      { active: true, created-at: block-height }
    )
    (ok true)))

(define-public (revoke-access
    (grantee principal)
    (api-id uint))
  (begin
    (map-delete access-grants { grantor: tx-sender, grantee: grantee, api-id: api-id })
    (ok true)))

(define-read-only (verify-access
    (user principal)
    (api-id uint))
  (match (contract-call? .subscription-manager check-subscription user api-id)
    success (ok true)
    error (match (find-access-grant user api-id)
            grant (ok true)
            (err u403))))

(define-private (find-access-grant (user principal) (api-id uint))
  (fold check-grantor (map-keys access-grants) none))

(define-private (check-grantor
    (key { grantor: principal, grantee: principal, api-id: uint })
    (current-result (optional { grantor: principal, grantee: principal, api-id: uint })))
  (if (and (is-eq (get grantee key) user)
           (is-eq (get api-id key) api-id)
           (get active (default-to { active: false, created-at: u0 } 
                       (map-get? access-grants key))))
      (some key)
      current-result))