;; api-registry.clar
;; This contract manages the registration of APIs on the platform

(define-data-var next-api-id uint u0)

(define-map apis 
  uint 
  {
    owner: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    endpoint: (string-ascii 128),
    price-per-call: uint,
    active: bool
  }
)

(define-public (register-api 
    (name (string-ascii 64))
    (description (string-ascii 256))
    (endpoint (string-ascii 128))
    (price-per-call uint))
  (let ((api-id (var-get next-api-id)))
    (map-set apis api-id {
      owner: tx-sender,
      name: name, 
      description: description,
      endpoint: endpoint,
      price-per-call: price-per-call,
      active: true
    })
    (var-set next-api-id (+ api-id u1))
    (ok api-id)))

(define-public (update-api
    (api-id uint)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (endpoint (string-ascii 128))
    (price-per-call uint))
  (let ((api (unwrap! (map-get? apis api-id) (err u404))))
    (asserts! (is-eq tx-sender (get owner api)) (err u403))
    (map-set apis api-id {
      owner: tx-sender,
      name: name, 
      description: description,
      endpoint: endpoint,
      price-per-call: price-per-call,
      active: (get active api)
    })
    (ok true)))

(define-public (toggle-api-status (api-id uint))
  (let ((api (unwrap! (map-get? apis api-id) (err u404))))
    (asserts! (is-eq tx-sender (get owner api)) (err u403))
    (map-set apis api-id (merge api {active: (not (get active api))}))
    (ok true)))

(define-read-only (get-api (api-id uint))
  (map-get? apis api-id))

(define-read-only (get-api-price (api-id uint))
  (match (map-get? apis api-id)
    api (ok (get price-per-call api))
    (err u404)))