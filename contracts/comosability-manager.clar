;; composability-manager.clar
;; Allows APIs to be composed together for more complex workflows

(define-map workflows
  uint  ;; workflow-id
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    owner: principal,
    steps: (list 20 {
      api-id: uint,
      input-mapping: (string-utf8 1024),  ;; JSON string describing input mapping
      required: bool
    }),
    price: uint
  }
)

(define-data-var next-workflow-id uint u0)

(define-public (create-workflow
    (name (string-ascii 64))
    (description (string-ascii 256))
    (steps (list 20 {
        api-id: uint,
        input-mapping: (string-utf8 1024),
        required: bool
      }))
    (price uint))
  (let ((workflow-id (var-get next-workflow-id)))
    ;; Verify all APIs exist
    (asserts! (verify-apis steps) (err u404))
    
    ;; Create workflow
    (map-set workflows workflow-id {
      name: name,
      description: description,
      owner: tx-sender,
      steps: steps,
      price: price
    })
    
    ;; Increment workflow ID
    (var-set next-workflow-id (+ workflow-id u1))
    
    (ok workflow-id)))

(define-public (update-workflow
    (workflow-id uint)
    (name (string-ascii 64))
    (description (string-ascii 256))
    (steps (list 20 {
        api-id: uint,
        input-mapping: (string-utf8 1024),
        required: bool
      }))
    (price uint))
  (let ((workflow (unwrap! (map-get? workflows workflow-id) (err u404))))
    ;; Only owner can update
    (asserts! (is-eq tx-sender (get owner workflow)) (err u403))
    
    ;; Verify all APIs exist
    (asserts! (verify-apis steps) (err u404))
    
    ;; Update workflow
    (map-set workflows workflow-id {
      name: name,
      description: description,
      owner: tx-sender,
      steps: steps,
      price: price
    })
    
    (ok true)))

(define-public (execute-workflow
    (workflow-id uint)
    (input-data (string-utf8 4096)))  ;; JSON input data
  (let (
    (workflow (unwrap! (map-get? workflows workflow-id) (err u404)))
    (price (get price workflow))
  )
    ;; Pay for workflow execution
    (try! (stx-transfer? price tx-sender (get owner workflow)))
    
    ;; In a real implementation, this would trigger an off-chain execution
    ;; For now, we just record that the workflow was called
    
    ;; For each API in the workflow, we'd need to handle payments and access
    ;; This is simplified for demonstration purposes
    (map handle-step-payment (get steps workflow))
    
    (ok true)))

(define-private (handle-step-payment (step {api-id: uint, input-mapping: (string-utf8 1024), required: bool}))
  (let (
    (api (unwrap-panic (contract-call? .api-registry get-api (get api-id step))))
    (price (unwrap-panic (contract-call? .api-registry get-api-price (get api-id step))))
  )
    ;; In a real implementation, we would handle access checks and payments
    ;; This is simplified for demonstration
    true))

(define-private (verify-apis (steps (list 20 {api-id: uint, input-mapping: (string-utf8 1024), required: bool})))
  (fold check-api-exists steps true))

(define-private (check-api-exists 
    (step {api-id: uint, input-mapping: (string-utf8 1024), required: bool})
    (previous-result bool))
  (if previous-result
      (is-some (contract-call? .api-registry get-api (get api-id step)))
      false))

(define-read-only (get-workflow (workflow-id uint))
  (map-get? workflows workflow-id))