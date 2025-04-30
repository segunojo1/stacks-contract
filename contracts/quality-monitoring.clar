;; quality-monitoring.clar
;; Monitors and reports on API service quality

(define-map quality-reports
  uint  ;; api-id
  {
    uptime-score: uint,       ;; 0-100
    latency-score: uint,      ;; 0-100
    reliability-score: uint,  ;; 0-100
    last-updated: uint
  }
)

(define-map reporter-stakes
  principal
  uint
)

(define-constant min-stake-amount u100000000)  ;; 100 STX
(define-constant max-reporters-per-api u10)
(define-constant blocks-for-challenge u144)    ;; 1 day to challenge

(define-map api-reporters
  uint  ;; api-id
  (list 10 {
    reporter: principal,
    stake: uint,
    since-block: uint
  })
)

(define-public (stake-as-reporter (api-id uint) (amount uint))
  (begin
    (asserts! (>= amount min-stake-amount) (err u400))
    (asserts! (can-add-reporter api-id) (err u409))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (let ((current-stake (default-to u0 (map-get? reporter-stakes tx-sender))))
      (map-set reporter-stakes tx-sender (+ current-stake amount))
      
      (let ((reporters (default-to (list) (map-get? api-reporters api-id))))
        (map-set api-reporters api-id 
          (unwrap-panic (as-max-len? 
            (append reporters {
              reporter: tx-sender,
              stake: amount,
              since-block: block-height
            }) 
            u10)))
      ))
    (ok true)))

(define-public (submit-quality-report
    (api-id uint)
    (uptime-score uint)
    (latency-score uint)
    (reliability-score uint))
  (begin
    (asserts! (<= uptime-score u100) (err u400))
    (asserts! (<= latency-score u100) (err u400))
    (asserts! (<= reliability-score u100) (err u400))
    (asserts! (is-reporter tx-sender api-id) (err u403))
    
    (map-set quality-reports api-id {
      uptime-score: uptime-score,
      latency-score: latency-score,
      reliability-score: reliability-score,
      last-updated: block-height
    })
    (ok true)))

(define-private (is-reporter (reporter principal) (api-id uint))
  (is-some (find-reporter reporter api-id)))

(define-private (find-reporter (reporter principal) (api-id uint))
  (match (map-get? api-reporters api-id)
    reporters (find-in-reporters reporter reporters)
    none))

(define-private (find-in-reporters (reporter principal) (reporters (list 10 {reporter: principal, stake: uint, since-block: uint})))
  (find filter-by-reporter reporters))

(define-private (filter-by-reporter (entry {reporter: principal, stake: uint, since-block: uint}))
  (is-eq (get reporter entry) reporter))

(define-private (can-add-reporter (api-id uint))
  (match (map-get? api-reporters api-id)
    reporters (< (len reporters) max-reporters-per-api)
    true))

(define-read-only (get-quality-report (api-id uint))
  (map-get? quality-reports api-id))