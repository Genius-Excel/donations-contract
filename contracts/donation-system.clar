;; Error codes
(define-constant ERR_UNAUTHORIZED (err u403))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_AMOUNT (err u400))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_INVALID_NAME (err u405))
(define-constant ERR_INVALID_TARGET (err u406))
(define-constant ERR_CAUSE_EXISTS (err u407))
(define-constant ERR_DONATION_EXISTS (err u408))
(define-constant ERR_INVALID_RECIPIENT (err u409))

;; Data structures
(define-map donations 
  (tuple (donor principal) (cause-id uint)) 
  (tuple (amount uint) (timestamp uint))
)

(define-map causes 
  (tuple (cause-id uint)) 
  (tuple (name (string-ascii 64)) (target uint) (raised uint) (recipient principal))
)

(define-non-fungible-token donation-certificate uint)
(define-data-var next-cause-id uint u1)
(define-data-var next-certificate-id uint u1)

;; Read-only functions
(define-read-only (get-cause (cause-id uint))
  (map-get? causes {cause-id: cause-id})
)

(define-read-only (get-donation (donor principal) (cause-id uint))
  (map-get? donations {donor: donor, cause-id: cause-id})
)

(define-read-only (is-valid-name (name (string-ascii 64)))
  (and 
    (> (len name) u0)
    (<= (len name) u64)
  )
)

(define-read-only (is-valid-target (target uint))
  (> target u0)
)

;; Private helper functions
(define-private (check-duplicate-donation (donor principal) (cause-id uint))
  (match (get-donation donor cause-id)
    donation ERR_DONATION_EXISTS
    (ok true)
  )
)

(define-private (check-valid-recipient (recipient principal))
  (if (is-eq recipient tx-sender)
    (ok true)
    ERR_INVALID_RECIPIENT
  )
)

;; Public functions
(define-public (create-cause (name (string-ascii 64)) (target uint) (recipient principal))
  (begin
    ;; Input validation
    (asserts! (is-valid-name name) ERR_INVALID_NAME)
    (asserts! (is-valid-target target) ERR_INVALID_TARGET)
    (try! (check-valid-recipient recipient))
    
    (let 
      (
        (cause-id (var-get next-cause-id))
      )
      ;; Check if cause already exists
      (asserts! (is-none (get-cause cause-id)) ERR_CAUSE_EXISTS)
      
      (begin
        (map-set causes 
          {cause-id: cause-id} 
          {
            name: name, 
            target: target, 
            raised: u0, 
            recipient: recipient
          }
        )
        (var-set next-cause-id (+ cause-id u1))
        (ok cause-id)
      )
    )
  )
)

(define-private (mint-certificate (donor principal) (cause-id uint))
  (let 
    (
      (cert-id (var-get next-certificate-id))
    )
    (begin
      ;; Verify cause exists before minting
      (asserts! (is-some (get-cause cause-id)) ERR_NOT_FOUND)
      (try! (nft-mint? donation-certificate cert-id donor))
      (var-set next-certificate-id (+ cert-id u1))
      (ok cert-id)
    )
  )
)
