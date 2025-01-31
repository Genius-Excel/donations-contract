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


(define-public (donate (cause-id uint) (amount uint))
  (let 
    (
      (cause (get-cause cause-id))
    )
    (match cause cause-data
      (begin
        ;; Input validation
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (check-duplicate-donation tx-sender cause-id))
        
        ;; Check STX balance
        (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)
        
        (let 
          (
            (new-raised (+ (get raised cause-data) amount))
          )
          (begin
            ;; Verify we're not exceeding target
            (asserts! (<= new-raised (get target cause-data)) ERR_INVALID_AMOUNT)
            
            ;; Process donation
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            
            (map-set donations 
              {donor: tx-sender, cause-id: cause-id} 
              {amount: amount, timestamp: stacks-block-height}
            )
            (map-set causes 
              {cause-id: cause-id} 
              (merge cause-data {raised: new-raised})
            )
            (try! (mint-certificate tx-sender cause-id))
            (ok true)
          )
        )
      )
      ERR_NOT_FOUND
    )
  )
)

(define-public (get-cause-donations (cause-id uint))
  (match (get-cause cause-id)
    cause-data (ok cause-data)
    ERR_NOT_FOUND
  )
)

(define-public (disburse-funds (cause-id uint))
  (let 
    (
      (cause (get-cause cause-id))
    )
    (match cause cause-data
      (begin
        ;; Authorization checks
        (asserts! (is-eq tx-sender (get recipient cause-data)) ERR_UNAUTHORIZED)
        
        ;; Validate funds
        (asserts! (>= (get raised cause-data) (get target cause-data)) ERR_INSUFFICIENT_FUNDS)
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) (get raised cause-data)) ERR_INSUFFICIENT_FUNDS)
        
        ;; Process disbursement
        (try! (stx-transfer? (get raised cause-data) (as-contract tx-sender) (get recipient cause-data)))
        (map-delete causes {cause-id: cause-id})
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

;; Additional error codes
(define-constant ERR_TARGET_TOO_LOW (err u410))
(define-constant ERR_ALREADY_COMPLETED (err u411))

;; Additional read-only functions
(define-read-only (is-target-reached (cause-id uint))
  (match (get-cause cause-id)
    cause-data (ok (>= (get raised cause-data) (get target cause-data)))
    ERR_NOT_FOUND
  )
)


(define-private (get-donation-amount (cause-id uint))
  (match (get-donation tx-sender cause-id)
    donation (get amount donation)
    u0
  )
)


;; New public function to update target amount
(define-public (update-target (cause-id uint) (new-target uint))
  (let 
    (
      (cause (get-cause cause-id))
    )
    (match cause cause-data
      (begin
        ;; Authorization check
        (asserts! (is-eq tx-sender (get recipient cause-data)) ERR_UNAUTHORIZED)
        
        ;; Validate new target
        (asserts! (is-valid-target new-target) ERR_INVALID_TARGET)
        (asserts! (> new-target (get target cause-data)) ERR_TARGET_TOO_LOW)
        
        ;; Check cause hasn't already reached its target
        (asserts! (not (unwrap-panic (is-target-reached cause-id))) ERR_ALREADY_COMPLETED)
        
        ;; Update the target
        (map-set causes 
          {cause-id: cause-id} 
          (merge cause-data {target: new-target})
        )
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)
