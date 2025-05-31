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
(define-constant ERR_GOAL_NOT_MET (err u410))

;; Data structures
(define-map donations 
  (tuple (donor principal) (cause-id uint)) 
  (tuple (amount uint) (timestamp uint))
)

(define-map causes 
  (tuple (cause-id uint)) 
  (tuple (name (string-ascii 64)) (target uint) (raised uint) (recipient principal))
)

(define-map donor-totals
  (principal) ;; donor
  (uint)      ;; total donated
)

(define-non-fungible-token donation-certificate uint)
(define-data-var next-cause-id uint u1)
(define-data-var next-certificate-id uint u1)

;; Events
(define-event cause-created (cause-id uint) (name (string-ascii 64)) (target uint))
(define-event donation-made (donor principal) (cause-id uint) (amount uint))
(define-event funds-withdrawn (recipient principal) (cause-id uint) (amount uint))

;; Read-only functions
(define-read-only (get-cause (cause-id uint))
  (map-get? causes {cause-id: cause-id})
)

(define-read-only (get-donation (donor principal) (cause-id uint))
  (map-get? donations {donor: donor, cause-id: cause-id})
)

(define-read-only (get-donor-total (donor principal))
  (default-to u0 (map-get? donor-totals donor))
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

;; Private helpers
(define-private (check-valid-recipient (recipient principal))
  (if (is-eq recipient tx-sender)
    (ok true)
    ERR_INVALID_RECIPIENT
  )
)

;; Public functions

(define-public (create-cause (name (string-ascii 64)) (target uint) (recipient principal))
  (begin
    (asserts! (is-valid-name name) ERR_INVALID_NAME)
    (asserts! (is-valid-target target) ERR_INVALID_TARGET)
    (try! (check-valid-recipient recipient))

    (let ((cause-id (var-get next-cause-id)))
      (asserts! (is-none (get-cause cause-id)) ERR_CAUSE_EXISTS)

      (map-set causes {cause-id: cause-id}
        { name: name, target: target, raised: u0, recipient: recipient })

      (var-set next-cause-id (+ cause-id u1))
      (emit-event cause-created cause-id name target)
      (ok cause-id)
    )
  )
)

(define-public (donate (cause-id uint))
  (let ((amount (stx-transfer? tx-sender tx-sender tx-sender))) ;; dummy to satisfy balance check
    ;; amount will be taken from actual transfer below
    (begin
      (asserts! (> (stx-get-transfer-amount) u0) ERR_INVALID_AMOUNT)

      (let ((cause (unwrap! (get-cause cause-id) ERR_NOT_FOUND)))
        ;; Transfer the actual STX amount to contract itself
        (try! (stx-transfer? (stx-get-transfer-amount) tx-sender (as-contract tx-sender)))

        (let ((prev-donation (default-to {amount: u0, timestamp: u0} 
                                (map-get? donations {donor: tx-sender, cause-id: cause-id})))
              (new-amount (+ (get amount prev-donation) (stx-get-transfer-amount)))
              (timestamp (to-uint (block-height)))
        )
          ;; Update donation record
          (map-set donations 
            {donor: tx-sender, cause-id: cause-id}
            {amount: new-amount, timestamp: timestamp}
          )

          ;; Update total per donor
          (let ((total (get-donor-total tx-sender)))
            (map-set donor-totals tx-sender (+ total (stx-get-transfer-amount)))
          )

          ;; Update raised funds
          (map-set causes 
            {cause-id: cause-id}
            {
              name: (get name cause),
              target: (get target cause),
              raised: (+ (get raised cause) (stx-get-transfer-amount)),
              recipient: (get recipient cause)
            }
          )

          ;; Mint certificate
          (try! (mint-certificate tx-sender cause-id))

          ;; Emit donation event
          (emit-event donation-made tx-sender cause-id (stx-get-transfer-amount))

          (ok true)
        )
      )
    )
  )
)

(define-public (withdraw-funds (cause-id uint))
  (let ((cause (unwrap! (get-cause cause-id) ERR_NOT_FOUND)))
    (begin
      (asserts! (is-eq (get recipient cause) tx-sender) ERR_UNAUTHORIZED)
      (asserts! (>= (get raised cause) (get target cause)) ERR_GOAL_NOT_MET)

      (try! (stx-transfer? (get raised cause) (as-contract tx-sender) tx-sender))

      ;; Reset raised funds to prevent re-withdrawal
      (map-set causes {cause-id: cause-id}
        {
          name: (get name cause),
          target: (get target cause),
          raised: u0,
          recipient: (get recipient cause)
        })

      (emit-event funds-withdrawn tx-sender cause-id (get raised cause))
      (ok true)
    )
  )
)

(define-private (mint-certificate (donor principal) (cause-id uint))
  (let ((cert-id (var-get next-certificate-id)))
    (begin
      (asserts! (is-some (get-cause cause-id)) ERR_NOT_FOUND)
      (try! (nft-mint? donation-certificate cert-id donor))
      (var-set next-certificate-id (+ cert-id u1))
      (ok cert-id)
    )
  )
)
