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

(define-public (create-cause (name (string-ascii 64)) (target uint) (recipient principal))
  (let ((id (var-get next-cause-id)))
    (begin
      (map-set causes {cause-id: id} {name: name, target: target, raised: u0, recipient: recipient})
      (var-set next-cause-id (+ id u1))
      (ok id)
    )
  )
)

(define-public (donate (cause-id uint) (amount uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      (some c
        (if (> amount u0)
          (let ((new-raised (+ (get raised c) amount)))
            (begin
              (map-set donations {donor: tx-sender, cause-id: cause-id} {amount: amount, timestamp: block-height})
              (map-set causes {cause-id: cause-id} (merge c {raised: new-raised}))
              (try! (mint-certificate tx-sender cause-id))
              (ok "Donation successful")
            )
          )
          (err "Donation amount must be greater than zero")
        )
      )
      (none (err "Cause does not exist"))
    )
  )
)

(define-private (mint-certificate (donor principal) (cause-id uint))
  (let ((cert-id (var-get next-certificate-id)))
    (begin
      (try! (nft-mint? donation-certificate cert-id donor))
      (var-set next-certificate-id (+ cert-id u1))
      (ok cert-id)
    )
  )
)

(define-public (disburse-funds (cause-id uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      (some c
        (if (>= (get raised c) (get target c))
          (begin
            (asserts! (is-eq tx-sender (get recipient c)) (err "Unauthorized"))
            (as-contract (stx-transfer? (get raised c) tx-sender (get recipient c)))
            (map-delete causes {cause-id: cause-id})
            (ok "Funds disbursed successfully")
          )
          (err "Target not reached yet")
        )
      )
      (none (err "Cause does not exist"))
    )
  )
)