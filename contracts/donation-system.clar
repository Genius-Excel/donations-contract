(define-map donations 
  {donor: principal, cause-id: uint} 
  {amount: uint, timestamp: uint})

(define-map causes 
  {cause-id: uint} 
  {name: (string-ascii 64), target: uint, raised: uint, recipient: principal})

(define-map certificates 
  {donor: principal, cause-id: uint} 
  {cert-id: uint})

(define-non-fungible-token donation-certificate uint)
(define-data-var next-cause-id uint u0)
(define-data-var next-certificate-id uint u0)

(define-public (create-cause (name (string-ascii 64)) (target uint) (recipient principal))
  (let ((id (var-get next-cause-id)))
    (begin
      (print {action: "create-cause", id: id, name: name, target: target, recipient: recipient})
      (map-set causes {cause-id: id} {name: name, target: target, raised: u0, recipient: recipient})
      (var-set next-cause-id (+ id u1))
      (ok id)
    )
  )
)

;; (define-public (donate (cause-id uint) (amount uint))
;;     (print {action: "donate", cause-id: cause-id, amount: amount, donor: tx-sender})
;;     (match (map-get? causes {cause-id: cause-id})
;;         some cause
;;             (if (> amount u0)
;;                 (let ((new-raised (+ (get raised cause) amount)))
;;                     (begin
;;                         (map-set donations {donor: tx-sender, cause-id: cause-id} {amount: amount, timestamp: block-height})
;;                         (map-set causes {cause-id: cause-id} {name: (get name cause), target: (get target cause), raised: new-raised, recipient: (get recipient cause)})
;;                         (print {action: "donation-recorded", cause-id: cause-id, new-raised: new-raised})
;;                         (match (mint-certificate tx-sender cause-id)
;;                             ok cert-id (ok "Donation successful")
;;                             err e (err e)
;;                         )
;;                     )
;;                 )
;;                 (err "Donation amount must be greater than zero")
;;             )
;;         none (err "Cause does not exist")
;;     )
;; )

(define-private (mint-certificate (donor principal) (cause-id uint))
  (print {action: "mint-certificate", donor: donor, cause-id: cause-id})
  (match (map-get? certificates {donor: donor, cause-id: cause-id})
    some (err "Certificate already exists")
    none
      (let ((cert-id (var-get next-certificate-id)))
        (begin
          (nft-mint? donation-certificate cert-id donor)
          (map-set certificates {donor: donor, cause-id: cause-id} {cert-id: cert-id})
          (var-set next-certificate-id (+ cert-id u1))
          (print {action: "certificate-minted", cert-id: cert-id, donor: donor})
          (ok cert-id)
        )
      )
  )
)

(define-public (disburse-funds (cause-id uint))
  (print {action: "disburse-funds", cause-id: cause-id})
  (match (map-get? causes {cause-id: cause-id})
    some cause
      (if (>= (get raised cause) (get target cause))
        (let ((transfer-result (stx-transfer? (get raised cause) (get recipient cause) tx-sender)))
          (if (is-ok transfer-result)
            (begin
              (map-delete causes {cause-id: cause-id})
              (print {action: "funds-disbursed", cause-id: cause-id, recipient: (get recipient cause)})
              (ok "Funds disbursed successfully")
            )
            (err "Fund transfer failed")
          )
        )
        (err "Target not reached yet")
      )
    none (err "Cause does not exist")
  )
)
