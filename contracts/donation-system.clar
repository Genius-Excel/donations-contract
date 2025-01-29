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
      c  ;; 'c' will contain the value if 'cause' is (some ...)
      (if (> amount u0)
        (let ((new-raised (+ (get raised c) amount)))
          (begin
            (map-set donations {donor: tx-sender, cause-id: cause-id} {amount: amount, timestamp: block-height})
            (map-set causes {cause-id: cause-id} (merge c {raised: new-raised}))
            (try! (mint-certificate tx-sender cause-id))
            (ok u1)  ;; Changed to return a uint
          )
        )
        (err u400)  ;; Changed to return a uint error code
      )
      ;; None case
      (err u404)  ;; Changed to return a uint error code
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

(define-public (get-causes)
  (let ((cause-entries (map-to-list causes)))
    (ok (map cause-entries (lambda (entry) (get cause-id (get key entry)))))
  )
)

(define-private (filter-donations (cause-id uint))
  (let ((donations-list (map-entries donations)))
    (let ((filtered-donations (list)))
      (foreach donation donations-list
        (let ((entry-cause-id (get cause-id (get key donation))))
          (if (is-eq entry-cause-id cause-id)
            (let ((filtered-donations (cons donation filtered-donations)))
              (var-set filtered-donations filtered-donations))
            (var-set filtered-donations filtered-donations))))
      filtered-donations))
)

(define-public (get-donations (cause-id uint))
  (ok (filter-donations cause-id))
)

(define-public (get-cause (cause-id uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      c  ;; 'c' will contain the value if 'cause' is (some ...)
      (ok c)
      ;; None case
      (err u404)  ;; Changed to return a uint error code
    )
  )
)

(define-public (get-certificate (certificate-id uint))
  (let ((owner (nft-owner? donation-certificate certificate-id)))
    (match owner
      o  ;; 'o' will contain the value if 'owner' is (some ...)
      (ok o)
      ;; None case
      (err u404)  ;; Changed to return a uint error code
    )
  )
)

(define-public (disburse-funds (cause-id uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      c  ;; 'c' will contain the value if 'cause' is (some ...)
      (if (>= (get raised c) (get target c))
        (begin
          (try! (asserts! (is-eq tx-sender (get recipient c)) (err u403)))  ;; Changed to return a uint error code
          (try! (as-contract (stx-transfer? (get raised c) (get recipient c)))))
          (map-delete causes {cause-id: cause-id})
          (ok u1)  ;; Changed to return a uint
        )
        (err u402)  ;; Changed to return a uint error code
      )
      ;; None case
      (err u404)  ;; Changed to return a uint error code
    )
  )
