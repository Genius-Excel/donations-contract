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
      c
      (if (> amount u0)
        (let ((new-raised (+ (get raised c) amount)))
          (begin
            (map-set donations {donor: tx-sender, cause-id: cause-id} {amount: amount, timestamp: block-height})
            (map-set causes {cause-id: cause-id} (merge c {raised: new-raised}))
            (try! (mint-certificate tx-sender cause-id))
            (ok u1)
          )
        )
        (err u400)
      )
      (err u404)
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

(define-private (map-to-list (map (map uint uint)))
  (let ((entries (list)))
    (map-to-list-helper (map-keys-helper map) entries)))

(define-private (map-to-list-helper (keys (list uint)) (entries (list (tuple (key uint) (value uint)))))
  (if (is-none keys)
    entries
    (let ((key (unwrap-panic (element-at keys u0))))
      (let ((value (map-get? map {key: key})))
        (match value
          entry (map-to-list-helper (cdr keys) (cons {key: key, value: entry} entries))
          none (map-to-list-helper (cdr keys) entries))))))

(define-private (map-keys-helper (map (map uint uint)))
  (let ((keys (list)))
    (map-keys-helper-rec (map-entries-helper map) keys)))

(define-private (map-keys-helper-rec (entries (list (tuple (key uint) (value uint)))) (keys (list uint)))
  (if (is-none entries)
    keys
    (let ((entry (unwrap-panic (element-at entries u0))))
      (map-keys-helper-rec (cdr entries) (cons (get key entry) keys)))))

(define-private (map-entries-helper (map (map uint uint)))
  (let ((entries (list)))
    (map-entries-helper-rec (map-keys-helper map) entries)))

(define-private (map-entries-helper-rec (keys (list uint)) (entries (list (tuple (key uint) (value uint)))))
  (if (is-none keys)
    entries
    (let ((key (unwrap-panic (element-at keys u0))))
      (let ((value (map-get? map {key: key})))
        (match value
          entry (map-entries-helper-rec (cdr keys) (cons {key: key, value: entry} entries))
          none (map-entries-helper-rec (cdr keys) entries))))))

(define-private (filter-donations (cause-id uint))
  (let ((donations-list (map-to-list donations)))
    (let ((filtered-donations (list)))
      (filter-donations-helper donations-list filtered-donations cause-id))))

(define-private (filter-donations-helper (donations-list (list (tuple (key uint) (value uint)))) (filtered-donations (list (tuple (key uint) (value uint)))) (cause-id uint))
  (if (is-none donations-list)
    filtered-donations
    (let ((donation (unwrap-panic (element-at donations-list u0))))
      (let ((entry-cause-id (get cause-id (get key donation))))
        (if (is-eq entry-cause-id cause-id)
          (filter-donations-helper (cdr donations-list) (cons donation filtered-donations) cause-id)
          (filter-donations-helper (cdr donations-list) filtered-donations cause-id))))))

(define-public (get-donations (cause-id uint))
  (ok (filter-donations cause-id))
)

(define-public (get-causes)
  (let ((cause-entries (map-to-list causes)))
    (ok (map cause-entries (lambda (entry) (get cause-id (get key entry)))))
  )
)

(define-public (disburse-funds (cause-id uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      c
      (if (>= (get raised c) (get target c))
        (begin
          (try! (asserts! (is-eq tx-sender (get recipient c)) (err u403)))
          (try! (as-contract (stx-transfer? (get raised c) (get recipient c))))
          (map-delete causes {cause-id: cause-id})
          (ok u1)
        )
        (err u402)
      )
      (err u404)
    )
  )
)