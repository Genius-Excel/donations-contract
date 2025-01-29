;; Define the maps
(define-map donations 
  {donor: principal, cause-id: uint} 
  {amount: uint, timestamp: uint})

(define-map causes 
  {cause-id: uint} 
  {name: (string-ascii 64), target: uint, raised: uint, recipient: principal})

;; Define NFT and variables
(define-non-fungible-token donation-certificate uint)
(define-data-var next-cause-id uint u1)
(define-data-var next-certificate-id uint u1)

(define-public (create-cause (name (string-ascii 64)) (target uint) (recipient principal))
  (let ((id (var-get next-cause-id)))
    (begin
      (map-set causes {cause-id: id} {name: name, target: target, raised: u0, recipient: recipient})
      (var-set next-cause-id (+ id u1))
      (ok id))))

(define-public (donate (cause-id uint) (amount uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      cause-data  ;; Changed 'some' to a binding variable name
        (if (> amount u0)
          (let ((new-raised (+ (get raised cause-data) amount)))  ;; Fixed to use bound variable
            (begin
              ;; Add STX transfer before updating state
              (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
              (map-set donations {donor: tx-sender, cause-id: cause-id} 
                                {amount: amount, timestamp: block-height})
              (map-set causes {cause-id: cause-id} 
                            (merge cause-data {raised: new-raised}))  ;; Fixed to use bound variable
              (try! (mint-certificate tx-sender cause-id))  ;; Removed unwrap! since cause-id is already uint
              (ok "Donation successful")))
          (err u400))  ;; Added error code
      (err u404))))  ;; Added error code

    
(define-private (mint-certificate (donor principal) (cause-id uint))
  (let ((cert-id (var-get next-certificate-id)))
    (begin
      (try! (nft-mint? donation-certificate cert-id donor))  ;; Added try! to handle errors
      (var-set next-certificate-id (+ cert-id u1))
      (ok cert-id))))

(define-public (disburse-funds (cause-id uint))
  (let ((cause (map-get? causes {cause-id: cause-id})))
    (match cause
      cause-data  ;; Changed 'some' to a binding variable name
        (if (>= (get raised cause-data) (get target cause-data))  ;; Fixed to use bound variable
          (begin
            (try! (stx-transfer? (get raised cause-data) 
                                (get recipient cause-data) 
                                (as-contract tx-sender)))  ;; Fixed transfer to use contract as sender
            (map-delete causes {cause-id: (unwrap! cause-id (err u400))})
            (ok "Funds disbursed successfully"))
          (err u403))  ;; Added error code
      (err u404))))  ;; Added error code