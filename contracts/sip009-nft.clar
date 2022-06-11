
;; sip009-nft
;; <add a description here>

(impl-trait .sip009-nft-trait.sip009-nft-trait)

(define-constant contract-owner tx-sender)

(define-constant err-not-token-owner (err u100))
(define-constant err-not-contract-owner (err u101))

(define-data-var last-token-id uint u0)

(define-non-fungible-token larp-token uint)


(define-read-only (get-last-token-id) 
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? larp-token token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal)) 
    (begin 
        (asserts! (is-eq sender tx-sender) err-not-token-owner)
        (nft-transfer? larp-token token-id sender recipient)
    )
)

(define-public (mint (recipient principal)) 
    (let ((token-id (+ (var-get last-token-id) u1))) 
        (asserts! (is-eq tx-sender contract-owner) err-not-contract-owner)
        (try! (nft-mint? larp-token token-id recipient))
        (var-set last-token-id token-id)
        (ok token-id)        
    )
)