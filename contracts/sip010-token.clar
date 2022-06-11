
(impl-trait .sip010-ft-trait.sip010-ft-trait)

(define-fungible-token fungi-token)

(define-constant contract-owner tx-sender)

(define-constant err-not-token-owner (err u100))
(define-constant err-not-contract-owner (err u101))



(define-read-only (get-name)
    (ok "Fungi")
)

(define-read-only (get-symbol) 
    (ok "FUN")
)

(define-read-only (get-decimals)
    (ok u6)
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance fungi-token who))
)

(define-read-only (get-total-supply) 
    (ok (ft-get-supply fungi-token))
)

(define-read-only (get-token-uri) 
    (ok none)
)

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34)))) 
    (begin 
        (asserts! (is-eq sender tx-sender) err-not-token-owner)
        (try! (ft-transfer? fungi-token amount sender recipient))
        (match memo to-print (print to-print) 0x)
        (ok true)    
    )
)

(define-public (mint (amount uint) (recipient principal))
    (begin 
        (asserts! (is-eq contract-owner tx-sender) err-not-contract-owner)
        (ft-mint? fungi-token amount recipient)
    )

)