(use-trait nft-trait .sip009-nft-trait.sip009-nft-trait)
(use-trait ft-trait .sip010-ft-trait.sip010-ft-trait)


;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-expiry-in-past (err u1000))
(define-constant err-price-zero (err u1001))

(define-constant err-unknown-listing (err u2000))
(define-constant err-unauthorised (err u2001))
(define-constant err-listing-expired (err u2002))
(define-constant err-nft-asset-mismatch (err u2003))
(define-constant err-payment-asset-mismatch (err u2004))
(define-constant err-maker-taker-equal (err u2005))
(define-constant err-unintended-taker (err u2006))
(define-constant err-asset-contract-not-whitelisted (err u2007))
(define-constant err-payment-contract-not-whitelisted (err u2008))

;;---

;; Data (Maps) 

;; listing id looks up tuple of listing data
(define-map listings 
    uint 
    { 
        maker: principal, 
        taker: (optional principal),
        token-id: uint,
        expiry: uint,
        price: uint,
        payment-asset-contract: (optional principal),
        nft-asset-contract: principal,
    }
)

;; asset contract looks up is-white-listed boolean
(define-map whitelisted-asset-contracts principal bool)

;;---

;; Data (Vars)

;; auto incrementing listing id
(define-data-var listing-nonce uint u0)

;;---


;; Read-only
(define-read-only (is-whitelisted (asset-contract principal))
    (default-to false (map-get? whitelisted-asset-contracts asset-contract))
)

(define-read-only (get-listing (listing-id uint))
	(map-get? listings listing-id)
)
;;---

;; Private
(define-private (transfer-nft (token-contract <nft-trait>) (token-id uint) (sender principal) (recipient principal))
    (contract-call? token-contract transfer token-id sender recipient)
)

(define-private (transfer-ft (token-contract <ft-trait>) (amount uint) (sender principal) (recipient principal)) 
    (contract-call? token-contract transfer amount sender recipient none)
)

(define-private (assert-can-fulfil (nft-asset-contract principal) (payment-asset-contract (optional principal)) 
        (listing {maker: principal, taker: (optional principal), token-id: uint, nft-asset-contract: principal, 
        expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(begin
        ;; assert buyer is not the seller
		(asserts! (not (is-eq (get maker listing) tx-sender)) err-maker-taker-equal)

        ;; assert if there is an intended buyer and if there is, make sure that the buyer is that principal
		(asserts! (match (get taker listing) intended-taker 
                    (is-eq intended-taker tx-sender) 
                    true
                  ) err-unintended-taker)

        ;; assert that listing has not expired
		(asserts! (< block-height (get expiry listing)) err-listing-expired)

        ;; assert the nft asset contract matches the listing
		(asserts! (is-eq (get nft-asset-contract listing) nft-asset-contract) err-nft-asset-mismatch)

        ;; assert the payment asset contract matches the listing
		(asserts! (is-eq (get payment-asset-contract listing) payment-asset-contract) err-payment-asset-mismatch)
		(ok true)
	)
)

;;---

;; Public

;; the maker calls this function
;; listing function takes in the nft-asset contract and the listing data
;; invokes the nft-asset contract to send the nft to the tiny-market contract
(define-public (list-asset (nft-asset-contract <nft-trait>) 
    (nft-asset {taker: (optional principal), token-id: uint, expiry: uint, price: uint, payment-asset-contract: (optional principal)}))
	(let ((listing-id (var-get listing-nonce)))
        ;; assert the nft-trait asset is in the whitelist
		(asserts! (is-whitelisted (contract-of nft-asset-contract)) err-asset-contract-not-whitelisted)

        ;; assert that the listing expiry date is in the future
		(asserts! (> (get expiry nft-asset) block-height) err-expiry-in-past)

        ;; assert that the nft price is greater than zero
		(asserts! (> (get price nft-asset) u0) err-price-zero)

        ;; assert that the payment token is a whitelisted asset, or that it is STX
		(asserts! (match (get payment-asset-contract nft-asset) payment-asset 
                    (is-whitelisted payment-asset) 
                    true
                  ) err-payment-contract-not-whitelisted)
		
        ;; invoke the nft asset contract to send the nft asset to the tiny-market contract
        (try! (transfer-nft nft-asset-contract (get token-id nft-asset) tx-sender (as-contract tx-sender)))
		
        ;; update to the listings data with the maker and the nft-asset contract
        (map-set listings listing-id (merge {maker: tx-sender, nft-asset-contract: (contract-of nft-asset-contract)} nft-asset))

        (print (contract-of nft-asset-contract))

        ;; increment the listing nonce
        (var-set listing-nonce (+ listing-id u1))
		(ok listing-id)
	)
)

;; the maker calls this function
(define-public (cancel-listing (listing-id uint) (nft-asset-contract <nft-trait>))
	(let (
        ;; assign the listing data tuple
		(listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
		
        ;; assign the maker principle
        (maker (get maker listing))
		)

        ;; assert that the caller of the contract is the maker
		(asserts! (is-eq maker tx-sender) err-unauthorised)

        ;; assert that the nft-asset contract is the same one that is passed in
		(asserts! (is-eq (get nft-asset-contract listing) (contract-of nft-asset-contract)) err-nft-asset-mismatch)
		
        ;; remove the listing
        (map-delete listings listing-id)

        ;; transfer the nft back to the owner
		(as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender maker))
	)
)

;; contract owner calls this function
(define-public (set-whitelisted (asset-contract principal) (whitelisted bool))
    (begin 
        ;; assert the caller of this function is the contract-owner
        (asserts! (is-eq tx-sender contract-owner) err-unauthorised)
        (ok (map-insert whitelisted-asset-contracts asset-contract whitelisted))
    )
)



;; The buyer is the caller of this function, paying in stx
(define-public (fulfil-listing-stx (listing-id uint) (nft-asset-contract <nft-trait>)) 
    (let (
        (listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
        (taker tx-sender)
        )

        (try! (assert-can-fulfil (contract-of nft-asset-contract) none listing))
        (try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
        (try! (stx-transfer? (get price listing) taker (get maker listing)))
        (map-delete listings listing-id)
        (ok listing-id)
    ) 
)

;; the buyer calls this function
(define-public (fulfil-listing-ft (listing-id uint) (nft-asset-contract <nft-trait>) (payment-asset-contract <ft-trait>)) 
    
    ;; assert the listing can be fulfilled
    ;; transfer the nft to the buyer
    ;; transfer the payment to the seller
    ;; delete the listing

    (let (
        (listing (unwrap! (map-get? listings listing-id) err-unknown-listing))
        (taker tx-sender)
        )
        
        (try! (assert-can-fulfil (contract-of nft-asset-contract) (some (contract-of payment-asset-contract)) listing))
        (try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender taker)))
        (try! (transfer-ft payment-asset-contract (get price listing) taker (get maker listing)))
        (map-delete listings listing-id)
        (ok listing-id)


    ) 
)



;;---
