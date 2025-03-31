;; Land-Registry Smart Contract
;; A blockchain-based land registry system that provides immutable land ownership records
;; and enables secure property transfers with government verification

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPERTY-EXISTS (err u101))
(define-constant ERR-PROPERTY-NOT-FOUND (err u102))
(define-constant ERR-NOT-OWNER (err u103))
(define-constant ERR-TRANSFER-FAILED (err u104))
(define-constant ERR-VERIFICATION-REQUIRED (err u105))
(define-constant ERR-ALREADY-VERIFIED (err u106))
(define-constant ERR-INVALID-PRICE (err u107))
(define-constant ERR-LISTING-NOT-FOUND (err u108))
(define-constant ERR-INSUFFICIENT-FUNDS (err u109))

;; Define the contract owner (government authority)
(define-data-var contract-owner principal tx-sender)

;; Property record structure
(define-map properties
  { property-id: uint }
  {
    owner: principal,
    location: (string-ascii 100),
    area: uint,
    registration-date: uint,
    verified: bool,
    property-details: (string-ascii 200)
  }
)

;; Property transfer history
(define-map property-history
  { property-id: uint, transfer-id: uint }
  {
    from: principal,
    to: principal,
    price: uint,
    timestamp: uint,
    verified-by: (optional principal)
  }
)

;; Track the number of transfers for each property
(define-map property-transfer-count
  { property-id: uint }
  { count: uint }
)

;; Property listings for sale
(define-map property-listings
  { property-id: uint }
  {
    owner: principal,
    price: uint,
    listed-at: uint,
    active: bool
  }
)

;; Counter for property IDs
(define-data-var next-property-id uint u1)

;; List of government verifiers
(define-map verifiers
  { address: principal }
  { active: bool }
)

;; Read-only function to get property details
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

;; Read-only function to get property listing
(define-read-only (get-property-listing (property-id uint))
  (map-get? property-listings { property-id: property-id })
)

;; Read-only function to get property transfer history
(define-read-only (get-transfer-history (property-id uint) (transfer-id uint))
  (map-get? property-history { property-id: property-id, transfer-id: transfer-id })
)

;; Read-only function to get the total number of transfers for a property
(define-read-only (get-transfer-count (property-id uint))
  (default-to { count: u0 } (map-get? property-transfer-count { property-id: property-id }))
)

;; Read-only function to check if an address is a verifier
(define-read-only (is-verifier (address principal))
  (default-to { active: false } (map-get? verifiers { address: address }))
)

;; Function to add a new verifier (only contract owner can add)
(define-public (add-verifier (verifier-address principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers { address: verifier-address } { active: true }))
  )
)

;; Function to remove a verifier
(define-public (remove-verifier (verifier-address principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers { address: verifier-address } { active: false }))
  )
)

;; Function to register a new property
(define-public (register-property 
    (location (string-ascii 100)) 
    (area uint) 
    (property-details (string-ascii 200)))
  (let 
    (
      (property-id (var-get next-property-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-none (map-get? properties { property-id: property-id })) ERR-PROPERTY-EXISTS)
    
    ;; Create the property record
    (map-set properties
      { property-id: property-id }
      {
        owner: tx-sender,
        location: location,
        area: area,
        registration-date: current-time,
        verified: false,
        property-details: property-details
      }
    )
    
    ;; Initialize transfer count
    (map-set property-transfer-count
      { property-id: property-id }
      { count: u0 }
    )
    
    ;; Increment the property ID counter
    (var-set next-property-id (+ property-id u1))
    
    (ok property-id)
  )
)

;; Function for government to verify a property
(define-public (verify-property (property-id uint))
  (let 
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
      (verifier-status (is-verifier tx-sender))
    )
    ;; Check if the caller is an authorized verifier
    (asserts! (get active verifier-status) ERR-NOT-AUTHORIZED)
    ;; Check if the property is not already verified
    (asserts! (not (get verified property)) ERR-ALREADY-VERIFIED)
    
    ;; Update the property record to mark it as verified
    (map-set properties
      { property-id: property-id }
      (merge property { verified: true })
    )
    
    (ok true)
  )
)

;; Function to list a property for sale
(define-public (list-property-for-sale (property-id uint) (price uint))
  (let 
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    ;; Check if the caller is the property owner
    (asserts! (is-eq (get owner property) tx-sender) ERR-NOT-OWNER)
    ;; Check if the property is verified
    (asserts! (get verified property) ERR-VERIFICATION-REQUIRED)
    ;; Check if the price is valid
    (asserts! (> price u0) ERR-INVALID-PRICE)
    
    ;; Create the listing
    (map-set property-listings
      { property-id: property-id }
      {
        owner: tx-sender,
        price: price,
        listed-at: current-time,
        active: true
      }
    )
    
    (ok true)
  )
)

;; Function to cancel a property listing
(define-public (cancel-property-listing (property-id uint))
  (let 
    (
      (listing (unwrap! (map-get? property-listings { property-id: property-id }) ERR-LISTING-NOT-FOUND))
    )
    ;; Check if the caller is the property owner
    (asserts! (is-eq (get owner listing) tx-sender) ERR-NOT-OWNER)
    
    ;; Update the listing to inactive
    (map-set property-listings
      { property-id: property-id }
      (merge listing { active: false })
    )
    
    (ok true)
  )
)

;; Function to buy a property
(define-public (buy-property (property-id uint))
  (let 
    (
      (listing (unwrap! (map-get? property-listings { property-id: property-id }) ERR-LISTING-NOT-FOUND))
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
      (price (get price listing))
      (seller (get owner listing))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (transfer-count (get count (get-transfer-count property-id)))
      (new-transfer-count (+ transfer-count u1))
    )
    ;; Check if the listing is active
    (asserts! (get active listing) ERR-LISTING-NOT-FOUND)
    ;; Check if the property is verified
    (asserts! (get verified property) ERR-VERIFICATION-REQUIRED)
    ;; Check if the seller is still the owner
    (asserts! (is-eq seller (get owner property)) ERR-NOT-OWNER)
    
    ;; Process the payment
    (unwrap! (stx-transfer? price tx-sender seller) ERR-INSUFFICIENT-FUNDS)
    
    ;; Update property ownership
    (map-set properties
      { property-id: property-id }
      (merge property { owner: tx-sender })
    )
    
    ;; Record the transfer in history
    (map-set property-history
      { property-id: property-id, transfer-id: new-transfer-count }
      {
        from: seller,
        to: tx-sender,
        price: price,
        timestamp: current-time,
        verified-by: (some tx-sender)
      }
    )
    
    ;; Update transfer count
    (map-set property-transfer-count
      { property-id: property-id }
      { count: new-transfer-count }
    )
    
    ;; Deactivate the listing
    (map-set property-listings
      { property-id: property-id }
      (merge listing { active: false })
    )
    
    (ok true)
  )
)

;; Function to transfer property ownership (direct transfer without sale)
(define-public (transfer-property (property-id uint) (recipient principal))
  (let 
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (transfer-count (get count (get-transfer-count property-id)))
      (new-transfer-count (+ transfer-count u1))
    )
    ;; Check if the caller is the property owner
    (asserts! (is-eq (get owner property) tx-sender) ERR-NOT-OWNER)
    ;; Check if the property is verified
    (asserts! (get verified property) ERR-VERIFICATION-REQUIRED)
    
    ;; Update property ownership
    (map-set properties
      { property-id: property-id }
      (merge property { owner: recipient })
    )
    
    ;; Record the transfer in history
    (map-set property-history
      { property-id: property-id, transfer-id: new-transfer-count }
      {
        from: tx-sender,
        to: recipient,
        price: u0, ;; Direct transfer, no price
        timestamp: current-time,
        verified-by: none ;; No verification for direct transfers
      }
    )
    
    ;; Update transfer count
    (map-set property-transfer-count
      { property-id: property-id }
      { count: new-transfer-count }
    )
    
    ;; If the property was listed, deactivate the listing
    (match (map-get? property-listings { property-id: property-id })
      listing (map-set property-listings
                { property-id: property-id }
                (merge listing { active: false }))
      true
    )
    
    (ok true)
  )
)

;; Function to update property details (only owner can update)
(define-public (update-property-details 
    (property-id uint) 
    (new-details (string-ascii 200)))
  (let 
    (
      (property (unwrap! (map-get? properties { property-id: property-id }) ERR-PROPERTY-NOT-FOUND))
    )
    ;; Check if the caller is the property owner
    (asserts! (is-eq (get owner property) tx-sender) ERR-NOT-OWNER)
    
    ;; Update property details
    (map-set properties
      { property-id: property-id }
      (merge property { property-details: new-details })
    )
    
    (ok true)
  )
)

;; Helper function to check if the caller is the contract owner
(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

;; Function to transfer contract ownership (only current owner can do this)
(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

