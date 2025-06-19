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


(define-constant ERR-AUCTION-NOT-FOUND (err u110))
(define-constant ERR-AUCTION-ENDED (err u111))
(define-constant ERR-AUCTION-NOT-ENDED (err u112))
(define-constant ERR-BID-TOO-LOW (err u113))
(define-constant ERR-RESERVE-NOT-MET (err u114))
(define-constant ERR-AUCTION-ACTIVE (err u115))
(define-constant ERR-NO-BIDS (err u116))
(define-constant ERR-REFUND-FAILED (err u117))

(define-map property-auctions
  { property-id: uint }
  {
    seller: principal,
    start-time: uint,
    end-time: uint,
    reserve-price: uint,
    highest-bid: uint,
    highest-bidder: (optional principal),
    total-bids: uint,
    active: bool,
    settled: bool
  }
)

(define-map auction-bids
  { property-id: uint, bid-id: uint }
  {
    bidder: principal,
    amount: uint,
    timestamp: uint,
    refunded: bool
  }
)

(define-map bidder-amounts
  { property-id: uint, bidder: principal }
  { total-amount: uint }
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
;; Helper function to check if a principal is the owner of a property
(define-private (is-property-owner (property-id uint) (caller principal))
  (match (map-get? properties { property-id: property-id })
    property (is-eq (get owner property) caller)
    false
  )
)

;; Dispute status constants
(define-constant DISPUTE-PENDING u1)
(define-constant DISPUTE-RESOLVED u2)
(define-constant DISPUTE-REJECTED u3)

;; Dispute mapping
(define-map property-disputes
  { property-id: uint, dispute-id: uint }
  {
    complainant: principal,
    description: (string-ascii 200),
    status: uint,
    filed-at: uint,
    resolved-at: (optional uint),
    resolver: (optional principal)
  }
)

(define-data-var next-dispute-id uint u1)

(define-public (file-dispute (property-id uint) (description (string-ascii 200)))
  (let 
    ((dispute-id (var-get next-dispute-id))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (map-set property-disputes
      { property-id: property-id, dispute-id: dispute-id }
      {
        complainant: tx-sender,
        description: description,
        status: DISPUTE-PENDING,
        filed-at: current-time,
        resolved-at: none,
        resolver: none
      }
    )
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)



(define-map property-valuations
  { property-id: uint, valuation-id: uint }
  {
    value: uint,
    appraiser: principal,
    timestamp: uint,
    notes: (string-ascii 100)
  }
)

(define-data-var next-valuation-id uint u1)

(define-public (add-property-valuation (property-id uint) (value uint) (notes (string-ascii 100)))
  (let 
    ((valuation-id (var-get next-valuation-id))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (map-set property-valuations
      { property-id: property-id, valuation-id: valuation-id }
      {
        value: value,
        appraiser: tx-sender,
        timestamp: current-time,
        notes: notes
      }
    )
    (var-set next-valuation-id (+ valuation-id u1))
    (ok valuation-id)
  )
)



(define-map property-liens
  { property-id: uint }
  {
    lender: principal,
    amount: uint,
    start-date: uint,
    end-date: uint,
    active: bool
  }
)

(define-public (register-lien 
    (property-id uint) 
    (amount uint)
    (duration uint))
  (let 
    ((current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
     (end-time (+ current-time duration)))
     (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (ok (map-set property-liens
      { property-id: property-id }
      {
        lender: tx-sender,
        amount: amount,
        start-date: current-time,
        end-date: end-time,
        active: true
      }))
  )
)



(define-map property-rentals
  { property-id: uint }
  {
    tenant: (optional principal),
    monthly-rent: uint,
    lease-start: uint,
    lease-end: uint,
    active: bool
  }
)

(define-public (create-rental-listing 
    (property-id uint) 
    (monthly-rent uint)
    (lease-duration uint))
  (let 
    ((current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-OWNER)
    (ok (map-set property-rentals
      { property-id: property-id }
      {
        tenant: none,
        monthly-rent: monthly-rent,
        lease-start: current-time,
        lease-end: (+ current-time lease-duration),
        active: true
      }))
  )
)


(define-map subdivided-properties
  { parent-id: uint, sub-id: uint }
  {
    owner: principal,
    area: uint,
    verified: bool
  }
)

(define-public (subdivide-property 
    (property-id uint) 
    (sub-areas (list 10 uint)))
  (let 
    ((property (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
     (total-area (get area property)))
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-OWNER)
    (asserts! (get verified property) ERR-VERIFICATION-REQUIRED)
    ;; Additional logic to create subdivided properties
    (ok true)
  )
)


(define-map property-documents
  { property-id: uint, document-id: uint }
  {
    document-hash: (buff 32),
    document-type: (string-ascii 50),
    upload-date: uint,
    uploader: principal
  }
)

(define-data-var next-document-id uint u1)

(define-public (add-property-document 
    (property-id uint) 
    (document-hash (buff 32))
    (document-type (string-ascii 50)))
  (let 
    ((doc-id (var-get next-document-id))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-OWNER)
    (map-set property-documents
      { property-id: property-id, document-id: doc-id }
      {
        document-hash: document-hash,
        document-type: document-type,
        upload-date: current-time,
        uploader: tx-sender
      }
    )
    (var-set next-document-id (+ doc-id u1))
    (ok doc-id)
  )
)


(define-map property-insurance
  { property-id: uint }
  {
    insurer: principal,
    coverage-amount: uint,
    start-date: uint,
    end-date: uint,
    policy-id: (string-ascii 50),
    active: bool
  }
)

(define-public (register-insurance 
    (property-id uint)
    (coverage-amount uint)
    (duration uint)
    (policy-id (string-ascii 50)))
  (let 
    ((current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)

    (ok (map-set property-insurance
      { property-id: property-id }
      {
        insurer: tx-sender,
        coverage-amount: coverage-amount,
        start-date: current-time,
        end-date: (+ current-time duration),
        policy-id: policy-id,
        active: true
      }))
  )
)


(define-map property-taxes
  { property-id: uint, year: uint }
  {
    amount: uint,
    paid: bool,
    payment-date: (optional uint),
    payment-tx: (optional (buff 32))
  }
)

(define-public (register-property-tax 
    (property-id uint)
    (year uint)
    (amount uint))
  (begin
    (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (ok (map-set property-taxes
      { property-id: property-id, year: year }
      {
        amount: amount,
        paid: false,
        payment-date: none,
        payment-tx: none
      }))
  )
)


(define-map maintenance-records
  { property-id: uint, record-id: uint }
  {
    provider: principal,
    maintenance-type: (string-ascii 50),
    cost: uint,
    date: uint,
    description: (string-ascii 200),
    warranty-end: (optional uint)
  }
)

(define-data-var next-maintenance-id uint u1)

(define-public (add-maintenance-record
    (property-id uint)
    (maintenance-type (string-ascii 50))
    (cost uint)
    (description (string-ascii 200))
    (warranty-duration (optional uint)))
  (let 
    ((record-id (var-get next-maintenance-id))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
     (warranty-end (match warranty-duration
                    duration (some (+ current-time duration))
                    none)))
    (asserts! (or (is-property-owner property-id tx-sender) 
                  (get active (is-verifier tx-sender))) 
              ERR-NOT-AUTHORIZED)
    (map-set maintenance-records
      { property-id: property-id, record-id: record-id }
      {
        provider: tx-sender,
        maintenance-type: maintenance-type,
        cost: cost,
        date: current-time,
        description: description,
        warranty-end: warranty-end
      }
    )
    (var-set next-maintenance-id (+ record-id u1))
    (ok record-id)
  )
)



(define-map property-access-rights
  { property-id: uint, grantee: principal }
  {
    granted-by: principal,
    access-type: (string-ascii 50),
    start-time: uint,
    end-time: uint,
    active: bool
  }
)

(define-public (grant-property-access
    (property-id uint)
    (grantee principal)
    (access-type (string-ascii 50))
    (duration uint))
  (let 
    ((current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    (asserts! (is-property-owner property-id tx-sender) ERR-NOT-OWNER)
    (ok (map-set property-access-rights
      { property-id: property-id, grantee: grantee }
      {
        granted-by: tx-sender,
        access-type: access-type,
        start-time: current-time,
        end-time: (+ current-time duration),
        active: true
      }))
  )
)

(define-public (revoke-property-access
    (property-id uint)
    (grantee principal))
  (let 
    ((access-right (unwrap! (map-get? property-access-rights 
                            { property-id: property-id, grantee: grantee })
                           (err u102))))
    (asserts! (is-eq (get granted-by access-right) tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set property-access-rights
      { property-id: property-id, grantee: grantee }
      (merge access-right { active: false })))
  )
)


(define-read-only (get-auction (property-id uint))
  (map-get? property-auctions { property-id: property-id })
)

(define-read-only (get-auction-bid (property-id uint) (bid-id uint))
  (map-get? auction-bids { property-id: property-id, bid-id: bid-id })
)

(define-read-only (get-bidder-amount (property-id uint) (bidder principal))
  (default-to { total-amount: u0 } 
    (map-get? bidder-amounts { property-id: property-id, bidder: bidder }))
)

(define-read-only (is-auction-ended (property-id uint))
  (match (get-auction property-id)
    auction (let ((current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
              (>= current-time (get end-time auction)))
    false
  )
)

(define-public (create-auction 
    (property-id uint) 
    (duration uint) 
    (reserve-price uint))
  (let 
    ((property (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
     (end-time (+ current-time duration)))
    
    (asserts! (is-eq (get owner property) tx-sender) ERR-NOT-OWNER)
    (asserts! (get verified property) ERR-VERIFICATION-REQUIRED)
    (asserts! (> reserve-price u0) ERR-INVALID-PRICE)
    (asserts! (> duration u0) ERR-INVALID-PRICE)
    (asserts! (is-none (get-auction property-id)) ERR-PROPERTY-EXISTS)
    
    (match (get-property-listing property-id)
      listing (asserts! (not (get active listing)) ERR-AUCTION-ACTIVE)
      true
    )
    
    (map-set property-auctions
      { property-id: property-id }
      {
        seller: tx-sender,
        start-time: current-time,
        end-time: end-time,
        reserve-price: reserve-price,
        highest-bid: u0,
        highest-bidder: none,
        total-bids: u0,
        active: true,
        settled: false
      }
    )
    
    (ok true)
  )
)

(define-public (place-bid (property-id uint) (bid-amount uint))
  (let 
    ((auction (unwrap! (get-auction property-id) ERR-AUCTION-NOT-FOUND))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
     (current-highest (get highest-bid auction))
     (bid-id (+ (get total-bids auction) u1))
     (current-bidder-amount (get total-amount (get-bidder-amount property-id tx-sender))))
    
    (asserts! (get active auction) ERR-AUCTION-NOT-FOUND)
    (asserts! (< current-time (get end-time auction)) ERR-AUCTION-ENDED)
    (asserts! (> bid-amount current-highest) ERR-BID-TOO-LOW)
    (asserts! (> bid-amount u0) ERR-INVALID-PRICE)
    
    (unwrap! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)) ERR-INSUFFICIENT-FUNDS)
    
    (map-set auction-bids
      { property-id: property-id, bid-id: bid-id }
      {
        bidder: tx-sender,
        amount: bid-amount,
        timestamp: current-time,
        refunded: false
      }
    )
    
    (map-set bidder-amounts
      { property-id: property-id, bidder: tx-sender }
      { total-amount: (+ current-bidder-amount bid-amount) }
    )
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction {
        highest-bid: bid-amount,
        highest-bidder: (some tx-sender),
        total-bids: bid-id
      })
    )
    
    (ok bid-id)
  )
)

(define-public (settle-auction (property-id uint))
  (let 
    ((auction (unwrap! (get-auction property-id) ERR-AUCTION-NOT-FOUND))
     (property (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
     (highest-bid (get highest-bid auction))
     (highest-bidder (get highest-bidder auction))
     (seller (get seller auction))
     (reserve-price (get reserve-price auction))
     (transfer-count (get count (get-transfer-count property-id)))
     (new-transfer-count (+ transfer-count u1)))
    
    (asserts! (get active auction) ERR-AUCTION-NOT-FOUND)
    (asserts! (>= current-time (get end-time auction)) ERR-AUCTION-NOT-ENDED)
    (asserts! (not (get settled auction)) ERR-AUCTION-ACTIVE)
    (asserts! (> highest-bid u0) ERR-NO-BIDS)
    (asserts! (>= highest-bid reserve-price) ERR-RESERVE-NOT-MET)
    
    (let ((winner (unwrap! highest-bidder ERR-NO-BIDS)))
      (as-contract (unwrap! (stx-transfer? highest-bid tx-sender seller) ERR-TRANSFER-FAILED))
      
      (map-set properties
        { property-id: property-id }
        (merge property { owner: winner })
      )
      
      (map-set property-history
        { property-id: property-id, transfer-id: new-transfer-count }
        {
          from: seller,
          to: winner,
          price: highest-bid,
          timestamp: current-time,
          verified-by: (some winner)
        }
      )
      
      (map-set property-transfer-count
        { property-id: property-id }
        { count: new-transfer-count }
      )
      
      (map-set property-auctions
        { property-id: property-id }
        (merge auction { active: false, settled: true })
      )
      
      (ok winner)
    )
  )
)

(define-public (cancel-auction (property-id uint))
  (let 
    ((auction (unwrap! (get-auction property-id) ERR-AUCTION-NOT-FOUND)))
    
    (asserts! (is-eq (get seller auction) tx-sender) ERR-NOT-OWNER)
    (asserts! (get active auction) ERR-AUCTION-NOT-FOUND)
    (asserts! (is-eq (get total-bids auction) u0) ERR-AUCTION-ACTIVE)
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction { active: false })
    )
    
    (ok true)
  )
)

(define-public (refund-unsuccessful-bid (property-id uint) (bidder principal))
  (let 
    ((auction (unwrap! (get-auction property-id) ERR-AUCTION-NOT-FOUND))
     (bidder-amount (get total-amount (get-bidder-amount property-id bidder)))
     (highest-bidder (get highest-bidder auction)))
    
    (asserts! (not (get active auction)) ERR-AUCTION-ACTIVE)
    (asserts! (> bidder-amount u0) ERR-INSUFFICIENT-FUNDS)
    (asserts! (not (is-eq (some bidder) highest-bidder)) ERR-NOT-AUTHORIZED)
    
    (as-contract (unwrap! (stx-transfer? bidder-amount tx-sender bidder) ERR-REFUND-FAILED))
    
    (map-set bidder-amounts
      { property-id: property-id, bidder: bidder }
      { total-amount: u0 }
    )
    
    (ok bidder-amount)
  )
)

(define-public (extend-auction (property-id uint) (additional-time uint))
  (let 
    ((auction (unwrap! (get-auction property-id) ERR-AUCTION-NOT-FOUND))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))
    
    (asserts! (is-eq (get seller auction) tx-sender) ERR-NOT-OWNER)
    (asserts! (get active auction) ERR-AUCTION-NOT-FOUND)
    (asserts! (< current-time (get end-time auction)) ERR-AUCTION-ENDED)
    (asserts! (> additional-time u0) ERR-INVALID-PRICE)
    
    (map-set property-auctions
      { property-id: property-id }
      (merge auction { end-time: (+ (get end-time auction) additional-time) })
    )
    
    (ok (+ (get end-time auction) additional-time))
  )
)