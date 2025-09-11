;; Property Neighborhoods Smart Contract
;; Neighborhood zoning, planning restrictions, compliance, and versioned history

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NEIGHBORHOOD-NOT-FOUND (err u101))
(define-constant ERR-PROPERTY-NOT-FOUND (err u102))
(define-constant ERR-INVALID-ZONING (err u103))

;; Zoning constants
(define-constant ZONING-RESIDENTIAL u1)
(define-constant ZONING-COMMERCIAL u2)
(define-constant ZONING-INDUSTRIAL u3)
(define-constant ZONING-MIXED-USE u4)
(define-constant ZONING-AGRICULTURAL u5)

(define-data-var contract-owner principal tx-sender)
(define-map verifiers { address: principal } { active: bool })

;; Neighborhood data with zoning, restrictions, and versioning
(define-map neighborhoods
  { neighborhood-id: uint }
  {
    zoning-type: uint, max-height: uint, max-density: uint,
    residential-allowed: bool, commercial-allowed: bool, industrial-allowed: bool,
    mixed-use-allowed: bool, agricultural-allowed: bool,
    version: uint, last-updated: uint, updated-by: principal
  })

(define-map property-neighborhoods { property-id: uint } { neighborhood-id: uint })
(define-map neighborhood-versions { neighborhood-id: uint } { version: uint })

;; Versioned history
(define-map neighborhood-history
  { neighborhood-id: uint, version: uint }
  {
    zoning-type: uint, max-height: uint, max-density: uint,
    residential-allowed: bool, commercial-allowed: bool, industrial-allowed: bool,
    mixed-use-allowed: bool, agricultural-allowed: bool,
    updated-at: uint, updated-by: principal
  })

(define-read-only (is-verifier (address principal))
  (default-to { active: false } (map-get? verifiers { address: address })))

(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner)))

(define-public (add-verifier (verifier-address principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers { address: verifier-address } { active: true }))))

(define-public (remove-verifier (verifier-address principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set verifiers { address: verifier-address } { active: false }))))

(define-public (set-neighborhood-zoning
    (neighborhood-id uint) (zoning-type uint) (max-height uint) (max-density uint)
    (residential-allowed bool) (commercial-allowed bool) (industrial-allowed bool)
    (mixed-use-allowed bool) (agricultural-allowed bool))
  (let
    ((current-version (get version (default-to { version: u0 } 
                                   (map-get? neighborhood-versions { neighborhood-id: neighborhood-id }))))
     (new-version (+ current-version u1))
     (current-time stacks-block-height))
    (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= zoning-type ZONING-RESIDENTIAL) (<= zoning-type ZONING-AGRICULTURAL)) ERR-INVALID-ZONING)
    (asserts! (and (> max-height u0) (> max-density u0)) ERR-INVALID-ZONING)
    
    (map-set neighborhood-history
      { neighborhood-id: neighborhood-id, version: new-version }
      { zoning-type: zoning-type, max-height: max-height, max-density: max-density,
        residential-allowed: residential-allowed, commercial-allowed: commercial-allowed,
        industrial-allowed: industrial-allowed, mixed-use-allowed: mixed-use-allowed,
        agricultural-allowed: agricultural-allowed, updated-at: current-time, updated-by: tx-sender })
    
    (map-set neighborhoods
      { neighborhood-id: neighborhood-id }
      { zoning-type: zoning-type, max-height: max-height, max-density: max-density,
        residential-allowed: residential-allowed, commercial-allowed: commercial-allowed,
        industrial-allowed: industrial-allowed, mixed-use-allowed: mixed-use-allowed,
        agricultural-allowed: agricultural-allowed, version: new-version,
        last-updated: current-time, updated-by: tx-sender })
    
    (map-set neighborhood-versions { neighborhood-id: neighborhood-id } { version: new-version })
    (ok new-version)))

(define-public (assign-property-to-neighborhood (property-id uint) (neighborhood-id uint))
  (begin
    (asserts! (get active (is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? neighborhoods { neighborhood-id: neighborhood-id })) ERR-NEIGHBORHOOD-NOT-FOUND)
    (map-set property-neighborhoods { property-id: property-id } { neighborhood-id: neighborhood-id })
    (ok true)))

(define-read-only (get-neighborhood (neighborhood-id uint))
  (map-get? neighborhoods { neighborhood-id: neighborhood-id }))

(define-read-only (get-property-neighborhood (property-id uint))
  (map-get? property-neighborhoods { property-id: property-id }))

(define-read-only (get-property-zoning (property-id uint))
  (match (map-get? property-neighborhoods { property-id: property-id })
    assignment (map-get? neighborhoods { neighborhood-id: (get neighborhood-id assignment) })
    none))

(define-read-only (get-neighborhood-version-count (neighborhood-id uint))
  (get version (default-to { version: u0 } 
                           (map-get? neighborhood-versions { neighborhood-id: neighborhood-id }))))

(define-read-only (get-neighborhood-history (neighborhood-id uint) (version uint))
  (map-get? neighborhood-history { neighborhood-id: neighborhood-id, version: version }))

(define-read-only (is-usage-allowed (neighborhood-id uint) (zoning-usage uint))
  (match (map-get? neighborhoods { neighborhood-id: neighborhood-id })
    neighborhood
      (ok
        (if (is-eq zoning-usage ZONING-RESIDENTIAL) (get residential-allowed neighborhood)
          (if (is-eq zoning-usage ZONING-COMMERCIAL) (get commercial-allowed neighborhood)
            (if (is-eq zoning-usage ZONING-INDUSTRIAL) (get industrial-allowed neighborhood)
              (if (is-eq zoning-usage ZONING-MIXED-USE) (get mixed-use-allowed neighborhood)
                (get agricultural-allowed neighborhood))))))
    ERR-NEIGHBORHOOD-NOT-FOUND))

(define-read-only (check-neighborhood-compliance 
    (neighborhood-id uint) (proposed-height uint) (proposed-density uint) (proposed-usage uint))
  (match (map-get? neighborhoods { neighborhood-id: neighborhood-id })
    neighborhood
      (let
        ((height-compliant (<= proposed-height (get max-height neighborhood)))
         (density-compliant (<= proposed-density (get max-density neighborhood))))
        (match (is-usage-allowed neighborhood-id proposed-usage)
          usage-allowed (ok (and height-compliant (and density-compliant usage-allowed)))
          error-code (err error-code)))
    ERR-NEIGHBORHOOD-NOT-FOUND))

(define-read-only (check-property-compliance 
    (property-id uint) (proposed-height uint) (proposed-density uint) (proposed-usage uint))
  (match (map-get? property-neighborhoods { property-id: property-id })
    assignment 
      (check-neighborhood-compliance (get neighborhood-id assignment) 
                                     proposed-height proposed-density proposed-usage)
    ERR-PROPERTY-NOT-FOUND))

(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)))
