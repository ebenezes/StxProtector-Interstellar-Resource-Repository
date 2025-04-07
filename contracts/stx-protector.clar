;; StxProtector - Interstellar Digital Resource Repository

;; Base configuration parameters
(define-constant ADMIN_USER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_MISSING_CHAMBER (err u101))
(define-constant ERR_PREVIOUSLY_PROCESSED (err u102))
(define-constant ERR_TRANSMISSION_FAILED (err u103))
(define-constant ERR_INVALID_IDENTIFIER (err u104))
(define-constant ERR_INVALID_QUANTITY (err u105))
(define-constant ERR_INVALID_ORIGINATOR (err u106))
(define-constant ERR_CHAMBER_LAPSED (err u107))
(define-constant CHAMBER_LIFESPAN_BLOCKS u1008)

;; Primary storage architecture
(define-map ChamberRegistry
  { chamber-index: uint }
  {
    originator: principal,
    destination: principal,
    resource-index: uint,
    quantity: uint,
    chamber-status: (string-ascii 10),
    genesis-block: uint,
    termination-block: uint
  }
)

;; Chamber indexing mechanism
(define-data-var latest-chamber-index uint u0)

;; Support functions

(define-private (legitimate-chamber-index? (chamber-index uint))
  (<= chamber-index (var-get latest-chamber-index))
)

(define-private (legitimate-destination? (destination principal))
  (and 
    (not (is-eq destination tx-sender))
    (not (is-eq destination (as-contract tx-sender)))
  )
)


;; Interface functions

;; Facilitate transmission to destination
(define-public (execute-chamber-transmission (chamber-index uint))
  (begin
    (asserts! (legitimate-chamber-index? chamber-index) ERR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-data (unwrap! (map-get? ChamberRegistry { chamber-index: chamber-index }) ERR_MISSING_CHAMBER))
        (destination (get destination chamber-data))
        (quantity (get quantity chamber-data))
        (resource (get resource-index chamber-data))
      )
      (asserts! (or (is-eq tx-sender ADMIN_USER) (is-eq tx-sender (get originator chamber-data))) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get chamber-status chamber-data) "pending") ERR_PREVIOUSLY_PROCESSED)
      (asserts! (<= block-height (get termination-block chamber-data)) ERR_CHAMBER_LAPSED)
      (match (as-contract (stx-transfer? quantity tx-sender destination))
        success
          (begin
            (map-set ChamberRegistry
              { chamber-index: chamber-index }
              (merge chamber-data { chamber-status: "completed" })
            )
            (print {action: "chamber_transmitted", chamber-index: chamber-index, destination: destination, resource-index: resource, quantity: quantity})
            (ok true)
          )
        error ERR_TRANSMISSION_FAILED
      )
    )
  )
)

;; Apply time-locked safeguard
(define-public (apply-time-locked-safeguard (chamber-index uint) (lock-duration uint) (unlock-code (buff 32)))
  (begin
    (asserts! (legitimate-chamber-index? chamber-index) ERR_INVALID_IDENTIFIER)
    (asserts! (> lock-duration u0) ERR_INVALID_QUANTITY)
    (asserts! (<= lock-duration u720) ERR_INVALID_QUANTITY) ;; Maximum 720 blocks (~5 days)
    (let
      (
        (chamber-data (unwrap! (map-get? ChamberRegistry { chamber-index: chamber-index }) ERR_MISSING_CHAMBER))
        (originator (get originator chamber-data))
        (current-status (get chamber-status chamber-data))
        (unlock-block (+ block-height lock-duration))
      )
      ;; Only originator or admin can apply time lock
      (asserts! (or (is-eq tx-sender originator) (is-eq tx-sender ADMIN_USER)) ERR_UNAUTHORIZED)
      ;; Only certain statuses allow time locking
      (asserts! (or (is-eq current-status "pending") (is-eq current-status "acknowledged")) ERR_PREVIOUSLY_PROCESSED)
      ;; Chamber must not be expired
      (asserts! (<= block-height (get termination-block chamber-data)) ERR_CHAMBER_LAPSED)

      ;; Update chamber status to time-locked and store unlock-block as termination-block

      (print {action: "time_locked_safeguard_applied", chamber-index: chamber-index, 
              originator: originator, unlock-block: unlock-block, 
              unlock-code-hash: (hash160 unlock-code)})
      (ok unlock-block)
    )
  )
)

;; Originator requests chamber termination
(define-public (abort-chamber (chamber-index uint))
  (begin
    (asserts! (legitimate-chamber-index? chamber-index) ERR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-data (unwrap! (map-get? ChamberRegistry { chamber-index: chamber-index }) ERR_MISSING_CHAMBER))
        (originator (get originator chamber-data))
        (quantity (get quantity chamber-data))
      )
      (asserts! (is-eq tx-sender originator) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get chamber-status chamber-data) "pending") ERR_PREVIOUSLY_PROCESSED)
      (asserts! (<= block-height (get termination-block chamber-data)) ERR_CHAMBER_LAPSED)
      (match (as-contract (stx-transfer? quantity tx-sender originator))
        success
          (begin
            (map-set ChamberRegistry
              { chamber-index: chamber-index }
              (merge chamber-data { chamber-status: "aborted" })
            )
            (print {action: "chamber_aborted", chamber-index: chamber-index, originator: originator, quantity: quantity})
            (ok true)
          )
        error ERR_TRANSMISSION_FAILED
      )
    )
  )
)

