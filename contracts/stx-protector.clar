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

