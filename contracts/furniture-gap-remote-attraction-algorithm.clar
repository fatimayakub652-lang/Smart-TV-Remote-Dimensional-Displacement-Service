;; Furniture Gap Remote Attraction Algorithm Smart Contract
;; Generates gravitational fields that pull remotes into spaces exactly 1mm too narrow for human fingers

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_COORDINATES (err u402))
(define-constant ERR_REMOTE_NOT_FOUND (err u404))
(define-constant ERR_GAP_TOO_WIDE (err u405))
(define-constant ERR_ALREADY_ATTRACTED (err u406))

;; Data structures
(define-map remotes 
  { remote-id: uint }
  {
    owner: principal,
    x-coordinate: int,
    y-coordinate: int,
    z-coordinate: int,
    is-attracted: bool,
    attraction-timestamp: uint
  }
)

(define-map furniture-gaps
  { gap-id: uint }
  {
    furniture-type: (string-ascii 50),
    x-position: int,
    y-position: int,
    z-position: int,
    gap-width: uint,
    attraction-force: uint,
    is-active: bool
  }
)

(define-map gravitational-fields
  { field-id: uint }
  {
    center-x: int,
    center-y: int,
    center-z: int,
    field-strength: uint,
    radius: uint,
    target-gap-id: uint
  }
)

;; Data variables
(define-data-var next-remote-id uint u1)
(define-data-var next-gap-id uint u1)
(define-data-var next-field-id uint u1)
(define-data-var total-remotes-attracted uint u0)
(define-data-var system-active bool true)

;; Private functions
(define-private (calculate-distance (x1 int) (y1 int) (z1 int) (x2 int) (y2 int) (z2 int))
  (let (
    (dx (- x1 x2))
    (dy (- y1 y2))
    (dz (- z1 z2))
  )
    ;; Simple distance calculation (sum of absolute differences)
    (+ (if (> dx 0) dx (- 0 dx))
       (if (> dy 0) dy (- 0 dy))
       (if (> dz 0) dz (- 0 dz)))
  )
)

(define-private (is-gap-suitable (gap-width uint))
  ;; Gap must be between 1-2mm (represented as 1-2 units)
  (and (>= gap-width u1) (<= gap-width u2))
)

(define-private (calculate-attraction-force (distance int) (field-strength uint))
  ;; Attraction force inversely proportional to distance
  (if (> distance 0)
    (/ field-strength (to-uint distance))
    field-strength
  )
)

;; Public functions
(define-public (register-remote (owner principal) (x int) (y int) (z int))
  (let (
    (remote-id (var-get next-remote-id))
  )
    (asserts! (var-get system-active) ERR_UNAUTHORIZED)
    (asserts! (and (>= x -1000) (<= x 1000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= y -1000) (<= y 1000)) ERR_INVALID_COORDINATES)
    (asserts! (and (>= z -1000) (<= z 1000)) ERR_INVALID_COORDINATES)
    
    (map-set remotes
      { remote-id: remote-id }
      {
        owner: owner,
        x-coordinate: x,
        y-coordinate: y,
        z-coordinate: z,
        is-attracted: false,
        attraction-timestamp: u0
      }
    )
    
    (var-set next-remote-id (+ remote-id u1))
    (ok remote-id)
  )
)

(define-public (create-furniture-gap (furniture-type (string-ascii 50)) (x int) (y int) (z int) (gap-width uint))
  (let (
    (gap-id (var-get next-gap-id))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (var-get system-active) ERR_UNAUTHORIZED)
    (asserts! (is-gap-suitable gap-width) ERR_GAP_TOO_WIDE)
    
    (map-set furniture-gaps
      { gap-id: gap-id }
      {
        furniture-type: furniture-type,
        x-position: x,
        y-position: y,
        z-position: z,
        gap-width: gap-width,
        attraction-force: (* gap-width u100), ;; Force based on gap width
        is-active: true
      }
    )
    
    (var-set next-gap-id (+ gap-id u1))
    (ok gap-id)
  )
)

(define-public (generate-gravitational-field (center-x int) (center-y int) (center-z int) (field-strength uint) (radius uint) (target-gap-id uint))
  (let (
    (field-id (var-get next-field-id))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (var-get system-active) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? furniture-gaps { gap-id: target-gap-id })) ERR_INVALID_COORDINATES)
    
    (map-set gravitational-fields
      { field-id: field-id }
      {
        center-x: center-x,
        center-y: center-y,
        center-z: center-z,
        field-strength: field-strength,
        radius: radius,
        target-gap-id: target-gap-id
      }
    )
    
    (var-set next-field-id (+ field-id u1))
    (ok field-id)
  )
)

(define-public (attract-remote (remote-id uint) (field-id uint))
  (let (
    (remote-data (unwrap! (map-get? remotes { remote-id: remote-id }) ERR_REMOTE_NOT_FOUND))
    (field-data (unwrap! (map-get? gravitational-fields { field-id: field-id }) ERR_INVALID_COORDINATES))
    (distance (calculate-distance
      (get x-coordinate remote-data)
      (get y-coordinate remote-data)
      (get z-coordinate remote-data)
      (get center-x field-data)
      (get center-y field-data)
      (get center-z field-data)
    ))
  )
    (asserts! (var-get system-active) ERR_UNAUTHORIZED)
    (asserts! (not (get is-attracted remote-data)) ERR_ALREADY_ATTRACTED)
    (asserts! (<= (to-uint distance) (get radius field-data)) ERR_INVALID_COORDINATES)
    
    ;; Update remote to attracted state
    (map-set remotes
      { remote-id: remote-id }
      (merge remote-data {
        is-attracted: true,
        attraction-timestamp: stacks-block-height
      })
    )
    
    ;; Increment total attracted remotes counter
    (var-set total-remotes-attracted (+ (var-get total-remotes-attracted) u1))
    
    (ok {
      remote-id: remote-id,
      attracted-to-gap: (get target-gap-id field-data),
      attraction-force: (calculate-attraction-force distance (get field-strength field-data)),
      timestamp: stacks-block-height
    })
  )
)

(define-public (deactivate-gap (gap-id uint))
  (let (
    (gap-data (unwrap! (map-get? furniture-gaps { gap-id: gap-id }) ERR_INVALID_COORDINATES))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set furniture-gaps
      { gap-id: gap-id }
      (merge gap-data { is-active: false })
    )
    
    (ok gap-id)
  )
)

(define-public (toggle-system (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set system-active active)
    (ok active)
  )
)

;; Read-only functions
(define-read-only (get-remote-info (remote-id uint))
  (map-get? remotes { remote-id: remote-id })
)

(define-read-only (get-gap-info (gap-id uint))
  (map-get? furniture-gaps { gap-id: gap-id })
)

(define-read-only (get-field-info (field-id uint))
  (map-get? gravitational-fields { field-id: field-id })
)

(define-read-only (get-system-stats)
  {
    total-remotes-attracted: (var-get total-remotes-attracted),
    next-remote-id: (var-get next-remote-id),
    next-gap-id: (var-get next-gap-id),
    next-field-id: (var-get next-field-id),
    system-active: (var-get system-active)
  }
)

(define-read-only (is-remote-in-range (remote-id uint) (field-id uint))
  (match (map-get? remotes { remote-id: remote-id })
    remote-data
      (match (map-get? gravitational-fields { field-id: field-id })
        field-data
          (let (
            (distance (calculate-distance
              (get x-coordinate remote-data)
              (get y-coordinate remote-data)
              (get z-coordinate remote-data)
              (get center-x field-data)
              (get center-y field-data)
              (get center-z field-data)
            ))
          )
            (<= (to-uint distance) (get radius field-data))
          )
        false
      )
    false
  )
)

;; title: furniture-gap-remote-attraction-algorithm
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

