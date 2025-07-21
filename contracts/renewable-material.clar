;; ================================
;; RENEWABLE BUILDING MATERIALS PLATFORM
;; ================================
;; A decentralized platform for sustainable construction supplies,
;; bamboo cultivation, and eco-friendly building practices

;; ================================
;; CONSTANTS & ERROR CODES
;; ================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_PARAMS (err u400))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_BALANCE (err u402))
(define-constant ERR_INVALID_STATUS (err u405))

;; Material types
(define-constant BAMBOO u1)
(define-constant RECYCLED_WOOD u2)
(define-constant NATURAL_FIBER u3)
(define-constant BIO_COMPOSITE u4)
(define-constant EARTH_BLOCK u5)

;; Production stages
(define-constant CULTIVATION u1)
(define-constant HARVESTING u2)
(define-constant PROCESSING u3)
(define-constant QUALITY_CHECK u4)
(define-constant READY_FOR_USE u5)

;; ================================
;; DATA STRUCTURES
;; ================================

;; Producer registration and profile
(define-map producers
  principal
  {
    name: (string-ascii 100),
    location: (string-ascii 200),
    specializations: (list 10 uint),
    carbon-credits: uint,
    reputation-score: uint,
    total-materials-produced: uint,
    certification-level: uint,
    joined-at: uint
  })

;; Material batch tracking with full lifecycle
(define-map material-batches
  uint ;; batch-id
  {
    producer: principal,
    material-type: uint,
    quantity: uint,
    unit: (string-ascii 20),
    cultivation-start: uint,
    harvest-date: (optional uint),
    processing-date: (optional uint),
    quality-score: uint,
    carbon-sequestered: uint,
    location-coordinates: (string-ascii 100),
    certifications: (list 5 (string-ascii 50)),
    current-stage: uint,
    environmental-impact: {
      water-usage: uint,
      energy-consumption: uint,
      waste-generated: uint,
      biodiversity-score: uint
    },
    price-per-unit: uint,
    available-quantity: uint,
    reserved-quantity: uint
  })

;; Building technique knowledge base
(define-map building-techniques
  uint ;; technique-id
  {
    creator: principal,
    name: (string-ascii 200),
    description: (string-ascii 1000),
    materials-required: (list 20 uint),
    difficulty-level: uint,
    estimated-time: uint,
    carbon-impact: int, ;; can be negative (sequestration)
    cost-effectiveness: uint,
    sustainability-score: uint,
    votes-up: uint,
    votes-down: uint,
    usage-count: uint,
    created-at: uint
  })

;; Construction projects using sustainable materials
(define-map construction-projects
  uint ;; project-id
  {
    owner: principal,
    name: (string-ascii 200),
    project-type: (string-ascii 100),
    location: (string-ascii 200),
    materials-used: (list 50 uint),
    techniques-applied: (list 20 uint),
    total-carbon-sequestered: uint,
    environmental-score: uint,
    completion-percentage: uint,
    start-date: uint,
    estimated-completion: uint,
    budget: uint,
    community-supported: bool,
    open-source: bool
  })

;; Skill development and certification tracking
(define-map user-skills
  principal
  {
    completed-workshops: (list 50 uint),
    certifications: (list 10 uint),
    skill-points: uint,
    mentorship-hours: uint,
    projects-completed: uint,
    specializations: (list 10 uint),
    last-activity: uint
  })

;; Material marketplace orders
(define-map material-orders
  uint ;; order-id
  {
    buyer: principal,
    seller: principal,
    batch-id: uint,
    quantity: uint,
    total-price: uint,
    order-status: uint, ;; 1: pending, 2: confirmed, 3: shipped, 4: delivered, 5: completed
    created-at: uint,
    delivery-date: (optional uint),
    quality-rating: (optional uint),
    feedback: (optional (string-ascii 500))
  })

;; ================================
;; DATA VARIABLES
;; ================================

(define-data-var next-batch-id uint u1)
(define-data-var next-technique-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-order-id uint u1)

;; Platform statistics
(define-data-var total-carbon-sequestered uint u0)
(define-data-var total-materials-produced uint u0)
(define-data-var active-producers uint u0)
(define-data-var completed-projects uint u0)

;; ================================
;; PRODUCER MANAGEMENT
;; ================================

;; Register as a sustainable materials producer
(define-public (register-producer
  (name (string-ascii 100))
  (location (string-ascii 200))
  (specializations (list 10 uint)))
  (begin
    (asserts! (is-none (map-get? producers tx-sender)) ERR_ALREADY_EXISTS)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len location) u0) ERR_INVALID_PARAMS)

    (map-set producers tx-sender {
      name: name,
      location: location,
      specializations: specializations,
      carbon-credits: u0,
      reputation-score: u50, ;; Starting reputation
      total-materials-produced: u0,
      certification-level: u1,
      joined-at: stacks-block-height
    })

    (var-set active-producers (+ (var-get active-producers) u1))
    (ok true)))

;; Update producer profile
(define-public (update-producer-profile
  (name (string-ascii 100))
  (location (string-ascii 200))
  (specializations (list 10 uint)))
  (let ((producer-data (unwrap! (map-get? producers tx-sender) ERR_NOT_FOUND)))
    (map-set producers tx-sender (merge producer-data {
      name: name,
      location: location,
      specializations: specializations
    }))
    (ok true)))

;; ================================
;; MATERIAL BATCH MANAGEMENT
;; ================================

;; Create a new material batch for tracking
(define-public (create-material-batch
  (material-type uint)
  (quantity uint)
  (unit (string-ascii 20))
  (location-coordinates (string-ascii 100))
  (price-per-unit uint))
  (let ((batch-id (var-get next-batch-id))
        (producer-data (unwrap! (map-get? producers tx-sender) ERR_UNAUTHORIZED)))

    (asserts! (and (>= material-type u1) (<= material-type u5)) ERR_INVALID_PARAMS)
    (asserts! (> quantity u0) ERR_INVALID_PARAMS)
    (asserts! (> price-per-unit u0) ERR_INVALID_PARAMS)

    (map-set material-batches batch-id {
      producer: tx-sender,
      material-type: material-type,
      quantity: quantity,
      unit: unit,
      cultivation-start: stacks-block-height,
      harvest-date: none,
      processing-date: none,
      quality-score: u0,
      carbon-sequestered: u0,
      location-coordinates: location-coordinates,
      certifications: (list),
      current-stage: CULTIVATION,
      environmental-impact: {
        water-usage: u0,
        energy-consumption: u0,
        waste-generated: u0,
        biodiversity-score: u0
      },
      price-per-unit: price-per-unit,
      available-quantity: quantity,
      reserved-quantity: u0
    })

    (var-set next-batch-id (+ batch-id u1))
    (var-set total-materials-produced (+ (var-get total-materials-produced) quantity))

    ;; Update producer stats
    (map-set producers tx-sender (merge producer-data {
      total-materials-produced: (+ (get total-materials-produced producer-data) quantity)
    }))

    (ok batch-id)))

;; Update batch progress through production stages
(define-public (update-batch-stage
  (batch-id uint)
  (new-stage uint)
  (quality-score uint)
  (carbon-sequestered uint)
  (environmental-impact {water-usage: uint, energy-consumption: uint, waste-generated: uint, biodiversity-score: uint}))
  (let ((batch-data (unwrap! (map-get? material-batches batch-id) ERR_NOT_FOUND)))

    (asserts! (is-eq (get producer batch-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (and (>= new-stage u1) (<= new-stage u5)) ERR_INVALID_PARAMS)
    (asserts! (<= quality-score u100) ERR_INVALID_PARAMS)

    (map-set material-batches batch-id (merge batch-data {
      current-stage: new-stage,
      quality-score: quality-score,
      carbon-sequestered: carbon-sequestered,
      environmental-impact: environmental-impact,
      harvest-date: (if (>= new-stage HARVESTING) (some stacks-block-height) (get harvest-date batch-data)),
      processing-date: (if (>= new-stage PROCESSING) (some stacks-block-height) (get processing-date batch-data))
    }))

    ;; Update global carbon sequestration tracking
    (var-set total-carbon-sequestered (+ (var-get total-carbon-sequestered) carbon-sequestered))

    (ok true)))

;; ================================
;; BUILDING TECHNIQUES KNOWLEDGE BASE
;; ================================

;; Submit a new sustainable building technique
(define-public (submit-building-technique
  (name (string-ascii 200))
  (description (string-ascii 1000))
  (materials-required (list 20 uint))
  (difficulty-level uint)
  (estimated-time uint)
  (carbon-impact int)
  (sustainability-score uint))
  (let ((technique-id (var-get next-technique-id)))

    (asserts! (> (len name) u0) ERR_INVALID_PARAMS)
    (asserts! (> (len description) u0) ERR_INVALID_PARAMS)
    (asserts! (and (>= difficulty-level u1) (<= difficulty-level u5)) ERR_INVALID_PARAMS)
    (asserts! (<= sustainability-score u100) ERR_INVALID_PARAMS)

    (map-set building-techniques technique-id {
      creator: tx-sender,
      name: name,
      description: description,
      materials-required: materials-required,
      difficulty-level: difficulty-level,
      estimated-time: estimated-time,
      carbon-impact: carbon-impact,
      cost-effectiveness: u0,
      sustainability-score: sustainability-score,
      votes-up: u0,
      votes-down: u0,
      usage-count: u0,
      created-at: stacks-block-height
    })

    (var-set next-technique-id (+ technique-id u1))
    (ok technique-id)))

;; Vote on building technique quality
(define-public (vote-technique (technique-id uint) (vote-up bool))
  (let ((technique-data (unwrap! (map-get? building-techniques technique-id) ERR_NOT_FOUND)))
    (map-set building-techniques technique-id (merge technique-data {
      votes-up: (if vote-up (+ (get votes-up technique-data) u1) (get votes-up technique-data)),
      votes-down: (if vote-up (get votes-down technique-data) (+ (get votes-down technique-data) u1))
    }))
    (ok true)))

;; ================================
;; CONSTRUCTION PROJECT TRACKING
;; ================================

;; Register a new construction project
(define-public (register-construction-project
  (name (string-ascii 200))
  (project-type (string-ascii 100))
  (location (string-ascii 200))
  (estimated-completion uint)
  (budget uint)
  (community-supported bool)
  (open-source bool))
  (let ((project-id (var-get next-project-id)))

    (asserts! (> (len name) u0) ERR_INVALID_PARAMS)
    (asserts! (> budget u0) ERR_INVALID_PARAMS)

    (map-set construction-projects project-id {
      owner: tx-sender,
      name: name,
      project-type: project-type,
      location: location,
      materials-used: (list),
      techniques-applied: (list),
      total-carbon-sequestered: u0,
      environmental-score: u0,
      completion-percentage: u0,
      start-date: stacks-block-height,
      estimated-completion: estimated-completion,
      budget: budget,
      community-supported: community-supported,
      open-source: open-source
    })

    (var-set next-project-id (+ project-id u1))
    (ok project-id)))

;; Update project progress
(define-public (update-project-progress
  (project-id uint)
  (completion-percentage uint)
  (materials-used (list 50 uint))
  (techniques-applied (list 20 uint))
  (environmental-score uint))
  (let ((project-data (unwrap! (map-get? construction-projects project-id) ERR_NOT_FOUND)))

    (asserts! (is-eq (get owner project-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= completion-percentage u100) ERR_INVALID_PARAMS)

    (map-set construction-projects project-id (merge project-data {
      completion-percentage: completion-percentage,
      materials-used: materials-used,
      techniques-applied: techniques-applied,
      environmental-score: environmental-score
    }))

    ;; Track completed projects
    (if (is-eq completion-percentage u100)
      (var-set completed-projects (+ (var-get completed-projects) u1))
      true)

    (ok true)))

;; ================================
;; MATERIAL MARKETPLACE
;; ================================

;; Create an order for sustainable materials
(define-public (create-material-order
  (batch-id uint)
  (quantity uint))
  (let ((batch-data (unwrap! (map-get? material-batches batch-id) ERR_NOT_FOUND))
        (order-id (var-get next-order-id)))

    (asserts! (not (is-eq tx-sender (get producer batch-data))) ERR_INVALID_PARAMS)
    (asserts! (is-eq (get current-stage batch-data) READY_FOR_USE) ERR_INVALID_STATUS)
    (asserts! (<= quantity (get available-quantity batch-data)) ERR_INSUFFICIENT_BALANCE)

    (let ((total-price (* quantity (get price-per-unit batch-data))))

      ;; Reserve the materials
      (map-set material-batches batch-id (merge batch-data {
        available-quantity: (- (get available-quantity batch-data) quantity),
        reserved-quantity: (+ (get reserved-quantity batch-data) quantity)
      }))

      ;; Create the order
      (map-set material-orders order-id {
        buyer: tx-sender,
        seller: (get producer batch-data),
        batch-id: batch-id,
        quantity: quantity,
        total-price: total-price,
        order-status: u1, ;; pending
        created-at: stacks-block-height,
        delivery-date: none,
        quality-rating: none,
        feedback: none
      })

      (var-set next-order-id (+ order-id u1))
      (ok order-id))))

;; Confirm material order and arrange delivery
(define-public (confirm-order
  (order-id uint)
  (delivery-date uint))
  (let ((order-data (unwrap! (map-get? material-orders order-id) ERR_NOT_FOUND)))

    (asserts! (is-eq (get seller order-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get order-status order-data) u1) ERR_INVALID_STATUS)

    (map-set material-orders order-id (merge order-data {
      order-status: u2, ;; confirmed
      delivery-date: (some delivery-date)
    }))

    (ok true)))

;; ================================
;; SKILL DEVELOPMENT SYSTEM
;; ================================

;; Record completion of sustainability workshop
(define-public (complete-workshop (workshop-id uint))
  (let ((current-skills (default-to
    {completed-workshops: (list), certifications: (list), skill-points: u0,
     mentorship-hours: u0, projects-completed: u0, specializations: (list),
     last-activity: stacks-block-height}
    (map-get? user-skills tx-sender))))

    (map-set user-skills tx-sender (merge current-skills {
      completed-workshops: (unwrap! (as-max-len? (append (get completed-workshops current-skills) workshop-id) u50) ERR_INVALID_PARAMS),
      skill-points: (+ (get skill-points current-skills) u10),
      last-activity: stacks-block-height
    }))

    (ok true)))

;; Award certification for sustainable building practices
(define-public (award-certification (user principal) (certification-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (let ((current-skills (default-to
      {completed-workshops: (list), certifications: (list), skill-points: u0,
       mentorship-hours: u0, projects-completed: u0, specializations: (list),
       last-activity: stacks-block-height}
      (map-get? user-skills user))))

      (map-set user-skills user (merge current-skills {
        certifications: (unwrap! (as-max-len? (append (get certifications current-skills) certification-id) u10) ERR_INVALID_PARAMS),
        skill-points: (+ (get skill-points current-skills) u50)
      }))

      (ok true))))

;; ================================
;; READ-ONLY FUNCTIONS
;; ================================

;; Get producer information
(define-read-only (get-producer (producer principal))
  (map-get? producers producer))

;; Get material batch details
(define-read-only (get-material-batch (batch-id uint))
  (map-get? material-batches batch-id))

;; Get building technique information
(define-read-only (get-building-technique (technique-id uint))
  (map-get? building-techniques technique-id))

;; Get construction project details
(define-read-only (get-construction-project (project-id uint))
  (map-get? construction-projects project-id))

;; Get material order information
(define-read-only (get-material-order (order-id uint))
  (map-get? material-orders order-id))

;; Get user skills and certifications
(define-read-only (get-user-skills (user principal))
  (map-get? user-skills user))

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-carbon-sequestered: (var-get total-carbon-sequestered),
    total-materials-produced: (var-get total-materials-produced),
    active-producers: (var-get active-producers),
    completed-projects: (var-get completed-projects),
    current-block: stacks-block-height
  })

;; Calculate environmental impact score for a material batch
(define-read-only (calculate-environmental-impact (batch-id uint))
  (match (map-get? material-batches batch-id)
    batch-data
    (let ((impact (get environmental-impact batch-data))
          (carbon-score (* (get carbon-sequestered batch-data) u2))
          (efficiency-score (- u100 (/ (+ (get water-usage impact) (get energy-consumption impact)) u20)))
          (waste-penalty (/ (get waste-generated impact) u10))
          (biodiversity-bonus (get biodiversity-score impact)))
      (some (+ carbon-score efficiency-score biodiversity-bonus (- waste-penalty))))
    none))

;; Get top-rated building techniques
(define-read-only (get-technique-rating (technique-id uint))
  (match (map-get? building-techniques technique-id)
    technique-data
    (let ((total-votes (+ (get votes-up technique-data) (get votes-down technique-data))))
      (if (> total-votes u0)
        (some (/ (* (get votes-up technique-data) u100) total-votes))
        (some u50))) ;; neutral rating for new techniques
    none))
