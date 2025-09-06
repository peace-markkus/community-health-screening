
;; Community Health Screening Contract
;; A preventive care platform for screening event coordination, results management, 
;; follow-up care, and population health tracking

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-event-full (err u105))
(define-constant err-event-ended (err u106))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Event status constants
(define-constant status-scheduled u1)
(define-constant status-active u2)
(define-constant status-completed u3)
(define-constant status-cancelled u4)

;; Screening result status
(define-constant result-pending u1)
(define-constant result-normal u2)
(define-constant result-abnormal u3)
(define-constant result-requires-followup u4)

;; Data structures
(define-map screening-events
  { event-id: uint }
  {
    organizer: principal,
    title: (string-ascii 100),
    location: (string-ascii 100),
    start-block: uint,
    end-block: uint,
    max-participants: uint,
    current-participants: uint,
    screening-type: (string-ascii 50),
    status: uint
  }
)

(define-map participant-registrations
  { event-id: uint, participant: principal }
  {
    registration-block: uint,
    attended: bool,
    screening-completed: bool
  }
)

(define-map screening-results
  { result-id: uint }
  {
    event-id: uint,
    participant: principal,
    healthcare-provider: principal,
    result-status: uint,
    test-values: (string-ascii 200),
    recommendations: (string-ascii 300),
    followup-required: bool,
    recorded-at: uint
  }
)

(define-map followup-care
  { followup-id: uint }
  {
    result-id: uint,
    participant: principal,
    provider: principal,
    scheduled-block: uint,
    completed: bool,
    notes: (string-ascii 200)
  }
)

(define-map healthcare-providers
  { provider: principal }
  {
    name: (string-ascii 100),
    specialization: (string-ascii 50),
    license-number: (string-ascii 50),
    verified: bool
  }
)

;; Counters for unique IDs
(define-data-var next-event-id uint u1)
(define-data-var next-result-id uint u1)
(define-data-var next-followup-id uint u1)

;; Population health tracking
(define-data-var total-screenings-conducted uint u0)
(define-data-var total-abnormal-results uint u0)
(define-data-var total-followups-completed uint u0)

;; Private functions
(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

(define-private (get-current-block)
  stacks-block-height
)

;; Public functions for event management
(define-public (create-screening-event 
  (title (string-ascii 100))
  (location (string-ascii 100))
  (start-block uint)
  (end-block uint)
  (max-participants uint)
  (screening-type (string-ascii 50))
)
  (let (
    (event-id (var-get next-event-id))
  )
    (asserts! (> end-block start-block) err-invalid-status)
    (asserts! (> max-participants u0) err-invalid-status)
    (map-set screening-events
      { event-id: event-id }
      {
        organizer: tx-sender,
        title: title,
        location: location,
        start-block: start-block,
        end-block: end-block,
        max-participants: max-participants,
        current-participants: u0,
        screening-type: screening-type,
        status: status-scheduled
      }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (register-for-screening (event-id uint))
  (let (
    (event-data (unwrap! (map-get? screening-events { event-id: event-id }) err-not-found))
    (current-block (get-current-block))
  )
    (asserts! (< current-block (get end-block event-data)) err-event-ended)
    (asserts! (< (get current-participants event-data) (get max-participants event-data)) err-event-full)
    (asserts! (is-none (map-get? participant-registrations { event-id: event-id, participant: tx-sender })) err-already-exists)
    
    ;; Register participant
    (map-set participant-registrations
      { event-id: event-id, participant: tx-sender }
      {
        registration-block: current-block,
        attended: false,
        screening-completed: false
      }
    )
    
    ;; Update participant count
    (map-set screening-events
      { event-id: event-id }
      (merge event-data { current-participants: (+ (get current-participants event-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (mark-attendance (event-id uint) (participant principal))
  (let (
    (event-data (unwrap! (map-get? screening-events { event-id: event-id }) err-not-found))
    (registration (unwrap! (map-get? participant-registrations { event-id: event-id, participant: participant }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get organizer event-data)) err-unauthorized)
    
    (map-set participant-registrations
      { event-id: event-id, participant: participant }
      (merge registration { attended: true })
    )
    
    (ok true)
  )
)

;; Healthcare provider functions
(define-public (register-provider 
  (name (string-ascii 100))
  (specialization (string-ascii 50))
  (license-number (string-ascii 50))
)
  (begin
    (map-set healthcare-providers
      { provider: tx-sender }
      {
        name: name,
        specialization: specialization,
        license-number: license-number,
        verified: false
      }
    )
    (ok true)
  )
)

(define-public (verify-provider (provider principal))
  (let (
    (provider-data (unwrap! (map-get? healthcare-providers { provider: provider }) err-not-found))
  )
    (asserts! (is-contract-owner tx-sender) err-owner-only)
    
    (map-set healthcare-providers
      { provider: provider }
      (merge provider-data { verified: true })
    )
    
    (ok true)
  )
)

;; Results management
(define-public (record-screening-result
  (event-id uint)
  (participant principal)
  (result-status uint)
  (test-values (string-ascii 200))
  (recommendations (string-ascii 300))
  (followup-required bool)
)
  (let (
    (result-id (var-get next-result-id))
    (provider-data (unwrap! (map-get? healthcare-providers { provider: tx-sender }) err-unauthorized))
    (registration (unwrap! (map-get? participant-registrations { event-id: event-id, participant: participant }) err-not-found))
  )
    (asserts! (get verified provider-data) err-unauthorized)
    (asserts! (get attended registration) err-unauthorized)
    (asserts! (<= result-status result-requires-followup) err-invalid-status)
    
    ;; Record the result
    (map-set screening-results
      { result-id: result-id }
      {
        event-id: event-id,
        participant: participant,
        healthcare-provider: tx-sender,
        result-status: result-status,
        test-values: test-values,
        recommendations: recommendations,
        followup-required: followup-required,
        recorded-at: (get-current-block)
      }
    )
    
    ;; Mark screening as completed
    (map-set participant-registrations
      { event-id: event-id, participant: participant }
      (merge registration { screening-completed: true })
    )
    
    ;; Update population health metrics
    (var-set total-screenings-conducted (+ (var-get total-screenings-conducted) u1))
    (if (is-eq result-status result-abnormal)
      (var-set total-abnormal-results (+ (var-get total-abnormal-results) u1))
      true
    )
    
    (var-set next-result-id (+ result-id u1))
    (ok result-id)
  )
)

;; Follow-up care management
(define-public (schedule-followup
  (result-id uint)
  (scheduled-block uint)
  (notes (string-ascii 200))
)
  (let (
    (followup-id (var-get next-followup-id))
    (result-data (unwrap! (map-get? screening-results { result-id: result-id }) err-not-found))
  )
    (asserts! (get followup-required result-data) err-invalid-status)
    (asserts! (or (is-eq tx-sender (get participant result-data)) (is-eq tx-sender (get healthcare-provider result-data))) err-unauthorized)
    
    (map-set followup-care
      { followup-id: followup-id }
      {
        result-id: result-id,
        participant: (get participant result-data),
        provider: (get healthcare-provider result-data),
        scheduled-block: scheduled-block,
        completed: false,
        notes: notes
      }
    )
    
    (var-set next-followup-id (+ followup-id u1))
    (ok followup-id)
  )
)

(define-public (complete-followup (followup-id uint) (completion-notes (string-ascii 200)))
  (let (
    (followup-data (unwrap! (map-get? followup-care { followup-id: followup-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get provider followup-data)) err-unauthorized)
    (asserts! (not (get completed followup-data)) err-invalid-status)
    
    (map-set followup-care
      { followup-id: followup-id }
      (merge followup-data { completed: true, notes: completion-notes })
    )
    
    (var-set total-followups-completed (+ (var-get total-followups-completed) u1))
    (ok true)
  )
)

;; Read-only functions for data retrieval
(define-read-only (get-screening-event (event-id uint))
  (map-get? screening-events { event-id: event-id })
)

(define-read-only (get-participant-registration (event-id uint) (participant principal))
  (map-get? participant-registrations { event-id: event-id, participant: participant })
)

(define-read-only (get-screening-result (result-id uint))
  (map-get? screening-results { result-id: result-id })
)

(define-read-only (get-followup-care (followup-id uint))
  (map-get? followup-care { followup-id: followup-id })
)

(define-read-only (get-healthcare-provider (provider principal))
  (map-get? healthcare-providers { provider: provider })
)

(define-read-only (get-population-health-stats)
  {
    total-screenings: (var-get total-screenings-conducted),
    total-abnormal-results: (var-get total-abnormal-results),
    total-followups-completed: (var-get total-followups-completed),
    abnormal-rate: (if (> (var-get total-screenings-conducted) u0)
                    (/ (* (var-get total-abnormal-results) u100) (var-get total-screenings-conducted))
                    u0),
    followup-completion-rate: (if (> (var-get total-abnormal-results) u0)
                              (/ (* (var-get total-followups-completed) u100) (var-get total-abnormal-results))
                              u0)
  }
)

;; Admin functions
(define-public (update-event-status (event-id uint) (new-status uint))
  (let (
    (event-data (unwrap! (map-get? screening-events { event-id: event-id }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get organizer event-data)) err-unauthorized)
    (asserts! (<= new-status status-cancelled) err-invalid-status)
    
    (map-set screening-events
      { event-id: event-id }
      (merge event-data { status: new-status })
    )
    
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-contract-owner tx-sender) err-owner-only)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

