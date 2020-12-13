(in-package #:ndjinn)

(define-component mouse-control ()
  ((%mouse-control/rotation-speed :accessor mouse-control/rotation-speed
                                  :initarg :mouse-control/rotation-speed
                                  :initform 0.005f0)
   (%mouse-control/rotate-button :reader mouse-control/rotate-button
                                 :initarg :mouse-control/rotate-button
                                 :initform :left)
   (%mouse-control/rotation :accessor mouse-control/rotation
                            :initform nil))
  (:order :before render :after transform))

(defstruct (mouse-control/rotation-state
            (:predicate nil)
            (:copier nil))
  (dragging nil :type boolean)
  (drag-start (v2:vec) :type v2:vec)
  (drag-end (v2:vec) :type v2:vec)
  (initial (q:quat 1) :type q:quat)
  (rotation-vector (v2:vec) :type v2:vec)
  (speed 0.005 :type single-float))

(defun mouse-control/start-rotation (entity)
  (u:mvlet ((state (mouse-control/rotation entity))
            (x y (get-mouse-position)))
    (setf (mouse-control/rotation-state-drag-start state) (v2:vec x y)
          (mouse-control/rotation-state-dragging state) t)))

(defun mouse-control/rotate (entity)
  (u:mvlet* ((state (mouse-control/rotation entity))
             (x y (get-mouse-position))
             (vector (mouse-control/rotation-state-rotation-vector state))
             (speed  (mouse-control/rotation-state-speed state))
             (start (mouse-control/rotation-state-drag-start state)))
    (v2:with-components ((r vector)
                         (v (v2:- (v2:vec x y) start)))
      (rotate-entity entity
                     (q:rotate (q:orient :local
                                         :y (+ rx (* vx speed))
                                         :x (- (+ ry (* vy speed))))
                               (mouse-control/rotation-state-initial state))
                     :replace t))))

(defun mouse-control/finish-rotation (entity)
  (u:mvlet* ((state (mouse-control/rotation entity))
             (x y (get-mouse-position))
             (vector (mouse-control/rotation-state-rotation-vector state))
             (start (mouse-control/rotation-state-drag-start state))
             (end (v2:vec x y))
             (speed (mouse-control/rotation-state-speed state)))
    (setf (mouse-control/rotation-state-dragging state) nil
          (mouse-control/rotation-state-drag-end state) end)
    (v2:+! vector vector (v2:scale (v2:- end start) speed))))

(define-entity-hook :attach (entity mouse-control)
  (let* ((buttons (remove nil (map 'list #'identity +mouse-button-names+)))
         (initial-rotation (q:copy (get-rotation entity)))
         (rotation-speed (mouse-control/rotation-speed entity))
         (rotation-state (make-mouse-control/rotation-state
                          :initial initial-rotation
                          :speed rotation-speed)))
    (unless (member (mouse-control/rotate-button entity) buttons)
      (error "Mouse rotation button must be one of: ~{~s~^, ~}" buttons))
    (setf (mouse-control/rotation entity) rotation-state)))

(define-entity-hook :update (entity mouse-control)
  (let* ((state (mouse-control/rotation entity))
         (dragging (mouse-control/rotation-state-dragging state))
         (button (mouse-control/rotate-button entity))
         (drag-started (on-button-enter :mouse button))
         (drag-stopped (on-button-exit :mouse button)))
    (cond
      (drag-started
       (pick-entity))
      (drag-stopped
       (unpick-entity)))
    (when (or (entity-picked-p entity) dragging)
      (when drag-started
        (mouse-control/start-rotation entity))
      (when dragging
        (mouse-control/rotate entity))
      (when (and drag-stopped
                 (mouse-control/rotation-state-rotation-vector state))
        (mouse-control/finish-rotation entity)))))
