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

(defclass mouse-control/rotation-state ()
  ((%dragging :accessor dragging
              :initform nil)
   (%drag-start :accessor drag-start
                :initform (v2:vec))
   (%drag-end :accessor drag-end
              :initform (v2:vec))
   (%initial :accessor initial
             :initarg :initial)
   (%rotation-vector :accessor rotation-vector
                     :initform nil)
   (%speed :accessor rotation-speed
           :initarg :speed)))

(define-entity-hook :attach (entity mouse-control)
  (let* ((buttons (remove nil (map 'list #'identity +mouse-button-names+)))
         (initial-rotation (q:copy (get-rotation entity)))
         (rotation-speed (mouse-control/rotation-speed entity))
         (rotation-state (make-instance 'mouse-control/rotation-state
                                        :initial initial-rotation
                                        :speed rotation-speed)))
    (unless (member (mouse-control/rotate-button entity) buttons)
      (error "Mouse rotation button must be one of: ~{~s~^, ~}" buttons))
    (setf (mouse-control/rotation entity) rotation-state)))

(define-entity-hook :update (entity mouse-control)
  (when (entity-picked-p entity)
    (with-slots (%rotation-vector %dragging) (mouse-control/rotation entity)
      (u:mvlet* ((button (mouse-control/rotate-button entity))
                 (drag-started (on-button-enter :mouse button))
                 (drag-stopped (on-button-exit :mouse button)))
        (when drag-started
          (mouse-control/start-rotation entity))
        (when %dragging
          (mouse-control/rotate entity))
        (when (and drag-stopped %rotation-vector)
          (mouse-control/finish-rotation entity))))))

(defun mouse-control/start-rotation (entity)
  (u:mvlet ((state (mouse-control/rotation entity))
            (x y (get-mouse-position)))
    (with-slots (%rotation-vector %drag-start %dragging) state
      (unless %rotation-vector
        (setf %rotation-vector (v2:vec)))
      (setf %drag-start (v2:vec x y)
            %dragging t))))

(defun mouse-control/rotate (entity)
  (u:mvlet ((state (mouse-control/rotation entity))
            (x y (get-mouse-position)))
    (with-slots (%speed %initial %rotation-vector %drag-start) state
      (v2:with-components ((r %rotation-vector)
                           (v (v2:- (v2:vec x y) %drag-start)))
        (rotate-entity entity
                       (q:rotate (q:orient :local
                                           :y (+ rx (* vx %speed))
                                           :x (- (+ ry (* vy %speed))))
                                 %initial)
                       :replace t)))))

(defun mouse-control/finish-rotation (entity)
  (u:mvlet ((state (mouse-control/rotation entity))
            (x y (get-mouse-position)))
    (with-slots (%rotation-vector %dragging %drag-start %drag-end %speed) state
      (setf %dragging nil
            %drag-end (v2:vec x y))
      (v2:+! %rotation-vector
             %rotation-vector
             (v2:scale (v2:- %drag-end %drag-start) %speed)))))
