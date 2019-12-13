(in-package #:pyx)

;;; sprite

(define-animation-state sprite ())

(define-animation-state-hook sprite entity state :update
  (with-slots (%sprite/initial-index %sprite/index %sprite/frames) entity
    (let* ((min %sprite/initial-index)
           (max (1- (+ min %sprite/frames)))
           (index (floor
                   (a:clamp (u:map-domain 0 1 min max (progress state))
                            min
                            max))))
      (setf %sprite/index index))))

;;; fade

(define-animation-state fade ())

(define-animation-state-hook fade entity state :update
  (setf (u:href (uniforms (render/current-material entity)) :opacity)
        (- 1 (progress state))))

(define-animation-state-hook fade entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'fade :name 'fade/reverse)))

(define-animation-state-hook fade/reverse entity state :update
  (setf (u:href (uniforms (render/current-material entity)) :opacity) (progress state)))

(define-animation-state-hook fade/reverse entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'fade :name 'fade)))

;;; rotate

(define-animation-state rotate ()
  (:angle (* pi 2)
   :axis (v3:vec 0f0 0f0 1f0)))

(define-animation-state-hook rotate entity state :update
  (with-slots (%angle %axis %progress) state
    (let ((step (float (u:map-domain 0 1 0 %angle %progress) 1f0))
          (axis (v3:normalize %axis)))
      (rotate-entity entity (v3:scale axis step) t))))

(define-animation-state-hook rotate entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'rotate :name 'rotate/reverse)))

(define-animation-state-hook rotate/reverse entity state :update
  (with-slots (%angle %axis %progress) state
    (let ((step (float (- %angle (u:map-domain 0 1 0 %angle %progress)) 1f0))
          (axis (v3:normalize %axis)))
      (rotate-entity entity (v3:scale axis step) t))))

(define-animation-state-hook rotate/reverse entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'rotate :name 'rotate)))
