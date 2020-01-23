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
  (set-uniforms entity :opacity (- 1 (progress state))))

(define-animation-state-hook fade entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'fade :name 'fade/reverse)))

(define-animation-state-hook fade/reverse entity state :update
  (set-uniforms entity :opacity (progress state)))

(define-animation-state-hook fade/reverse entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'fade :name 'fade)))
