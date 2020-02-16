(in-package #:pyx)

;;; sprite

(define-animation-state animate/sprite ())

(define-animation-state-hook animate/sprite entity state :update
  (let* ((min (comp::sprite/initial-index entity))
         (max (1- (+ min (comp::sprite/frames entity))))
         (index (floor (a:clamp (a:lerp (progress state) min max) min max))))
    (setf (comp::sprite/index entity) index)))

;;; fade

(define-animation-state animate/opacity ())

(define-animation-state-hook animate/opacity entity state :update
  (set-uniforms entity :opacity (- 1 (progress state))))

(define-animation-state-hook animate/opacity entity state :finish
  (when (repeat-p state)
    (replace-animation-state state
                             'animate/opacity
                             :name 'animate/opacity/reverse)))

(define-animation-state-hook animate/opacity/reverse entity state :update
  (set-uniforms entity :opacity (progress state)))

(define-animation-state-hook animate/opacity/reverse entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'opacity :name 'animate/opacity)))
