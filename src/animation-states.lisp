(in-package #:%pyx.animation)

;;; sprite

(define-animation-state sprite ())

(define-animation-state-hook sprite entity state :update
  (let* ((min (comp::sprite/initial-index entity))
         (max (1- (+ min (comp::sprite/frames entity))))
         (index (floor (a:clamp (a:lerp (progress state) min max) min max))))
    (setf (comp::sprite/index entity) index)))

;;; fade

(define-animation-state opacity ())

(define-animation-state-hook opacity entity state :update
  (mat:set-uniforms entity :opacity (- 1 (progress state))))

(define-animation-state-hook opacity entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'opacity :name 'opacity/reverse)))

(define-animation-state-hook opacity/reverse entity state :update
  (mat:set-uniforms entity :opacity (progress state)))

(define-animation-state-hook opacity/reverse entity state :finish
  (when (repeat-p state)
    (replace-animation-state state 'opacity :name 'opacity)))
