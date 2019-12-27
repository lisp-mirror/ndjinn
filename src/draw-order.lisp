(in-package #:pyx)

(defun draw-order-comparator (x y)
  (let* ((draw-order (draw-order (pipeline (spec (current-scene *state*)))))
         (x (u:href draw-order x))
         (y (u:href draw-order y)))
    (and (integerp x)
         (integerp y)
         (< x y))))

(defun sort-draw-order (scene pass)
  (symbol-macrolet ((order (u:href (draw-order scene) pass)))
    (setf order (stable-sort
                 (copy-seq order)
                 #'draw-order-comparator
                 :key (lambda (x) (render/order (car x)))))))

(defun register-draw-order (entity)
  (dolist (material (render/materials entity))
    (let ((scene (current-scene *state*))
          (pass (pass (spec material))))
      (symbol-macrolet ((order (u:href (draw-order scene) pass)))
        (unless (find entity order :key #'car)
          (push (cons entity material) order))
        (pushnew pass (render/passes entity))
        (sort-draw-order scene pass)))))

(defun deregister-draw-order (entity)
  (let ((order (draw-order (current-scene *state*))))
    (dolist (pass (render/passes entity))
      (a:deletef (u:href order pass) entity :key #'car)
      (unless (u:href order pass)
        (remhash pass order)))))
