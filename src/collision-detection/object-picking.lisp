(in-package #:net.mfiano.lisp.pyx)

(defclass picking-ray ()
  ((%start :reader start
           :initform (v3:vec))
   (%end :reader end
         :initform (v3:vec))))

(defun make-picking-ray ()
  (make-instance 'picking-ray))

(defgeneric pick-collider-shape (ray shape)
  (:method (ray shape)))

(defmethod pick-collider-shape (ray (shape collider-shape/sphere))
  (with-slots (%entity %center %radius) shape
    (let* ((line (v3:- (end ray) (start ray)))
           (direction (v3:normalize line))
           (m (v3:- (start ray) (transform-point %entity %center)))
           (b (v3:dot m direction))
           (c (- (v3:dot m m) (expt %radius 2))))
      (unless (and (plusp c) (plusp b))
        (let ((discriminant (- (expt b 2) c)))
          (unless (minusp discriminant)
            (let ((x (max 0 (- (- b) (sqrt discriminant)))))
              (when (<= x (v3:length line))
                x))))))))

(defun update-picking-ray ()
  (u:mvlet ((x y dx dy (get-mouse-position)))
    (u:when-let* ((viewport (get-viewport-by-coordinates x y))
                  (ray (picking-ray viewport))
                  (camera (camera viewport))
                  (view (camera/view camera))
                  (proj (camera/projection camera))
                  (viewport (v4:vec (x viewport)
                                    (y viewport)
                                    (width viewport)
                                    (height viewport))))
      (math:unproject! (start ray) (v3:vec x y) view proj viewport)
      (math:unproject! (end ray) (v3:vec x y 1) view proj viewport)
      (values (start ray) (end ray)))))

(defun pick-entity ()
  (let* ((viewport (active (get-viewport-manager)))
         (ray (picking-ray viewport))
         (picked nil))
    (update-picking-ray)
    (u:do-hash-values (v (active (collision-system (current-scene))))
      (u:do-hash-keys (k v)
        (u:when-let ((n (pick-collider-shape ray (collider/shape k))))
          (push (cons n k) picked))))
    (when picked
      (let ((entity (cdar (stable-sort picked #'< :key #'car))))
        (setf (picked-entity (current-scene)) entity)
        (on-collision-picked (collider/layer entity) entity)))))

(defun entity-picked-p (entity)
  (eq entity (picked-entity (current-scene))))
