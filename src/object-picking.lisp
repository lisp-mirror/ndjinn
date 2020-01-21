(in-package #:pyx)

(defclass picking-ray ()
  ((%start :reader start
           :initform (v3:zero))
   (%end :reader end
         :initform (v3:zero))
   (%visualize :reader visualize)))

(defun update-picking-ray ()
  (u:mvlet* ((x y dx dy viewport (get-mouse-position))
             (camera (camera viewport))
             (view (camera/view camera))
             (proj (camera/projection camera)))
    (with-slots (%x %y %width %height %picking-ray) viewport
      (with-slots (%start %end) %picking-ray
        (let ((viewport (v4:vec %x %y %width %height)))
          (math:unproject! %start (v3:vec x y 0f0) view proj viewport)
          (math:unproject! %end (v3:vec x y 1f0) view proj viewport)
          (values %start %end))))))

(defgeneric pick-shape (ray shape)
  (:method (ray shape)))

(defmethod pick-shape (ray (shape shape/sphere))
  (with-slots (%start %end) ray
    (with-slots (%entity %center %radius) shape
      (let* ((line (v3:- %end %start))
             (direction (v3:normalize line))
             (m (v3:- %start (transform-point %entity %center)))
             (b (v3:dot m direction))
             (c (- (v3:dot m m) (expt %radius 2))))
        (unless (and (plusp c) (plusp b))
          (let ((discriminant (- (expt b 2) c)))
            (unless (minusp discriminant)
              (let ((x (max 0f0 (- (- b) (sqrt discriminant)))))
                (when (<= x (v3:length line))
                  x)))))))))

(defun pick-entity ()
  (let* ((viewport (get-viewport))
         (ray (picking-ray viewport))
         (object-tree (draw-order viewport))
         (picked nil))
    (update-picking-ray)
    (avl-tree/walk
     object-tree
     (lambda (x)
       (when (and (has-component-p x 'collider))
         (a:when-let ((n (pick-shape ray (collider/shape x))))
           (push (cons n x) picked)))))
    (when picked
      (let ((entity (cdar (stable-sort picked #'< :key #'car))))
        (on-collision-picked (collider/target entity) nil entity)))))
