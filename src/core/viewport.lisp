(in-package #:net.mfiano.lisp.pyx)

;;; spec

(defclass viewport-spec ()
  ((%name :reader name
          :initarg :name)
   (%x :accessor x)
   (%y :accessor y)
   (%width :accessor width)
   (%height :accessor height)))

(u:define-printer (viewport-spec stream :identity t)
  (format stream "~s" (name viewport-spec)))

(defun update-viewport-spec (name x y width height)
  (let ((spec (u:href (metadata-viewports =metadata=) name)))
    (setf (x spec) (u:clamp (float x 1f0) 0f0 1f0)
          (y spec) (u:clamp (float y 1f0) 0f0 1f0)
          (width spec) (u:clamp (float width 1f0) 0f0 1f0)
          (height spec) (u:clamp (float height 1f0) 0f0 1f0))
    (enqueue :recompile (list :viewport name))))

(defun make-viewport-spec (name x y width height)
  (let ((spec (make-instance 'viewport-spec :name name)))
    (setf (u:href (metadata-viewports =metadata=) name) spec)
    (update-viewport-spec name x y width height)
    spec))

(defmacro define-viewport (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1)) (car body)
    `(if (u:href (metadata-viewports =metadata=) ',name)
         (update-viewport-spec ',name ,x ,y ,width ,height)
         (make-viewport-spec ',name ,x ,y ,width ,height))))

(define-viewport default ()
  (:x 0 :y 0 :width 1 :height 1))

;;; implementation

(defclass viewport-manager ()
  ((%table :reader table
           :initform (u:dict #'eq))
   (%order :accessor order
           :initform nil)
   (%active :accessor active
            :initform nil)
   (%default :accessor default)))

(defclass viewport ()
  ((%spec :reader spec
          :initarg :spec)
   (%camera :accessor camera
            :initform nil)
   (%draw-order :reader draw-order
                :initarg :draw-order)
   (%picker :reader picker
            :initarg :picker)
   (%x :accessor x
       :initform 0)
   (%y :accessor y
       :initform 0)
   (%width :accessor width
           :initform 0)
   (%height :accessor height
            :initform 0)))

(u:define-printer (viewport stream :identity t)
  (format stream "~s" (name (spec viewport))))

(defun make-viewport (name order picker)
  (let* ((spec (u:href (metadata-viewports =metadata=) name))
         (viewport (make-instance 'viewport
                                  :spec spec
                                  :draw-order order
                                  :picker picker)))
    (configure-viewport viewport)
    viewport))

(defun get-viewport-manager ()
  (viewports (current-scene =context=)))

(defun get-viewport-by-coordinates (x y)
  (let ((manager (get-viewport-manager)))
    (u:do-hash-values (v (table manager))
      (when (and (<= (x v) x (+ (x v) (width v)))
                 (<= (y v) y (+ (y v) (height v))))
        (return-from get-viewport-by-coordinates v)))
    (default manager)))

(defun configure-viewport (viewport)
  (let ((spec (spec viewport)))
    (v2:with-components ((r (window-size)))
      (setf (x viewport) (u:lerp (x spec) 0 rx)
            (y viewport) (u:lerp (y spec) 0 ry)
            (width viewport) (u:lerp (width spec) 0 rx)
            (height viewport) (u:lerp (height spec) 0 ry))))
  (gl:viewport (x viewport)
               (y viewport)
               (width viewport)
               (height viewport)))

(defun get-entity-viewports (entity)
  (let ((scene-viewports (get-viewport-manager))
        (viewports nil))
    (dolist (id (id/views entity))
      (let ((viewport (u:href (table scene-viewports) id)))
        (pushnew viewport viewports)))
    (or viewports (list (default scene-viewports)))))

(on-recompile :viewport data ()
  (recompile :scene (get-scene-name))
  (log:debug :pyx.live "Recompiled viewport: ~s" data))

(defun get-viewport-dimensions (&optional viewport-name)
  (let* ((manager (get-viewport-manager))
         (viewport (or (u:href (table manager) viewport-name)
                       (active manager)
                       (default manager))))
    (v2:vec (width viewport) (height viewport))))
