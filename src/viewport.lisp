(in-package #:ndjinn)

;;; spec

(defstruct (viewport-spec
            (:constructor %make-viewport-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (x 0f0 :type u:f32)
  (y 0f0 :type u:f32)
  (width 0f0 :type u:f32)
  (height 0f0 :type u:f32))

(u:define-printer (viewport-spec stream)
  (format stream "~s" (viewport-spec-name viewport-spec)))

(defun update-viewport-spec (name x y width height)
  (let ((spec (u:href =meta/viewports= name)))
    (setf (viewport-spec-x spec) (u:clamp (float x 1f0) 0f0 1f0)
          (viewport-spec-y spec) (u:clamp (float y 1f0) 0f0 1f0)
          (viewport-spec-width spec) (u:clamp (float width 1f0) 0f0 1f0)
          (viewport-spec-height spec) (u:clamp (float height 1f0) 0f0 1f0))
    (enqueue :recompile (list :viewport name))))

(defun make-viewport-spec (name x y width height)
  (let ((spec (%make-viewport-spec :name name)))
    (setf (u:href =meta/viewports= name) spec)
    (update-viewport-spec name x y width height)
    spec))

(defmacro define-viewport (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key (x 0) (y 0) (width 1) (height 1)) (car body)
    `(if (u:href =meta/viewports= ',name)
         (update-viewport-spec ',name ,x ,y ,width ,height)
         (make-viewport-spec ',name ,x ,y ,width ,height))))

;;; implementation

(defstruct (viewport-manager
            (:predicate nil)
            (:copier nil))
  (table (u:dict #'eq) :type hash-table)
  (order nil :type list)
  (active nil :type (or viewport null))
  (default nil :type (or viewport null)))

(defstruct (viewport
            (:constructor %make-viewport)
            (:predicate nil)
            (:copier nil))
  (spec nil :type (or viewport-spec null))
  camera
  (draw-order nil :type (or avl:tree null))
  picker
  (x 0f0 :type u:f32)
  (y 0f0 :type u:f32)
  (width 0f0 :type u:f32)
  (height 0f0 :type u:f32))

(u:define-printer (viewport stream)
  (format stream "~s" (viewport-spec-name (viewport-spec viewport))))

(defun update-viewport (viewport)
  (v2:with-components ((r (window-size)))
    (let* ((spec (viewport-spec viewport))
           (x (u:lerp (viewport-spec-x spec) 0 rx))
           (y (u:lerp (viewport-spec-y spec) 0 ry))
           (width (u:lerp (viewport-spec-width spec) 0 rx))
           (height (u:lerp (viewport-spec-height spec) 0 ry)))
      (setf (viewport-x viewport) x
            (viewport-y viewport) y
            (viewport-width viewport) width
            (viewport-height viewport) height))))

(defun update-all-viewports ()
  (u:do-hash-values (v (viewport-manager-table (get-viewport-manager)))
    (update-viewport v)))

(defun configure-viewport (viewport)
  (gl:viewport (viewport-x viewport)
               (viewport-y viewport)
               (viewport-width viewport)
               (viewport-height viewport)))

(defun make-viewport (name order picker)
  (let* ((spec (u:href =meta/viewports= name))
         (viewport (%make-viewport :spec spec
                                   :draw-order order
                                   :picker picker)))
    (update-viewport viewport)
    viewport))

(defun get-viewport-manager ()
  (scene-viewports (current-scene =context=)))

(defun get-viewport-by-coordinates (x y)
  (let ((manager (get-viewport-manager)))
    (u:do-hash-values (v (viewport-manager-table manager))
      (let ((vx (viewport-x v))
            (vy (viewport-y v))
            (vw (viewport-width v))
            (vh (viewport-height v)))
        (when (and (<= vx x (+ vx vw))
                   (<= vy y (+ vy vh)))
          (return-from get-viewport-by-coordinates v))))
    (viewport-manager-default manager)))

(defun get-entity-viewports (entity)
  (let ((manager (get-viewport-manager))
        (viewports nil))
    (dolist (id (id/views entity))
      (let ((viewport (u:href (viewport-manager-table manager) id)))
        (pushnew viewport viewports)))
    (or viewports (list (viewport-manager-default manager)))))

(defun get-viewport-size (&optional viewport-name)
  (let* ((manager (get-viewport-manager))
         (viewport (or (u:href (viewport-manager-table manager) viewport-name)
                       (viewport-manager-active manager)
                       (viewport-manager-default manager))))
    (v2:vec (viewport-width viewport)
            (viewport-height viewport))))

(defun get-viewport-camera ()
  (u:when-let ((viewport (viewport-manager-active (get-viewport-manager))))
    (viewport-camera viewport)))

(on-recompile :viewport data ()
  (recompile :scene (get-scene-name))
  (log:debug :ndjinn "Recompiled viewport: ~s" data))
