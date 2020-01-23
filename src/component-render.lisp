(in-package #:pyx)

(define-component render ()
  ((%render/materials :accessor render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform 'default)
   (%render/current-material :accessor render/current-material
                             :initform nil))
  (:sorting :after xform :before sprite))

(defun clear-render-pass (pass-name)
  (let ((pass (u:href (pass-table (spec (get-scene))) pass-name)))
    (with-framebuffer (find-framebuffer (framebuffer pass)) ()
      (v4:with-components ((v (clear-color pass)))
        (gl:clear-color vx vy vz vw)
        (apply #'gl:clear (clear-buffers pass))))))

(defun render-frame ()
  (let ((scene-spec (spec (get-scene))))
    (map nil #'render-pass (pass-order scene-spec))))

(defun render-pass (pass)
  (with-debug-group (format nil "Render Pass: ~s" pass)
    (a:when-let* ((scene (get-scene))
                  (viewports (viewports scene)))
      (clear-render-pass pass)
      (u:do-hash-values (viewport (table viewports))
        (setf (active viewports) viewport)
        (configure-viewport viewport)
        (avl-tree/walk
         (draw-order viewport)
         (lambda (x)
           (a:when-let ((material (u:href (render/materials x) pass)))
             (setf (render/current-material x) material)
             (render-entity x))))))))

(defun render-entity (entity)
  (let ((material (render/current-material entity)))
    (with-debug-group (format nil "Entity: ~a" (id/display entity))
      (funcall (render-func (spec material)) entity))))

;;; entity hooks

(define-hook :pre-render (entity render)
  (a:when-let ((camera (camera (get-viewport))))
    (when (has-component-p camera 'camera)
      (set-uniforms entity
                    :view (camera/view camera)
                    :proj (camera/projection camera)))))

(define-hook :attach (entity render)
  (setf render/materials (register-materials entity)))

(define-hook :detach (entity render)
  (u:do-hash-values (viewport (table (viewports (get-scene))))
    (deregister-draw-order viewport entity)))
