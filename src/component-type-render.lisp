(in-package #:ndjinn)

(define-component render ()
  ((%render/materials :accessor render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform 'default)
   (%render/current-material :accessor render/current-material
                             :initform nil))
  (:order :after transform :before sprite))

(defun register-material (entity)
  (let ((materials (u:dict #'eq)))
    (dolist (spec-name (render/materials entity))
      (let* ((material (make-material spec-name))
             (pass (material-spec-pass (material-spec material))))
        (setf (u:href materials pass) material)))
    materials))

(defun render-entity (entity)
  (with-debug-group (format nil "Entity: ~a" (id/display entity))
    (let ((material (render/current-material entity)))
      (funcall (material-spec-render-func (material-spec material)) entity))))

(defun render-viewport (viewport pass)
  (configure-viewport viewport)
  (avl:walk
   (viewport-draw-order viewport)
   (lambda (x)
     (u:when-let ((material (u:href (render/materials x)
                                    (render-pass-name pass))))
       (setf (render/current-material x) material)
       (render-entity x)))))

(defun render-pass (pass)
  (with-debug-group (format nil "Render Pass: ~s" (render-pass-name pass))
    (let ((viewport-manager (get-viewport-manager)))
      (clear-render-pass pass)
      (dolist (viewport-name (viewport-manager-order viewport-manager))
        (let ((viewport (u:href (viewport-manager-table viewport-manager)
                                viewport-name)))
          (setf (viewport-manager-active viewport-manager) viewport)
          (render-viewport viewport pass))))))

(defun render-frame ()
  (map nil
       (lambda (x) (render-pass (find-render-pass x)))
       (scene-passes (current-scene =context=))))

(defun generate-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode
                         line-width point-size)
      features
    (u:with-gensyms (entity material)
      (let ((enable (set-difference enable +enabled-capabilities+))
            (disable (set-difference disable +disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode +blend-mode+))
                             blend-mode))
            (depth-mode (and (not (equal depth-mode +depth-mode+))
                             depth-mode))
            (polygon-mode (and (not (equal polygon-mode +polygon-mode+))
                               polygon-mode)))
        `(lambda (,entity)
           (let ((,material (render/current-material ,entity)))
             (with-framebuffer (material-framebuffer ,material)
                 (:attachments (material-attachments ,material))
               (shadow:with-shader (material-spec-shader
                                    (material-spec ,material))
                 ,@(when enable
                     `((gl:enable ,@enable)))
                 ,@(when disable
                     `((gl:disable ,@disable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@blend-mode)))
                 ,@(when depth-mode
                     `((gl:depth-func ,depth-mode)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode :front-and-back ,polygon-mode)))
                 ,@(when line-width
                     `((gl:line-width ,line-width)))
                 ,@(when point-size
                     `((gl:point-size ,point-size)))
                 (on-entity-pre-render ,entity)
                 (u:do-hash-values (v (material-uniforms ,material))
                   (resolve-uniform-func ,entity v))
                 (on-entity-render ,entity)
                 (setf (material-texture-unit-state ,material) 0)
                 ,@(when disable
                     `((gl:enable ,@disable)))
                 ,@(when enable
                     `((gl:disable ,@enable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@+blend-mode+)))
                 ,@(when depth-mode
                     `((gl:depth-func ,+depth-mode+)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode ,@+polygon-mode+)))
                 ,@(when line-width
                     `((gl:line-width 1f0)))
                 ,@(when point-size
                     `((gl:point-size 1f0)))))))))))

;;; entity hooks

(define-entity-hook :attach (entity render)
  (setf (render/materials entity) (register-material entity)))

(define-entity-hook :detach (entity render)
  (u:do-hash-values (viewport (viewport-manager-table (get-viewport-manager)))
    (deregister-render-order viewport entity)))

(define-entity-hook :pre-render (entity render)
  (u:when-let ((camera (get-current-camera)))
    (when (has-component-p camera 'camera)
      (set-uniforms entity
                    :view (camera/view camera)
                    :proj (camera/projection camera)))))

(define-entity-hook :enable (entity render)
  (dolist (viewport (get-entity-viewports entity))
    (register-render-order viewport entity)))

(define-entity-hook :disable (entity render)
  (dolist (viewport (get-entity-viewports entity))
    (deregister-render-order viewport entity)))
