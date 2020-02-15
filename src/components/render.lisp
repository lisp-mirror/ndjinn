(in-package #:%pyx.component)

(ent:define-component render ()
  ((%render/materials :accessor render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform :default)
   (%render/current-material :accessor render/current-material
                             :initform nil))
  (:sorting :after transform :before sprite))

(defun register-material (entity)
  (let ((materials (u:dict #'eq)))
    (dolist (spec-name (render/materials entity))
      (let ((material (mat:make-material entity spec-name)))
        (setf (u:href materials (mat:pass (mat:spec material))) material)))
    materials))

(defun render-frame ()
  (let ((scene (ctx:current-scene)))
    (map nil
         (lambda (x) (render-pass (render:find-pass-spec x)))
         (scene:passes scene))))

(defun render-pass (pass)
  (ogl:with-debug-group (format nil "Render Pass: ~s" pass)
    (let ((viewport-manager (vp:get-manager)))
      (render:clear-pass pass)
      (u:do-hash-values (viewport (vp:table viewport-manager))
        (setf (vp:active viewport-manager) viewport)
        (render-viewport viewport pass)))))

(defun render-viewport (viewport pass)
  (vp:configure viewport)
  (util::avl-walk
   (vp:draw-order viewport)
   (lambda (x)
     (a:when-let ((material (u:href (render/materials x) (render:name pass))))
       (setf (render/current-material x) material)
       (render-entity x)))))

(defun render-entity (entity)
  (ogl:with-debug-group (format nil "Entity: ~a" (id/display entity))
    (let ((material (render/current-material entity)))
      (funcall (mat:render-func (mat:spec material)) entity))))

(defun generate-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode
                         line-width point-size)
      features
    (a:with-gensyms (entity material)
      (let ((enable (set-difference enable ogl:+enabled+))
            (disable (set-difference disable ogl:+disabled+))
            (blend-mode (and (not (equal blend-mode ogl:+blend-mode+))
                             blend-mode))
            (depth-mode (and (not (equal depth-mode ogl:+depth-mode+))
                             depth-mode))
            (polygon-mode (and (not (equal polygon-mode ogl:+polygon-mode+))
                               polygon-mode)))
        `(lambda (,entity)
           (let ((,material (render/current-material ,entity)))
             (fb:with-framebuffer (mat:framebuffer ,material)
                 (:attachments (mat:attachments ,material))
               (shadow:with-shader (mat:shader (mat:spec ,material))
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
                 (ent:on-pre-render ,entity)
                 (u:do-hash-values (v (mat:uniforms ,material))
                   (mat:resolve-uniform-func ,entity v))
                 (ent:on-render ,entity)
                 (setf (mat:texture-unit-state ,material) 0)
                 ,@(when disable
                     `((gl:enable ,@disable)))
                 ,@(when enable
                     `((gl:disable ,@enable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@ogl:+blend-mode+)))
                 ,@(when depth-mode
                     `((gl:depth-func ,ogl:+depth-mode+)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode ,@ogl:+polygon-mode+)))
                 ,@(when line-width
                     `((gl:line-width 1f0)))
                 ,@(when point-size
                     `((gl:point-size 1f0)))))))))))

;;; entity hooks

(ent:define-entity-hook :attach (entity render)
  (setf render/materials (register-material entity)))

(ent:define-entity-hook :detach (entity render)
  (u:do-hash-values (viewport (vp:table (vp:get-manager)))
    (render:deregister-order viewport entity)))

(ent:define-entity-hook :pre-render (entity render)
  (a:when-let ((camera (vp:camera (vp:active (vp:get-manager)))))
    (when (ent:has-component-p camera 'camera)
      (mat:set-uniforms entity
                        :view (camera/view camera)
                        :proj (camera/projection camera)))))
