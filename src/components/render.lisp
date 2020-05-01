(in-package #:pyx.component)

(pyx:define-component render ()
  ((%render/materials :accessor render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform 'pyx::default)
   (%render/current-material :accessor render/current-material
                             :initform nil))
  (:sorting :after transform :before sprite))

(defun register-material (entity)
  (let ((materials (u:dict #'eq)))
    (dolist (spec-name (render/materials entity))
      (let ((material (pyx::make-material entity spec-name)))
        (setf (u:href materials (pyx::pass (pyx::spec material))) material)))
    materials))

(defun render-frame ()
  (let ((scene (pyx::current-scene)))
    (map nil
         (lambda (x) (render-pass (pyx::find-render-pass-spec x)))
         (pyx::passes scene))))

(defun render-pass (pass)
  (pyx::with-debug-group (format nil "Render Pass: ~s" pass)
    (let ((viewport-manager (pyx::get-viewport-manager)))
      (pyx::clear-render-pass pass)
      (u:do-hash-values (viewport (pyx::table viewport-manager))
        (setf (pyx::active viewport-manager) viewport)
        (render-viewport viewport pass)))))

(defun render-viewport (viewport pass)
  (pyx::configure-viewport viewport)
  (avl:walk
   (pyx::draw-order viewport)
   (lambda (x)
     (a:when-let ((material (u:href (render/materials x) (pyx::name pass))))
       (setf (render/current-material x) material)
       (render-entity x)))))

(defun render-entity (entity)
  (pyx::with-debug-group (format nil "Entity: ~a" (id/display entity))
    (let ((material (render/current-material entity)))
      (funcall (pyx::render-func (pyx::spec material)) entity))))

(defun generate-render-func (features)
  (destructuring-bind (&key enable disable blend-mode depth-mode polygon-mode
                         line-width point-size)
      features
    (a:with-gensyms (entity material)
      (let ((enable (set-difference enable pyx::+enabled-capabilities+))
            (disable (set-difference disable pyx::+disabled-capabilities+))
            (blend-mode (and (not (equal blend-mode pyx::+blend-mode+))
                             blend-mode))
            (depth-mode (and (not (equal depth-mode pyx::+depth-mode+))
                             depth-mode))
            (polygon-mode (and (not (equal polygon-mode pyx::+polygon-mode+))
                               polygon-mode)))
        `(lambda (,entity)
           (let ((,material (render/current-material ,entity)))
             (pyx::with-framebuffer (pyx::framebuffer ,material)
                 (:attachments (pyx::attachments ,material))
               (shadow:with-shader (pyx::shader (pyx::spec ,material))
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
                 (pyx::on-pre-render ,entity)
                 (u:do-hash-values (v (pyx::uniforms ,material))
                   (pyx::resolve-uniform-func ,entity v))
                 (pyx::on-render ,entity)
                 (setf (pyx::texture-unit-state ,material) 0)
                 ,@(when disable
                     `((gl:enable ,@disable)))
                 ,@(when enable
                     `((gl:disable ,@enable)))
                 ,@(when blend-mode
                     `((gl:blend-func ,@pyx::+blend-mode+)))
                 ,@(when depth-mode
                     `((gl:depth-func ,pyx::+depth-mode+)))
                 ,@(when polygon-mode
                     `((gl:polygon-mode ,@pyx::+polygon-mode+)))
                 ,@(when line-width
                     `((gl:line-width 1f0)))
                 ,@(when point-size
                     `((gl:point-size 1f0)))))))))))

;;; entity hooks

(pyx:define-entity-hook :attach (entity render)
  (setf render/materials (register-material entity)))

(pyx:define-entity-hook :detach (entity render)
  (u:do-hash-values (viewport (pyx::table (pyx::get-viewport-manager)))
    (pyx::deregister-render-order viewport entity)))

(pyx:define-entity-hook :pre-render (entity render)
  (a:when-let ((camera (pyx::camera (pyx::active (pyx::get-viewport-manager)))))
    (when (pyx:has-component-p camera 'camera)
      (pyx:set-uniforms entity
                        :view (camera/view camera)
                        :proj (camera/projection camera)))))
