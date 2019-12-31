(in-package #:pyx)

(define-component render ()
  ((%render/materials :accessor render/materials
                      :initarg :render/materials)
   (%render/order :reader render/order
                  :initarg :render/order
                  :initform :default)
   (%render/current-material :accessor render/current-material
                             :initform nil))
  (:sorting :after xform :before sprite))

(defun clear-render-pass (pipeline pass)
  (let ((options (u:href (pass-options pipeline) pass)))
    (destructuring-bind (&key clear-color clear-buffers) options
      (v4:with-components ((v clear-color))
        (gl:clear-color vx vy vz vw)
        (apply #'gl:clear clear-buffers)))))

(defun render-frame ()
  (let ((scene-spec (spec (current-scene *state*))))
    (map nil #'render-pass (pass-order (pipeline scene-spec)))))

(defun render-pass (pass)
  (a:when-let ((scene (current-scene *state*)))
    (clear-render-pass (pipeline (spec scene)) pass)
    (tree:walk
     (draw-order scene)
     (lambda (x)
       ;; TODO: we should have a tree of trees where the outer tree are the
       ;; sorted passes, and the inner ones are the ordered entities with
       ;; materials for that pass
       (a:when-let ((material (u:href (render/materials x) pass)))
         (setf (render/current-material x) material)
         (render-entity x))))))

(defun render-entity (entity)
  (with-slots (%spec %framebuffer %attachment-points %uniforms %funcs
               %texture-unit-state)
      (render/current-material entity)
    (with-slots (%enabled %disabled %blend-mode %depth-mode) %spec
      (with-framebuffer %framebuffer (:attachments %attachment-points)
        (shadow:with-shader (shader %spec)
          (u:do-hash (k v %uniforms)
            (resolve-uniform-func %funcs k v))
          (with-opengl-state (%enabled %disabled %blend-mode %depth-mode)
            (on-entity-render entity))
          (on-entity-render entity)
          (setf %texture-unit-state 0))))))

;;; entity hooks

(define-hook :entity-create (entity render)
  (setf render/materials (register-materials entity)))

(define-hook :entity-render (entity render)
  (a:when-let ((camera (camera (current-scene *state*))))
    (set-uniforms render/current-material
                  :view (camera/view camera)
                  :proj (camera/projection camera))))

(define-hook :component-attach (entity render)
  (register-draw-order entity))

(define-hook :component-detach (entity render)
  (deregister-draw-order entity))
