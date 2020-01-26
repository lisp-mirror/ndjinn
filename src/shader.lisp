(in-package #:pyx)

(defclass shaders ()
  ((%table :reader table
           :initarg :table)
   (%buffer-bindings :reader buffer-bindings
                     :initform (u:dict #'equalp))
   (%released-buffer-bindings :accessor released-buffer-bindings
                              :initform nil)))

(define-event-handler :recompile :shaders recompile-shaders)

(defun initialize-shaders ()
  (let* ((table (shadow:load-shaders
                 (lambda (x)
                   (enqueue :recompile (list :shaders x)))))
         (shaders (make-instance 'shaders
                                 :table table)))
    (setf (slot-value *state* '%shaders) shaders)))

(defun recompile-shaders (program-names)
  (shadow:recompile-shaders program-names))

(defun select-shader-buffer-binding ()
  (with-slots (%buffer-bindings %released-buffer-bindings) (shaders *state*)
    (let* ((id-count (hash-table-count %buffer-bindings))
           (max-bindings (get-gpu-limit/ssbo-bindings)))
      (when (= id-count max-bindings)
        (error "Cannot create shader buffer. Maximum bindings reached: ~d."
               max-bindings))
      (or (pop %released-buffer-bindings)
          (1+ id-count)))))

(defun release-shader-buffer-binding (key)
  (with-slots (%buffer-bindings %released-buffer-bindings) (shaders *state*)
    (a:when-let ((id (u:href %buffer-bindings key)))
      (remhash key %buffer-bindings)
      (pushnew id %released-buffer-bindings)
      (setf %released-buffer-bindings
            (sort (copy-seq %released-buffer-bindings) #'<)))))

(defun make-shader-buffer (key block-id shader)
  (with-slots (%buffer-bindings) (shaders *state*)
    (let ((binding (select-shader-buffer-binding)))
      (setf (u:href %buffer-bindings key) binding)
      (shadow:create-block-alias :buffer block-id shader key)
      (shadow:bind-block key binding)
      (shadow:create-buffer key key)
      (shadow:bind-buffer key binding))))

(defgeneric update-shader-buffer (object))

(defun delete-shader-buffer (key)
  (release-shader-buffer-binding key)
  (shadow:delete-buffer key))
