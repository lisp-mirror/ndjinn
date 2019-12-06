(in-package #:pyx)

(defclass framebuffer ()
  ((%spec :reader spec
          :initarg :spec)
   (%name :reader name
          :initarg :name)
   (%id :reader id
        :initarg :id)
   (%target :reader target
            :initarg :target)
   (%attachments :reader attachments
                 :initform (u:dict #'eq))
   (%clear-color :reader clear-color
                 :initarg :clear-color)
   (%clear-buffers :reader clear-buffers
                   :initarg :clear-buffers)))

(defun make-framebuffer (spec)
  (with-slots (%name %mode %attachments) spec
    (let* ((target (framebuffer-mode->target %mode))
           (framebuffer (make-instance 'framebuffer
                                       :spec spec
                                       :id (gl/create-framebuffer)
                                       :name %name
                                       :target target
                                       :clear-color (clear-color spec)
                                       :clear-buffers (clear-buffers spec))))
      (u:do-hash-values (attachment %attachments)
        (framebuffer-attach framebuffer (name attachment)))
      (setf (u:href (framebuffers *state*) %name) framebuffer)
      framebuffer)))

(defun find-framebuffer (name)
  (u:href (framebuffers *state*) name))

(defun ensure-framebuffer (name)
  (a:when-let ((spec (meta :framebuffers name)))
    (or (find-framebuffer name)
        (make-framebuffer spec))))

(defun framebuffer-attachment-point->gl (point)
  (destructuring-bind (type &optional (index 0)) (a:ensure-list point)
    (ecase type
      (:color (a:format-symbol :keyword "~a-ATTACHMENT~d" type index))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(defun framebuffer-attachment-names->points (framebuffer attachment-names)
  (mapcar
   (lambda (x)
     (let ((attachment (find-framebuffer-attachment-spec (spec framebuffer) x)))
       (unless attachment
         (error "Attachment name ~s not found for framebuffer ~s."
                x (name framebuffer)))
       (framebuffer-attachment-point->gl (point attachment))))
   attachment-names))

(defun framebuffer-point->render-buffer-format (point)
  (destructuring-bind (type &optional index) (a:ensure-list point)
    (declare (ignore index))
    (ecase type
      (:color :rgb)
      (:depth :depth-component)
      (:stencil :stencil-index)
      (:depth/stencil :depth24-stencil8))))

(defun ensure-framebuffer-complete (framebuffer target buffer attachment)
  (with-slots (%id %name) framebuffer
    (let ((result (%gl:check-named-framebuffer-status %id target)))
      (unless (member result '(:framebuffer-complete :framebuffer-complete-oes))
        (error "Error attaching ~a as attachment ~a of framebuffer ~a: ~a"
               buffer attachment %name result)))))

(defmacro with-framebuffer (framebuffer (&key mode output) &body body)
  (a:with-gensyms (id target)
    `(if ,framebuffer
         (let ((,id (id ,framebuffer))
               (,target (if ,mode
                            (framebuffer-mode->target ,mode)
                            (target ,framebuffer))))
           ,@(when output `((gl/named-framebuffer-draw-buffers ,id ,output)))
           (gl:bind-framebuffer ,target ,id)
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
         (progn ,@body))))

(defun framebuffer-attach/render-buffer (framebuffer attachment)
  (with-slots (%id %target %attachments) framebuffer
    (with-slots (%point %width %height) attachment
      (let* ((point (framebuffer-attachment-point->gl %point))
             (internal-format (framebuffer-point->render-buffer-format %point))
             (buffer-id (gl/create-renderbuffer))
             (width (funcall %width))
             (height (funcall %height)))
        (%gl:named-renderbuffer-storage buffer-id internal-format width height)
        (%gl:named-framebuffer-renderbuffer %id point :renderbuffer buffer-id)
        (ensure-framebuffer-complete framebuffer %target buffer-id point)
        (setf (u:href %attachments point) buffer-id)
        buffer-id))))

(defun framebuffer-attach/texture (framebuffer attachment)
  (with-slots (%id %target %attachments) framebuffer
    (with-slots (%name %buffer %point) attachment
      (destructuring-bind (type &optional texture-name) %buffer
        (declare (ignore type))
        (unless texture-name
          (error "Framebuffer ~s attachment ~s uses a texture buffer without a ~
                texture name."
                 (name framebuffer)
                 %name))
        (let* ((buffer-id (id (load-framebuffer-texture
                               framebuffer attachment texture-name)))
               (point (framebuffer-attachment-point->gl %point)))
          (%gl:named-framebuffer-texture %id point buffer-id 0)
          (ensure-framebuffer-complete framebuffer %target buffer-id point)
          (setf (u:href %attachments point) buffer-id)
          buffer-id)))))

(defun framebuffer-attach (framebuffer attachment-name)
  (let* ((spec (spec framebuffer))
         (attachment (find-framebuffer-attachment-spec spec attachment-name)))
    (ecase (car (buffer attachment))
      (:render-buffer (framebuffer-attach/render-buffer framebuffer attachment))
      (:texture (framebuffer-attach/texture framebuffer attachment)))))

(defun find-framebuffer-buffer-id (framebuffer-name attachment-name)
  (let* ((framebuffer (find-framebuffer framebuffer-name))
         (spec (spec framebuffer))
         (attachment (find-framebuffer-attachment-spec spec attachment-name))
         (point (framebuffer-attachment-point->gl (point attachment))))
    (u:href (attachments framebuffer) point)))

(defun clear-framebuffers ()
  (u:do-hash-values (framebuffer (framebuffers *state*))
    (v4:with-components ((v (clear-color framebuffer)))
      (with-framebuffer framebuffer ()
        (gl:clear-color vx vy vz vw)
        (apply #'gl:clear (clear-buffers framebuffer))))))
