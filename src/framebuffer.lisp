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
                 :initform (u:dict))))

(defun make-framebuffer (spec)
  (with-slots (%name %mode) spec
    (let* ((target (framebuffer-mode->target %mode))
           (framebuffer (make-instance 'framebuffer
                                       :spec spec
                                       :id (gl:gen-framebuffer)
                                       :name %name
                                       :target target)))
      (setf (u:href (framebuffers (database *state*)) %name) framebuffer)
      framebuffer)))

(defun delete-framebuffer (framebuffer)
  (gl:delete-framebuffers (list (id framebuffer))))

(defun find-framebuffer (name)
  (u:href (framebuffers (database *state*)) name))

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
  (let ((result (gl:check-framebuffer-status target)))
    (unless (member result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Error attaching ~a as attachment ~a of framebuffer ~a: ~a"
             buffer attachment (name framebuffer) result))))

(defmacro with-framebuffer (framebuffer (&key mode output) &body body)
  (a:with-gensyms (target)
    `(if ,framebuffer
         (let ((,target ,(if mode
                             (framebuffer-mode->target mode)
                             `(target ,framebuffer))))
           (gl:bind-framebuffer ,target (id ,framebuffer))
           ,@(when output
               `((gl:draw-buffers ,output)))
           ,@body
           (gl:bind-framebuffer ,target 0))
         ,@body)))

(defun framebuffer-attach/render-buffer (framebuffer attachment)
  (with-slots (%id %target %attachments) framebuffer
    (with-slots (%point %width %height) attachment
      (let* ((point (framebuffer-attachment-point->gl %point))
             (internal-format (framebuffer-point->render-buffer-format %point))
             (buffer-id (gl:gen-renderbuffer))
             (width (funcall %width))
             (height (funcall %height)))
        (gl:bind-renderbuffer :renderbuffer buffer-id)
        (gl:renderbuffer-storage :renderbuffer internal-format width height)
        (gl:bind-renderbuffer :renderbuffer 0)
        (gl:bind-framebuffer %target %id)
        (gl:framebuffer-renderbuffer %target point :renderbuffer buffer-id)
        (ensure-framebuffer-complete framebuffer %target buffer-id point)
        (gl:bind-framebuffer %target 0)
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
          (gl:bind-framebuffer %target %id)
          (%gl:framebuffer-texture %target point buffer-id 0)
          (ensure-framebuffer-complete framebuffer %target buffer-id point)
          (gl:bind-framebuffer %target 0)
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

(defun initialize-framebuffers ()
  (when (meta :framebuffers)
    (u:do-hash-values (spec (meta :framebuffers))
      (let ((framebuffer (make-framebuffer spec)))
        (u:do-hash-values (attachment (attachments spec))
          (framebuffer-attach framebuffer (name attachment)))))))
