(in-package #:pyx)

(defclass framebuffer ()
  ((%spec :reader spec
          :initarg :spec)
   (%name :reader name
          :initarg :name)
   (%id :reader id
        :initarg :id)
   (%mode :reader mode
          :initarg :mode)
   (%attachments :reader attachments
                 :initform (u:dict))))

(defun make-framebuffer (spec)
  (with-slots (%name %mode) spec
    (let ((framebuffer (make-instance 'framebuffer
                                      :spec spec
                                      :id (gl:gen-framebuffer)
                                      :name %name
                                      :mode %mode)))
      (setf (u:href (framebuffers *state*) %name) framebuffer)
      framebuffer)))

(defun delete-framebuffer (framebuffer)
  (gl:delete-framebuffers (list (id framebuffer))))

(defun find-framebuffer (name)
  (u:href (framebuffers *state*) name))

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

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

(defmacro with-framebuffer ((framebuffer) &body body)
  (a:with-gensyms (target)
    `(let ((,target (framebuffer-mode->target (mode ,framebuffer))))
       (gl:bind-framebuffer ,target (id ,framebuffer))
       ,@body
       (gl:bind-framebuffer ,target 0))))

(defun framebuffer-attach/render-buffer (framebuffer attachment)
  (with-slots (%point %width %height) attachment
    (let* ((framebuffer-target (framebuffer-mode->target (mode framebuffer)))
           (internal-format (framebuffer-point->render-buffer-format %point))
           (point (framebuffer-attachment-point->gl %point))
           (buffer-id (gl:gen-renderbuffer))
           (width (funcall %width))
           (height (funcall %height)))
      (gl:bind-renderbuffer :renderbuffer buffer-id)
      (gl:renderbuffer-storage :renderbuffer internal-format width height)
      (gl:bind-renderbuffer :renderbuffer 0)
      (with-framebuffer (framebuffer)
        (gl:framebuffer-renderbuffer
         framebuffer-target point :renderbuffer buffer-id)
        (ensure-framebuffer-complete
         framebuffer framebuffer-target buffer-id point))
      (setf (u:href (attachments framebuffer) point) buffer-id)
      buffer-id)))

(defun framebuffer-attach/texture (framebuffer attachment)
  (with-slots (%point) attachment
    (let* ((target (framebuffer-mode->target (mode framebuffer)))
           (buffer-id (id (load-framebuffer-texture framebuffer attachment)))
           (point (framebuffer-attachment-point->gl %point)))
      (with-framebuffer (framebuffer)
        (%gl:framebuffer-texture target point buffer-id 0)
        (ensure-framebuffer-complete framebuffer target buffer-id point))
      (setf (u:href (attachments framebuffer) point) buffer-id)
      buffer-id)))

(defun framebuffer-attach (framebuffer attachment-name)
  (let* ((spec (spec framebuffer))
         (attachment (find-framebuffer-attachment-spec spec attachment-name)))
    (ecase (attachment-type attachment)
      (:render-buffer (framebuffer-attach/render-buffer framebuffer attachment))
      (:texture (framebuffer-attach/texture framebuffer attachment)))))

(defun find-framebuffer-texture-id (framebuffer-name attachment-name)
  (let* ((framebuffer (find-framebuffer framebuffer-name))
         (spec (spec framebuffer))
         (attachment (find-framebuffer-attachment-spec
                      spec attachment-name))
         (point (framebuffer-attachment-point->gl (point attachment))))
    (u:href (attachments framebuffer) point)))

(defun initialize-framebuffers ()
  (u:do-hash-values (spec (meta :framebuffers))
    (let ((framebuffer (make-framebuffer spec)))
      (u:do-hash-values (attachment (attachments spec))
        (framebuffer-attach framebuffer (name attachment))))))
