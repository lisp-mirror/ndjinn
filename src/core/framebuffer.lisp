(in-package #:net.mfiano.lisp.pyx)

;;; spec

(defstruct (framebuffer-spec
            (:constructor %make-framebuffer-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (mode :read/write :type keyword)
  (attachment-specs (u:dict #'eq) :type hash-table)
  (materials nil :type list))

(defstruct (framebuffer-attachment-spec
            (:constructor %make-framebuffer-attachment-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (buffer nil :type list)
  (point nil :type list)
  (width (constantly =window-width=) :type function)
  (height (constantly =window-height=) :type function))

(u:define-printer (framebuffer-spec stream)
  (format stream "~s" (framebuffer-spec-name framebuffer-spec)))

(defun find-framebuffer-spec (name)
  (or (u:href (metadata-framebuffers =metadata=) name)
      (error "Framebuffer ~s is not defined." name)))

(defun make-framebuffer-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (ecase dimension
                   (:width =window-width=)
                   (:height =window-height=))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height)
        spec
      (%make-framebuffer-attachment-spec
       :name name
       :buffer (u:ensure-list buffer)
       :point (u:ensure-list point)
       :width (generate-size-func :width width)
       :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment-spec (spec name)
  (u:href (framebuffer-spec-attachment-specs spec) name))

(defun update-framebuffer-spec (name mode attachments)
  (let* ((spec (find-framebuffer-spec name))
         (attachment-specs (framebuffer-spec-attachment-specs spec)))
    (setf (framebuffer-spec-mode spec) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (let ((attachment-spec (make-framebuffer-attachment-spec x)))
          (setf (u:href attachment-specs name) attachment-spec))))
    (enqueue :recompile (list :framebuffer name))))

(defun make-framebuffer-spec (name mode attachments)
  (let ((spec (%make-framebuffer-spec :name name)))
    (setf (u:href (metadata-framebuffers =metadata=) name) spec)
    (update-framebuffer-spec name mode attachments)
    spec))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href (metadata-framebuffers =metadata=) ',name)
       (update-framebuffer-spec ',name ',mode ',body)
       (make-framebuffer-spec ',name ',mode ',body)))

;;; implementation

(defstruct (framebuffer
            (:constructor %make-framebuffer)
            (:predicate nil)
            (:copier nil))
  (spec (%make-framebuffer-spec) :type framebuffer-spec)
  (id 0 :type fixnum)
  (target :framebuffer :type keyword)
  (attachments (u:dict #'eq) :type hash-table))

(u:define-printer (framebuffer stream)
  (format stream "~s" (framebuffer-spec-name (framebuffer-spec framebuffer))))

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun find-framebuffer (name)
  (u:href (framebuffers =context=) name))

(defun make-framebuffer (spec)
  (let* ((name (framebuffer-spec-name spec))
         (target (framebuffer-mode->target (framebuffer-spec-mode spec)))
         (framebuffer (%make-framebuffer :spec spec
                                         :id (gl:gen-framebuffer)
                                         :target target)))
    (framebuffer-attach-all framebuffer)
    (setf (u:href (framebuffers =context=) name) framebuffer)
    framebuffer))

(defun load-framebuffer (name)
  (let ((spec (find-framebuffer-spec name)))
    (or (find-framebuffer name)
        (make-framebuffer spec))))

(defun framebuffer-attachment-point->gl (point)
  (destructuring-bind (type &optional (index 0)) point
    (ecase type
      (:color (u:format-symbol :keyword "~a-ATTACHMENT~d" type index))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(defun framebuffer-attachment-names->points (framebuffer attachment-names)
  (mapcar
   (lambda (x)
     (let* ((spec (framebuffer-spec framebuffer))
            (attachment (find-framebuffer-attachment-spec spec x)))
       (unless attachment
         (error "Attachment name ~s not found for framebuffer ~s."
                x (framebuffer-spec-name spec)))
       (framebuffer-attachment-point->gl
        (framebuffer-attachment-spec-point attachment))))
   attachment-names))

(defun framebuffer-attachment-point->render-buffer-format (point)
  (destructuring-bind (type &optional index) point
    (declare (ignore index))
    (ecase type
      (:color :rgb)
      (:depth :depth-component)
      (:stencil :stencil-index)
      (:depth/stencil :depth24-stencil8))))

(defun ensure-framebuffer-complete (framebuffer target buffer attachment)
  (let* ((result (gl:check-framebuffer-status target))
         (spec (framebuffer-spec framebuffer))
         (name (framebuffer-spec-name spec)))
    (unless (find result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Error attaching ~a as attachment ~s of framebuffer ~s: ~a"
             buffer attachment name result))))

(defmacro with-framebuffer (framebuffer (&key mode attachments) &body body)
  (u:with-gensyms (id target)
    `(if ,framebuffer
         (let ((,id (framebuffer-id ,framebuffer))
               (,target ,(if mode
                             `(framebuffer-mode->target ,mode)
                             `(framebuffer-target ,framebuffer))))
           (gl:bind-framebuffer ,target ,id)
           ,@(when attachments
               `((gl:draw-buffers ,attachments)))
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
         (progn ,@body))))

(defun framebuffer-attach/render-buffer (framebuffer attachment)
  (let* ((point (framebuffer-attachment-spec-point attachment))
         (gl-point (framebuffer-attachment-point->gl point))
         (internal-format (framebuffer-attachment-point->render-buffer-format
                           point))
         (buffer-id (gl:gen-renderbuffer))
         (width (funcall (framebuffer-attachment-spec-width attachment)))
         (height (funcall (framebuffer-attachment-spec-height attachment)))
         (target (framebuffer-target framebuffer)))
    (gl:bind-renderbuffer :renderbuffer buffer-id)
    (gl:renderbuffer-storage :renderbuffer internal-format width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:bind-framebuffer target (framebuffer-id framebuffer))
    (gl:framebuffer-renderbuffer target gl-point :renderbuffer buffer-id)
    (ensure-framebuffer-complete framebuffer target buffer-id point)
    (gl:bind-framebuffer target 0)
    (setf (u:href (framebuffer-attachments framebuffer) point) buffer-id)
    buffer-id))

(defun framebuffer-attach/texture (framebuffer attachment)
  (let ((attachment-name (framebuffer-attachment-spec-name attachment))
        (point (framebuffer-attachment-spec-point attachment))
        (buffer (framebuffer-attachment-spec-buffer attachment))
        (width-func (framebuffer-attachment-spec-width attachment))
        (height-func (framebuffer-attachment-spec-height attachment)))
    (destructuring-bind (type &optional texture-name) buffer
      (declare (ignore type))
      (unless texture-name
        (error "Framebuffer ~s attachment ~s uses a texture buffer without a ~
                texture name."
               (framebuffer-spec-name (framebuffer-spec framebuffer))
               attachment-name))
      (let* ((width (funcall width-func))
             (height (funcall height-func))
             (buffer-id (id (load-texture texture-name
                                          :width width
                                          :height height)))
             (point (framebuffer-attachment-point->gl point))
             (target (framebuffer-target framebuffer)))
        (gl:bind-framebuffer target (framebuffer-id framebuffer))
        (%gl:framebuffer-texture target point buffer-id 0)
        (ensure-framebuffer-complete framebuffer target buffer-id point)
        (gl:bind-framebuffer target 0)
        (setf (u:href (framebuffer-attachments framebuffer) point) buffer-id)
        buffer-id))))

(defun framebuffer-attach (framebuffer attachment-name)
  (let* ((spec (framebuffer-spec framebuffer))
         (attachment (find-framebuffer-attachment-spec spec attachment-name)))
    (ecase (car (framebuffer-attachment-spec-buffer attachment))
      (:render-buffer (framebuffer-attach/render-buffer framebuffer attachment))
      (:texture (framebuffer-attach/texture framebuffer attachment)))))

(defun framebuffer-attach-all (framebuffer)
  (let ((spec (framebuffer-spec framebuffer)))
    (u:do-hash-values (attachment (framebuffer-spec-attachment-specs spec))
      (framebuffer-attach framebuffer
                          (framebuffer-attachment-spec-name attachment)))))

(on-recompile :framebuffer data ()
  (u:when-let* ((spec (find-framebuffer-spec data))
                (name (framebuffer-spec-name spec))
                (data (find-framebuffer data)))
    (framebuffer-attach-all data)
    (dolist (material (framebuffer-spec-materials spec))
      (enqueue :recompile (list :material material)))
    (log:debug :pyx.live "Recompiled framebuffer: ~s" name)))
