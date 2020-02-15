(in-package #:%pyx.framebuffer)

;;; spec

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  mode
  (attachment-specs (u:dict #'eq))
  materials)

(defstruct (attachment-spec (:constructor %make-attachment-spec)
                            (:conc-name nil)
                            (:predicate nil)
                            (:copier nil))
  attachment-name
  buffer
  point
  width
  height)

(u:define-printer (spec stream :identity t)
  (format stream "~s" (name spec)))

(defun find-spec (name)
  (or (u:href meta:=framebuffers= name)
      (error "Framebuffer ~s is not defined." name)))

(defun make-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (ecase dimension
                   (:width cfg:=window-width=)
                   (:height cfg:=window-height=))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height)
        spec
      (%make-attachment-spec :attachment-name name
                             :buffer (a:ensure-list buffer)
                             :point point
                             :width (generate-size-func :width width)
                             :height (generate-size-func :height height)))))

(defun find-attachment-spec (spec name)
  (u:href (attachment-specs spec) name))

(defun update-spec (name mode attachments)
  (let ((spec (find-spec name)))
    (setf (mode spec) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (setf (u:href (attachment-specs spec) name) (make-attachment-spec x))))
    (util::enqueue :recompile (list :framebuffer name))))

(defun make-spec (name mode attachments)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=framebuffers= name) spec)
    (update-spec name mode attachments)
    spec))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href meta:=framebuffers= ',name)
       (update-spec ',name ',mode ',body)
       (make-spec ',name ',mode ',body)))

;;; implementation

(defstruct (framebuffer (:constructor %make-framebuffer)
                        (:conc-name nil)
                        (:predicate nil)
                        (:copier nil))
  spec
  id
  target
  (attachments (u:dict #'eq)))

(u:define-printer (framebuffer stream)
  (format stream "~s" (name (spec framebuffer))))

(defun mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun find (name)
  (u:href (ctx:framebuffers) name))

(defun make-framebuffer (spec)
  (let* ((target (mode->target (mode spec)))
         (framebuffer (%make-framebuffer :spec spec
                                         :id (gl:gen-framebuffer)
                                         :target target)))
    (attach-all framebuffer)
    (setf (u:href (ctx:framebuffers) (name spec)) framebuffer)
    framebuffer))

(defun load (name)
  (let ((spec (find-spec name)))
    (or (find name)
        (make-framebuffer spec))))

(defun attachment-point->gl (point)
  (destructuring-bind (type &optional (index 0)) (a:ensure-list point)
    (ecase type
      (:color (a:format-symbol :keyword "~a-ATTACHMENT~d" type index))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(defun attachment-names->points (framebuffer attachment-names)
  (mapcar
   (lambda (x)
     (let* ((spec (spec framebuffer))
            (attachment (find-attachment-spec spec x)))
       (unless attachment
         (error "Attachment name ~s not found for framebuffer ~s."
                x (name spec)))
       (attachment-point->gl (point attachment))))
   attachment-names))

(defun attachment-point->render-buffer-format (point)
  (destructuring-bind (type &optional index) (a:ensure-list point)
    (declare (ignore index))
    (ecase type
      (:color :rgb)
      (:depth :depth-component)
      (:stencil :stencil-index)
      (:depth/stencil :depth24-stencil8))))

(defun ensure-complete (framebuffer target buffer attachment)
  (let ((result (gl:check-framebuffer-status  target)))
    (unless (cl:find result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Error attaching ~a as attachment ~s of framebuffer ~s: ~a"
             buffer attachment (name (spec framebuffer)) result))))

(defmacro with-framebuffer (framebuffer (&key mode attachments) &body body)
  (a:with-gensyms (id target)
    `(if ,framebuffer
         (let ((,id (id ,framebuffer))
               (,target ,(if mode
                             `(mode->target ,mode)
                             `(target ,framebuffer))))
           (gl:bind-framebuffer ,target ,id)
           ,@(when attachments
               `((gl:draw-buffers ,attachments)))
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
         (progn ,@body))))

(defun attach/render-buffer (framebuffer attachment)
  (let* ((point (point attachment))
         (gl-point (attachment-point->gl point))
         (internal-format (attachment-point->render-buffer-format point))
         (buffer-id (gl:gen-renderbuffer))
         (width (funcall (width attachment)))
         (height (funcall (height attachment)))
         (target (target framebuffer)))
    (gl:bind-renderbuffer :renderbuffer buffer-id)
    (gl:renderbuffer-storage :renderbuffer internal-format width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:bind-framebuffer target (id framebuffer))
    (gl:framebuffer-renderbuffer target gl-point :renderbuffer buffer-id)
    (ensure-complete framebuffer target buffer-id point)
    (gl:bind-framebuffer target 0)
    (setf (u:href (attachments framebuffer) point) buffer-id)
    buffer-id))

(defun attach/texture (framebuffer attachment)
  (destructuring-bind (type &optional texture-name) (buffer attachment)
    (declare (ignore type))
    (unless texture-name
      (error "Framebuffer ~s attachment ~s uses a texture buffer without a ~
                texture name."
             (name framebuffer)
             (attachment-name attachment)))
    (let* ((width (funcall (width attachment)))
           (height (funcall (height attachment)))
           (buffer-id (tex:id (tex:load texture-name
                                        :width width
                                        :height height)))
           (point (attachment-point->gl (point attachment)))
           (target (target framebuffer)))
      (gl:bind-framebuffer target (id framebuffer))
      (%gl:framebuffer-texture target point buffer-id 0)
      (ensure-complete framebuffer target buffer-id point)
      (gl:bind-framebuffer target 0)
      (setf (u:href (attachments framebuffer) point) buffer-id)
      buffer-id)))

(defun attach (framebuffer attachment-name)
  (let* ((spec (spec framebuffer))
         (attachment (find-attachment-spec spec attachment-name)))
    (ecase (car (buffer attachment))
      (:render-buffer (attach/render-buffer framebuffer attachment))
      (:texture (attach/texture framebuffer attachment)))))

(defun attach-all (framebuffer)
  (let ((spec (spec framebuffer)))
    (u:do-hash-values (attachment (attachment-specs spec))
      (attach framebuffer (attachment-name attachment)))))

(util::on-recompile :framebuffer data ()
  (a:when-let ((spec (find-spec data))
               (data (find data)))
    (attach-all data)
    (dolist (material (materials spec))
      (util::enqueue :recompile (list :material material)))))
