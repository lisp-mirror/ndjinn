(in-package #:%pyx.framebuffer)

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

(defun get-attachment-texture (attachment)
  (values (getf (buffer attachment) :texture)
          (funcall (width attachment))
          (funcall (height attachment))))

(defun update-spec (name mode attachments)
  (let ((spec (find-spec name)))
    (setf (mode spec) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (setf (u:href (attachment-specs spec) name) (make-attachment-spec x))))
    (tp:enqueue :recompile (list :framebuffer name))))

(defun make-spec (name mode attachments)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=framebuffers= name) spec)
    (update-spec name mode attachments)
    spec))

;;; Public API

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href meta:=framebuffers= ',name)
       (update-spec ',name ',mode ',body)
       (make-spec ',name ',mode ',body)))
