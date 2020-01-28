(in-package #:pyx)

(defclass framebuffer-spec ()
  ((%name :reader name
          :initarg :name)
   (%mode :reader mode)
   (%attachments :reader attachments
                 :initform (u:dict #'eq))
   (%materials :accessor materials
               :initform nil)))

(defclass framebuffer-attachment-spec ()
  ((%name :reader name
          :initarg :name)
   (%buffer :reader buffer
            :initarg :buffer)
   (%point :reader point
           :initarg :point)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)))

(u:define-printer (framebuffer-spec stream :identity t)
  (format stream "~s" (name framebuffer-spec)))

(define-event-handler :recompile :framebuffer recompile-framebuffer)

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun make-framebuffer-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (ecase dimension
                   (:width cfg:=WINDOW-WIDTH=)
                   (:height cfg:=WINDOW-HEIGHT=))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height)
        spec
      (make-instance 'framebuffer-attachment-spec
                     :name name
                     :buffer (a:ensure-list buffer)
                     :point point
                     :width (generate-size-func :width width)
                     :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment-spec (framebuffer attachment-name)
  (u:href (attachments framebuffer) attachment-name))

(defun make-framebuffer-spec (name mode attachments)
  (let ((spec (make-instance 'framebuffer-spec :name name)))
    (setf (meta :framebuffers name) spec)
    (update-framebuffer-spec name mode attachments)
    spec))

(defun update-framebuffer-spec (name mode attachments)
  (with-slots (%mode %attachments %materials) (meta :framebuffers name)
    (setf %mode mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (setf (u:href %attachments name)
              (make-framebuffer-attachment-spec x))))
    (dolist (material %materials)
      (unless (eq name (framebuffer (meta :materials material)))
        (a:deletef %materials material)))
    (enqueue :recompile (list :framebuffer name))))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(progn
     (unless (meta :framebuffers)
       (setf (meta :framebuffers) (u:dict #'eq)))
     (if (meta :framebuffers ',name)
         (update-framebuffer-spec ',name ',mode ',body)
         (make-framebuffer-spec ',name ',mode ',body))))
