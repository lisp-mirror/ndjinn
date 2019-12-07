(in-package #:pyx)

(defclass framebuffer-spec ()
  ((%name :reader name
          :initarg :name)
   (%mode :reader mode
          :initarg :mode)
   (%attachments :reader attachments
                 :initarg :attachments
                 :initform (u:dict #'eq))))

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

(u:define-printer (framebuffer-spec stream)
  (format stream "~s" (name framebuffer-spec)))

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun make-framebuffer-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (cfg (a:format-symbol :keyword "WINDOW-~a" dimension))))))
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

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  (a:with-gensyms (spec)
    `(let ((,spec (make-instance 'framebuffer-spec
                                 :name ',name
                                 :mode ,mode)))
       (unless (meta :framebuffers)
         (setf (meta :framebuffers) (u:dict #'eq)))
       ,@(mapcar
          (lambda (x)
            (destructuring-bind (name &key &allow-other-keys) x
              `(setf (u:href (attachments ,spec) ',name)
                     (make-framebuffer-attachment-spec ',x))))
          body)
       (setf (meta :framebuffers ',name) ,spec))))
