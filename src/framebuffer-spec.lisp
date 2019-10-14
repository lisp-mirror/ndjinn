(in-package #:pyx)

(defclass framebuffer-spec ()
  ((%name :reader name
          :initarg :name)
   (%mode :reader mode
          :initarg :mode)
   (%attachments :reader attachments
                 :initarg :attachments
                 :initform (u:dict))))

(defclass framebuffer-attachment-spec ()
  ((%name :reader name
          :initarg :name)
   (%type :reader attachment-type
          :initarg :type)
   (%point :reader point
           :initarg :point)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)))

(defun make-framebuffer-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (cfg (a:format-symbol :keyword "WINDOW-~a" dimension))))))
    (destructuring-bind (name &key point (type :render-buffer) width height)
        spec
      (make-instance 'framebuffer-attachment-spec
                     :name name
                     :type type
                     :point point
                     :width (generate-size-func :width width)
                     :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment-spec (framebuffer attachment-name)
  (u:href (attachments framebuffer) attachment-name))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  (a:with-gensyms (spec)
    `(let ((,spec (make-instance 'framebuffer-spec :name ',name :mode ,mode)))
       (unless (meta :framebuffers)
         (setf (meta :framebuffers) (u:dict #'eq)))
       ,@(mapcar
          (lambda (x)
            (destructuring-bind (name &key &allow-other-keys) x
              `(setf (u:href (attachments ,spec) ',name)
                     (make-framebuffer-attachment-spec ',x))))
          body)
       (setf (meta :framebuffers ',name) ,spec))))
