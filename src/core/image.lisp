(in-package #:pyx)

(defclass image ()
  ((%path :reader path
          :initarg :path
          :initform nil)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data
          :initform nil)))

(defun get-image-type (path)
  (a:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load-image (type path)
  (:method (type path)
    (error "Unsupported image type ~s for asset: ~s." type path)))

(defgeneric load-image (asset &key &allow-other-keys))

(defmethod load-image ((asset null)
                       &key width height pixel-format pixel-type
                         internal-format)
  (make-instance 'image
                 :width width
                 :height height
                 :pixel-format pixel-format
                 :pixel-type pixel-type
                 :internal-format internal-format))

(defmethod load-image (asset &key)
  (let* ((path (resolve-path asset))
         (type (get-image-type path)))
    (%load-image type path)))
