(in-package #:%pyx.image)

(defstruct (image (:conc-name nil)
                  (:predicate nil)
                  (:copier nil))
  path
  width
  height
  pixel-format
  pixel-type
  internal-format
  data)

(defun get-image-type (path)
  (a:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load (type path)
  (:method (type path)
    (error "Unsupported image type ~s for asset: ~s." type path)))

(defgeneric load (asset &key &allow-other-keys))

(defmethod load ((asset null)
                 &key width height pixel-format pixel-type internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))

(defmethod load (asset &key)
  (let* ((path (asset:resolve-path asset))
         (type (get-image-type path)))
    (%load type path)))
