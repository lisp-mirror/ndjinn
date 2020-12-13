(in-package #:ndjinn)

(defstruct (image
            (:predicate nil)
            (:copier nil))
  (path #p"" :type pathname)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type (member :red :rg :rgb :rgba))
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (data nil :type (or vector null)))

(defun get-image-type (path)
  (u:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load-image (type path)
  (:method (type path)
    (error "Unsupported image type ~s for asset: ~s." type path)))

(defgeneric load-image (asset &key &allow-other-keys))

(defmethod load-image ((asset null)
                       &key width height pixel-format pixel-type
                         internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))

(defmethod load-image (asset &key)
  (let* ((path (resolve-path asset))
         (type (get-image-type path)))
    (log:debug :ndjinn "Loading image: ~s..." path)
    (prog1 (%load-image type path)
      (log:debug :ndjinn "Loaded image: ~s" path))))
