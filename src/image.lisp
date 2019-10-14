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

(defun get-image-channel-count (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale 1)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-image-pixel-format (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-image-pixel-type (image)
  (ecase (pngload-fast:bit-depth image)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun get-image-internal-format (image)
  (let ((channel-count (get-image-channel-count image))
        (bit-depth (pngload-fast:bit-depth image)))
    (a:format-symbol :keyword (subseq "RGBA" 0 channel-count) bit-depth :ui)))

(defgeneric read-image (path &key &allow-other-keys))

(defmethod read-image ((path string) &key)
  (let* ((resolved-path (resolve-asset-path path))
         (image (pngload-fast:load-file resolved-path :flatten t :flip-y t)))
    (make-instance 'image
                   :path path
                   :width (pngload-fast:width image)
                   :height (pngload-fast:height image)
                   :pixel-format (get-image-pixel-format image)
                   :pixel-type (get-image-pixel-type image)
                   :internal-format (get-image-internal-format image)
                   :data (pngload-fast:data image))))

(defmethod read-image ((path null)
                       &key width height pixel-format pixel-type
                         internal-format)
  (make-instance 'image
                 :width width
                 :height height
                 :pixel-format pixel-format
                 :pixel-type pixel-type
                 :internal-format internal-format))
