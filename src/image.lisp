(in-package #:pyx)

(defclass image ()
  ((%path :reader path
          :initarg :path)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%channels :reader channels
              :initarg :channels)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)))

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

(defun read-image (path)
  (handler-case
      (let* ((resolved-path (resolve-asset-path path))
             (image (pngload-fast:load-file resolved-path :flatten t :flip-y t)))
        (make-instance 'image
                       :path path
                       :width (pngload-fast:width image)
                       :height (pngload-fast:height image)
                       :channels (get-image-channel-count image)
                       :pixel-format (get-image-pixel-format image)
                       :pixel-type (get-image-pixel-type image)
                       :internal-format (get-image-internal-format image)
                       :data (pngload-fast:data image)))
    (error () (read-image "debug.png"))))
