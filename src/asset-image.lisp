(in-package #:%pyx.asset.image)

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

(defun get-channel-count (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale 1)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-pixel-format (image)
  (ecase (pngload-fast:color-type image)
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-pixel-type (image)
  (ecase (pngload-fast:bit-depth image)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun get-internal-format (image)
  (let ((channel-count (get-channel-count image))
        (bit-depth (pngload-fast:bit-depth image)))
    (a:format-symbol :keyword "~a~d"
                     (subseq "RGBA" 0 channel-count)
                     bit-depth)))

(defgeneric load (path &key &allow-other-keys))

(defmethod load (asset &key)
  (let* ((path (asset:resolve-path asset))
         (image (pngload-fast:load-file path :flatten t :flip-y t)))
    (make-image :path path
                :width (pngload-fast:width image)
                :height (pngload-fast:height image)
                :pixel-format (get-pixel-format image)
                :pixel-type (get-pixel-type image)
                :internal-format (get-internal-format image)
                :data (pngload-fast:data image))))

(defmethod load ((path null)
                 &key width height pixel-format pixel-type internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))
