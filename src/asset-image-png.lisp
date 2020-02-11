(in-package #:%pyx.asset.image.png)

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

(defun load (path)
  (let ((image (pngload-fast:load-file path :flatten t :flip-y t)))
    (img:make-image :path path
                    :width (pngload-fast:width image)
                    :height (pngload-fast:height image)
                    :pixel-format (get-pixel-format image)
                    :pixel-type (get-pixel-type image)
                    :internal-format (get-internal-format image)
                    :data (pngload-fast:data image))))
