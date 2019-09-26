(in-package #:pyx)

(defclass texture ()
  ((%path :reader path
          :initarg :path)
   (%id :reader id
        :initarg :id)
   (%raw-data :reader raw-data
              :initarg :raw-data)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%channels :reader channels
              :initarg :channels)
   (%pixel-format :reader pixel-format
                  :initarg :pixel-format)
   (%pixel-type :reader pixel-type
                :initarg :pixel-type
                :initform :unsigned-byte)
   (%internal-format :reader internal-format
                     :initarg :internal-format)
   (%data :reader data
          :initarg :data)))

(defun get-internal-format (pixel-format)
  (ecase pixel-format
    (:red :r8)
    ((:rgb :bgr) :rgb8)
    ((:rgba :bgra) :rgba8)))

(defun get-image-channel-count (raw-data)
  (ecase (pngload-fast:color-type raw-data)
    (:greyscale 1)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-image-pixel-format (raw-data)
  (ecase (pngload-fast:color-type raw-data)
    (:greyscale :red)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun make-texture (path)
  (let* ((raw-data (pngload-fast:load-file path :flatten t :flip-y t))
         (pixel-format (get-image-pixel-format raw-data)))
    (make-instance 'texture
                   :path path
                   :id (gl:gen-texture)
                   :raw-data raw-data
                   :width (pngload-fast:width raw-data)
                   :height (pngload-fast:height raw-data)
                   :channels (get-image-channel-count raw-data)
                   :pixel-format pixel-format
                   :internal-format (get-internal-format pixel-format)
                   :data (pngload-fast:data raw-data))))

(defun free-texture-image (texture)
  (with-slots (%raw-data %data) texture
    (setf %raw-data nil
          %data nil)
    (u:noop)))

(defun store-texture (texture)
  (unwind-protect
       (gl:tex-image-2d :texture-2d
                        0
                        (internal-format texture)
                        (width texture)
                        (height texture)
                        0
                        (pixel-format texture)
                        (pixel-type texture)
                        (data texture))
    (free-texture-image texture)))

(defun configure-texture (mipmaps-p min-filter mag-filter)
  (when mipmaps-p
    (gl:generate-mipmap :texture-2d))
  (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
  (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter))

(defun load-texture (path
                     &key (mipmaps-p t) (min-filter :linear-mipmap-linear)
                       (mag-filter :linear))
  (flet ((%load-file (path)
           (cache-lookup :texture path
             (let ((texture (make-texture (resolve-asset-path path))))
               (gl:bind-texture :texture-2d (id texture))
               (store-texture texture)
               (configure-texture mipmaps-p min-filter mag-filter)
               (gl:bind-texture :texture-2d 0)
               texture))))
    (handler-case (%load-file path)
      (error () (%load-file "debug.png")))))

(defun bind-texture (unit value)
  (gl:active-texture unit)
  (gl:bind-texture :texture-2d value))
