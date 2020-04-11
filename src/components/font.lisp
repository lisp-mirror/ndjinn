(in-package #:pyx.component)

(pyx:define-component font ()
  ((%font/asset :reader font/asset
                :initarg :font/asset
                :initform nil)
   (%font/text :reader font/text
               :initarg :font/text
               :initform "")
   (%font/position :reader font/position
                   :initarg :font/position
                   :initform nil)
   (%font/offset :reader font/offset
                 :initarg :font/offset
                 :initform (v2:vec))
   (%font/spec :reader font/spec
               :initform nil)
   (%font/buffer-data :accessor font/buffer-data
                      :initform nil)
   (%font/dimensions :accessor font/dimensions
                     :initform (v2:vec)))
  (:sorting :before geometry :after render))

(pyx:define-geometry-layout text ()
  (:data (:format interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(pyx:define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))

(defun load-font-spec (entity)
  (with-slots (%font/asset %font/spec) entity
    (unless %font/asset
      (error "Font component ~s does not have an asset specified." entity))
    (let ((path (pyx:resolve-path %font/asset)))
      (setf %font/spec (pyx:with-asset-cache :font path
                         (with-open-file (in path)
                           (3b-bmfont-json:read-bmfont-json in)))))))

(defun resolve-font-text (entity)
  (with-slots (%font/text) entity
    (typecase %font/text
      (string %font/text)
      ((or function symbol)
       (let ((text (funcall %font/text)))
         (unless (stringp text)
           (error "Font component ~s has text that is not a string." entity))
         text)))))

(defun generate-font-data (entity)
  (lambda (x- y- x+ y+ u- v- u+ v+)
    (push `((,x- ,y+ ,u- ,v+)
            (,x- ,y- ,u- ,v-)
            (,x+ ,y+ ,u+ ,v+)
            (,x+ ,y+ ,u+ ,v+)
            (,x- ,y- ,u- ,v-)
            (,x+ ,y- ,u+ ,v-))
          (font/buffer-data entity))))

;;; entity hooks

(pyx:define-entity-hook :attach (entity font)
  (load-font-spec entity)
  (pyx:attach-component entity 'geometry :geometry/name 'text))

(pyx:define-entity-hook :physics-update (entity font)
  (translate-entity entity
                    (v3:vec (pyx::calculate-font-position
                             font/spec
                             font/position
                             font/dimensions
                             font/offset))
                    :replace t))

(pyx:define-entity-hook :render (entity font)
  (when (pyx::debug-time-p)
    (u:mvlet* ((text (resolve-font-text entity))
               (func (funcall #'generate-font-data entity))
               (width height (pyx::map-font-glyphs font/spec func text)))
      (v2:with-components ((fd font/dimensions))
        (setf fdx width fdy height))
      (pyx:update-geometry (geometry/geometry entity) :data font/buffer-data)))
  (setf font/buffer-data nil))
