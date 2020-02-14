(in-package #:%pyx.component.font)

(ent:define-component font ()
  ((%asset :reader asset
           :initarg :font/asset
           :initform nil)
   (%text :reader text
          :initarg :font/text
          :initform "")
   (%position :reader font-position
              :initarg :font/position
              :initform nil)
   (%offset :reader offset
            :initarg :font/offset
            :initform (v2:vec))
   (%spec :reader spec
          :initform nil)
   (%buffer-data :accessor buffer-data
                 :initform nil)
   (%dimensions :accessor dimensions
                :initform (v2:vec)))
  (:sorting :before c/geom:geometry :after c/render:render))

(geom:define-geometry-layout text ()
  (:data (:format interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(geom:define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))

(defun load-font-spec (entity)
  (with-slots (%asset %spec) entity
    (unless %asset
      (error "Font component ~s does not have an asset specified." entity))
    (let ((path (asset:resolve-path %asset)))
      (setf %spec (asset:with-asset-cache :font path
                    (with-open-file (in path)
                      (3b-bmfont-json:read-bmfont-json in)))))))

(defun resolve-font-text (entity)
  (with-slots (%text) entity
    (typecase %text
      (string %text)
      ((or function symbol)
       (let ((text (funcall %text)))
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
          (buffer-data entity))))

;;; entity hooks

(ent:define-entity-hook :attach (entity font)
  (load-font-spec entity)
  (ent:attach-component entity 'c/geom:geometry :geometry/name 'text))

(ent:define-entity-hook :physics-update (entity font)
  (c/transform:translate-entity entity
                                (v3:vec (ui.font:calculate-position
                                         spec
                                         font-position
                                         dimensions
                                         offset))
                                :replace-p t))

(ent:define-entity-hook :render (entity font)
  (when (clock:debug-time-p)
    (u:mvlet* ((text (resolve-font-text entity))
               (func (funcall #'generate-font-data entity))
               (width height (ui.font:map-glyphs spec func text)))
      (v2:with-components ((fd dimensions))
        (setf fdx width fdy height))
      (geom:update-geometry (c/geom:geometry entity) :data buffer-data)))
  (setf buffer-data nil))
