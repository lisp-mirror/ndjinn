(in-package #:%pyx.component.font)

(ent:define-component font ()
  ((%texture :reader texture
             :initarg :font/texture
             :initform nil)
   (%geometry :reader geometry
              :initarg :font/geometry
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
  (:sorting :after render))

(defun load-font-spec (entity)
  (with-slots (%texture %spec) entity
    (unless %texture
      (error "Font component ~s does not have a spec." entity))
    (let* ((texture-spec (tex:find-spec %texture))
           (source (tex:source texture-spec))
           (spec (res:resolve-path
                  (make-pathname :defaults source :type "json"))))
      (setf %spec (res:with-resource-cache :font %texture
                    (with-open-file (in spec)
                      (3b-bmfont-json:read-bmfont-json in)))))))

(defun load-font-geometry (entity)
  (with-slots (%geometry %buffer-data) entity
    (unless %geometry
      (error "Font component ~s does not have any geometry." entity))
    (setf %geometry (geom:make-geometry %geometry))))

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
  (load-font-geometry entity))

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
      (geom:update-geometry geometry :data buffer-data)))
  (geom:draw-geometry geometry 1)
  (setf buffer-data nil))

(ent:define-entity-hook :delete (entity font)
  (geom:delete-geometry geometry))
