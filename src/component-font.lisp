(in-package #:pyx)

(define-component font ()
  ((%font/texture :reader font/texture
                  :initarg :font/texture
                  :initform nil)
   (%font/geometry :reader font/geometry
                   :initarg :font/geometry
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
  (:sorting :after render))

(defun load-font-spec (entity)
  (with-slots (%font/texture %font/spec) entity
    (unless %font/texture
      (error "Font component ~s does not have a spec." entity))
    (let* ((texture-spec (find-texture-spec %font/texture))
           (source (source texture-spec))
           (spec (resolve-asset-path
                  (make-pathname :defaults source :type "json"))))
      (unless (uiop:file-exists-p spec)
        (error "Font texture source ~s does not have a spec file." source))
      (setf %font/spec (resource-lookup 'font %font/texture
                         (with-open-file (in spec)
                           (font-spec:read-bmfont-json in)))))))

(defun load-font-geometry (entity)
  (with-slots (%font/geometry %font/buffer-data) entity
    (unless %font/geometry
      (error "Font component ~s does not have any geometry." entity))
    (setf %font/geometry (make-geometry %font/geometry))))

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

(define-hook :attach (entity font)
  (load-font-spec entity)
  (load-font-geometry entity))

(define-hook :update (entity font)
  (u:mvlet* ((text (resolve-font-text entity))
             (func (funcall #'generate-font-data entity))
             (width height (get-font-dimensions font/spec text)))
    (v2:with-components ((v font/dimensions))
      (setf vx width vy height))
    (translate-entity entity (get-font-position entity) :replace-p t)
    (font:map-glyphs font/spec func text :model-y-up t :texture-y-up t)))

(define-hook :render (entity font)
  (update-geometry font/geometry :data font/buffer-data)
  (draw-geometry font/geometry 1)
  (setf font/buffer-data nil))

(define-hook :delete (entity font)
  (delete-geometry font/geometry))
