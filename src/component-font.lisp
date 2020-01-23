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
               :initform nil)
   (%font/spec :reader font/spec
               :initform nil)
   (%font/buffer-data :accessor font/buffer-data
                      :initform nil))
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
  (let ((text (resolve-font-text entity))
        (func (funcall #'generate-font-data entity)))
    (font:map-glyphs font/spec func text :model-y-up t :texture-y-up t)
    (update-geometry font/geometry :data font/buffer-data)))

(define-hook :render (entity font)
  (draw-geometry font/geometry 1)
  (setf font/buffer-data nil))

(define-hook :delete (entity font)
  (delete-geometry font/geometry))
