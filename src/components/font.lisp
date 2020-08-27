(in-package #:net.mfiano.lisp.pyx)

(define-component font ()
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
   (%font/rate :reader font/rate
               :initarg :font/rate
               :initform 0.5)
   (%font/spec :reader font/spec
               :initform nil)
   (%font/update-time :accessor font/update-time
                      :initform 0)
   (%font/dimensions :accessor font/dimensions
                     :initform (v2:vec)))
  (:type-order :before geometry :after render))

(define-geometry-layout text ()
  (:data (:format :interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))

(defun load-font-spec (entity)
  (with-slots (%font/asset %font/spec) entity
    (unless %font/asset
      (error "Font component ~s does not have an asset specified." entity))
    (let ((path (resolve-path %font/asset)))
      (setf %font/spec (with-asset-cache :font path
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
          (geometry/data entity))))

;;; entity hooks

(define-entity-hook :attach (entity font)
  (load-font-spec entity)
  (attach-component entity 'geometry :geometry/name 'text))

(define-entity-hook :update (entity font)
  (let ((time (get-running-time)))
    (when (>= time (+ font/update-time font/rate))
      (u:mvlet* ((text (resolve-font-text entity))
                 (func (funcall #'generate-font-data entity))
                 (width height (map-font-glyphs font/spec func text)))
        (v2:with-components ((fd font/dimensions))
          (setf fdx width fdy height)))
      (setf font/update-time time))))
