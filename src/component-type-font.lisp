(in-package #:ndjinn)

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
  (:order :before geometry :after render))

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
    (let ((data `((,x- ,y+ ,u- ,v+)
                  (,x- ,y- ,u- ,v-)
                  (,x+ ,y+ ,u+ ,v+)
                  (,x+ ,y+ ,u+ ,v+)
                  (,x- ,y- ,u- ,v-)
                  (,x+ ,y- ,u+ ,v-))))
      (update-geometry entity :data data))))

;;; entity hooks

(define-entity-hook :attach (entity font)
  (load-font-spec entity)
  (attach-component entity 'geometry :geometry/name 'text :geometry/cache nil))

(define-entity-hook :pre-render (entity font)
  (let ((time (get-running-time)))
    (when (>= time (+ (font/update-time entity)
                      (font/rate entity)))
      (u:mvlet* ((text (resolve-font-text entity))
                 (func (funcall #'generate-font-data entity))
                 (width height (map-font-glyphs (font/spec entity) func text)))
        (translate-entity entity
                          (v3:vec (calculate-font-position entity))
                          :replace t)
        (v2:with-components ((fd (font/dimensions entity)))
          (setf fdx width fdy height)))
      (setf (font/update-time entity) time))))
