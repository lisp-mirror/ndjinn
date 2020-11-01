(in-package #:net.mfiano.lisp.pyx)

(define-component curve ()
  ((%curve/name :reader curve/name
                :initarg :curve/name
                :initform nil)
   (%curve/divisions :reader curve/divisions
                     :initarg :curve/divisions
                     :initform 100)
   (%curve/even-spacing :reader curve/even-spacing
                        :initarg :curve/even-spacing
                        :initform nil)
   (%curve/visualize :reader curve/visualize
                     :initarg :curve/visualize
                     :initform nil)
   (%curve/flip :reader curve/flip
                :initarg :curve/flip
                :initform nil)
   (%curve/data :accessor curve/data
                :initform nil)
   (%curve/segments :accessor curve/segments
                    :initform nil))
  (:type-order :after transform))

(defun make-curve-data (curve)
  (u:if-let ((name (curve/name curve)))
    (u:if-let ((spec (u:href =meta/curves= name)))
      (let* ((data (curve:make-curve (flip-curve-points curve)
                                     :divisions (curve/divisions curve)))
             (even-spacing (curve/even-spacing curve))
             (segments (curve:collect-segments data
                                               (curve/divisions curve)
                                               :even-spacing even-spacing)))
        (setf (curve/data curve) data
              (curve/segments curve) segments))
      (error "Curve spec ~s not defined." name))
    (error "Curve name not specified for entity: ~s." curve)))

(defun make-curve-segments (curve)
  (u:when-let ((data (curve/data curve)))
    (curve:collect-segments data
                            (curve/divisions curve)
                            :even-spacing (curve/even-spacing curve))))

(defun initialize-curve-visualization (curve)
  (when (curve/visualize curve)
    (when (or (has-component-p curve 'geometry)
              (has-component-p curve 'render))
      (error "Entity ~s has a curve to be visualized, but it must not have ~
              a geometry or render component attached." curve))
    (attach-component curve 'geometry :geometry/name 'line-segments)
    (attach-component curve 'render :render/materials '(curve))))

;;; entity hooks

(define-entity-hook :attach (entity curve)
  (make-curve-data entity)
  (initialize-curve-visualization entity))

(define-entity-hook :update (entity curve)
  (when (curve/visualize entity)
    (update-geometry entity :data (curve/segments entity) :replace t)))
