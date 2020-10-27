(in-package #:net.mfiano.lisp.pyx)

(define-component camera ()
  ((%camera/active-p :reader camera/active-p
                     :initarg :camera/active-p
                     :initform t)
   (%camera/mode :reader camera/mode
                 :initarg :camera/mode
                 :initform :perspective)
   (%camera/viewport :accessor camera/viewport
                     :initarg :camera/viewport
                     :initform 'default)
   (%camera/clip-near :accessor camera/clip-near
                      :initarg :camera/clip-near
                      :initform 0.1)
   (%camera/clip-far :accessor camera/clip-far
                     :initarg :camera/clip-far
                     :initform 1024f0)
   (%camera/fov-y :accessor camera/fov-y
                  :initarg :camera/fov-y
                  :initform 45f0)
   (%camera/zoom :reader camera/zoom
                 :initarg :camera/zoom
                 :initform 1f0)
   (%camera/target :reader camera/target
                   :initarg :camera/target
                   :initform nil)
   (%camera/target-z-axis :reader camera/target-z-axis
                          :initarg :camera/target-z-axis
                          :initform nil)
   (%camera/translate-view :reader camera/translate-view
                           :initarg :camera/translate-view
                           :initform t)
   (%camera/view :reader camera/view
                 :initform (m4:mat 1))
   (%camera/projection :reader camera/projection
                       :initform (m4:mat 1))
   (%camera/free-look :accessor camera/free-look
                      :initarg :camera/free-look
                      :initform nil)
   (%camera/free-look-state :accessor camera/free-look-state
                            :initform nil)
   (%camera/zoom-p :accessor camera/zoom-p
                   :initform nil))
  (:type-order :before render :after transform))

(defun set-camera-projection (entity)
  (%set-camera-projection entity (camera/mode entity)))

(defmethod %set-camera-projection ((entity camera) (mode (eql :perspective)))
  (with-slots (%camera/viewport) entity
    (m4:set-projection/perspective! (camera/projection entity)
                                    (/ (camera/fov-y entity)
                                       (camera/zoom entity))
                                    (/ (width %camera/viewport)
                                       (height %camera/viewport))
                                    (camera/clip-near entity)
                                    (camera/clip-far entity))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%camera/zoom %camera/viewport) entity
    (let ((w (/ (width %camera/viewport) %camera/zoom 2))
          (h (/ (height %camera/viewport) %camera/zoom 2)))
      (m4:set-projection/orthographic! (camera/projection entity)
                                       (- w)
                                       w
                                       (- h)
                                       h
                                       (camera/clip-near entity)
                                       (camera/clip-far entity)))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :isometric)))
  (let ((rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec (- (asin (/ (sqrt 3)))) 0 math:pi/4)))))
    (%set-camera-projection entity :orthographic)
    (initialize-rotation (transform/rotation entity) rotation)))

(defun set-camera-view (entity)
  (with-slots (%camera/view %camera/target) entity
    (let* ((free-look-state (camera/free-look-state entity))
           (model (m4:copy (transform/model entity)))
           (target (camera/target entity))
           (eye (if target
                    (v3:with-components ((v (m4:get-translation
                                             (transform/model target))))
                      (v3:+ (m4:get-translation model)
                            (if (camera/target-z-axis entity)
                                v
                                (v3:vec vx vy))))
                    (m4:get-translation model)))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! %camera/view eye target up)
      (unless (camera/translate-view entity)
        (m4:set-translation! %camera/view %camera/view v3:+zero+))
      (when free-look-state
        (set-initial-free-look-orientation free-look-state model)))))

(defun get-current-camera ()
  (u:when-let ((viewport (active (get-viewport-manager))))
    (camera viewport)))

(defun zoom-camera (entity direction min max)
  (with-slots (%camera/zoom) entity
    (setf %camera/zoom (u:clamp (+ %camera/zoom (/ direction 2)) min max))))

(defun get-camera-zoom ()
  (camera/zoom (get-current-camera)))

;;; entity hooks

(define-entity-hook :attach (entity camera)
  (let ((entity-viewport (first (get-entity-viewports entity))))
    (unless (camera/viewport entity)
      (error "Camera ~s does not have a viewport tag known to this scene."
             entity))
    (when (camera/active-p entity)
      (setf (camera entity-viewport) entity))
    (when (camera/free-look entity)
      (setf (camera/free-look-state entity) (make-free-look-state entity)))
    (setf (camera/fov-y entity) (* (camera/fov-y entity) math:+deg+)
          (camera/clip-near entity) (float (camera/clip-near entity) 1f0)
          (camera/clip-far entity) (float (camera/clip-far entity) 1f0)
          (camera/viewport entity) entity-viewport)))

(define-entity-hook :update (entity camera)
  (when (camera/free-look entity)
    (update-free-look-state (camera/free-look-state entity)))
  (set-camera-view entity)
  (set-camera-projection entity))
