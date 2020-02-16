(in-package #:pyx.component)

(pyx:define-component camera ()
  ((%camera/active-p :reader camera/active-p
                     :initarg :camera/active-p
                     :initform t)
   (%camera/debug-p :reader camera/debug-p
                    :initarg :camera/debug
                    :initform nil)
   (%camera/debug-speed :reader camera/debug-speed
                        :initarg :camera/debug-speed
                        :initform 1f0)
   (%camera/mode :reader camera/mode
                 :initarg :camera/mode
                 :initform :perspective)
   (%camera/viewport :accessor camera/viewport
                     :initarg :camera/viewport
                     :initform 'pyx::default)
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
                 :initform 1)
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
                       :initform (m4:mat 1)))
  (:sorting :before render :after transform))

(defun set-camera-projection (entity)
  (%set-camera-projection entity (camera/mode entity)))

(defmethod %set-camera-projection ((entity camera) (mode (eql :perspective)))
  (with-slots (%camera/viewport) entity
    (m4:set-projection/perspective! (camera/projection entity)
                                    (/ (camera/fov-y entity)
                                       (camera/zoom entity))
                                    (/ (pyx::width %camera/viewport)
                                       (pyx::height %camera/viewport))
                                    (camera/clip-near entity)
                                    (camera/clip-far entity))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%camera/zoom %camera/viewport) entity
    (let ((w (/ (pyx::width %camera/viewport) %camera/zoom 2))
          (h (/ (pyx::height %camera/viewport) %camera/zoom 2)))
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
    (pyx::initialize-rotation (transform/rotation entity) rotation)))

(defun set-camera-view (entity)
  (with-slots (%camera/view %camera/target) entity
    (let* ((model (transform/model entity))
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
      (pyx::configure-viewport (camera/viewport entity)))))

(defun zoom-camera (entity direction)
  (with-slots (%camera/zoom) entity
    (setf %camera/zoom (a:clamp (+ %camera/zoom (/ direction 2)) 1 10))
    (set-camera-projection entity)))

;;; TODO: This is just a quick hack to be able to translate the camera for
;;; debugging purposes. Figure out a proper camera controlling system.
(defun camera-debug-transform (entity)
  (u:mvlet* ((x y dx dy (pyx:get-mouse-position))
             (viewport (pyx::get-viewport-by-coordinates x y))
             (speed (camera/debug-speed entity)))
    (when (and (camera/debug-p entity) (eq entity (pyx::camera viewport)))
      (when (pyx:on-button-enabled :key :lctrl)
        (translate-entity entity (v3:vec (* dx speed) (* dy speed))))
      (when (pyx:on-button-enabled :key :lalt)
        (translate-entity entity (v3:vec 0 0 (* dy speed)))))))

(defun get-current-camera ()
  (pyx::camera (pyx::active (pyx::get-viewport-manager))))

;;; entity hooks

(pyx:define-entity-hook :attach (entity camera)
  (let ((entity-viewport (first (pyx::get-entity-viewports entity))))
    (unless camera/viewport
      (error "Camera ~s does not have a viewport tag known to this scene."
             entity))
    (when camera/active-p
      (setf (pyx::camera entity-viewport) entity))
    (setf camera/fov-y (float (* camera/fov-y (/ pi 180)) 1f0)
          camera/clip-near (float camera/clip-near 1f0)
          camera/clip-far (float camera/clip-far 1f0)
          camera/viewport entity-viewport)
    (set-camera-projection entity)))

(pyx:define-entity-hook :update (entity camera)
  (camera-debug-transform entity)
  (set-camera-view entity))
