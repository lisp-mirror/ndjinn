(in-package #:%pyx.component.camera)

(ent:define-component camera ()
  ((%active-p :reader active-p
              :initarg :camera/active-p
              :initform t)
   (%debug-p :reader debug-p
             :initarg :camera/debug
             :initform nil)
   (%debug-speed :reader debug-speed
                 :initarg :camera/debug-speed
                 :initform 1f0)
   (%mode :reader mode
          :initarg :camera/mode
          :initform :perspective)
   (%viewport :accessor viewport
              :initarg :camera/viewport
              :initform 'ext:default)
   (%clip-near :reader clip-near
               :initarg :camera/clip-near
               :initform 0.1)
   (%clip-far :reader clip-far
              :initarg :camera/clip-far
              :initform 1024f0)
   (%fov-y :accessor fov-y
           :initarg :camera/fov-y
           :initform 45f0)
   (%zoom :reader zoom
          :initarg :camera/zoom
          :initform 1)
   (%target :reader target
            :initarg :camera/target
            :initform nil)
   (%target-z-axis :reader target-z-axis
                   :initarg :camera/target-z-axis
                   :initform nil)
   (%translate-view :reader translate-view
                    :initarg :camera/translate-view
                    :initform t)
   (%view :reader view
          :initform (m4:mat 1))
   (%projection :reader projection
                :initform (m4:mat 1)))
  (:sorting :before render :after c/transform:transform))

(defun set-camera-projection (entity)
  (%set-camera-projection entity (mode entity)))

(defmethod %set-camera-projection ((entity camera) (mode (eql :perspective)))
  (with-slots (%projection %fov-y %zoom %clip-near %clip-far %viewport) entity
    (let ((aspect-ratio (/ (vp:width %viewport) (vp:height %viewport))))
      (m4:set-projection/perspective! %projection
                                      (/ %fov-y %zoom)
                                      aspect-ratio
                                      %clip-near
                                      %clip-far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%projection %zoom %clip-near %clip-far %viewport) entity
    (let ((w (/ (vp:width %viewport) %zoom 2))
          (h (/ (vp:height %viewport) %zoom 2))
          (clip/near (float %clip-near 1f0))
          (clip/far (float %clip-far 1f0)))
      (m4:set-projection/orthographic!
       %projection (- w) w (- h) h clip/near clip/far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :isometric)))
  (let ((rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec (- (asin (/ (sqrt 3)))) 0 math:pi/4)))))
    (%set-camera-projection entity :orthographic)
    (tfm:initialize-rotation (c/transform:rotation entity) rotation)))

(defun set-camera-view (entity)
  (with-slots (%view %target %target-z-axis %viewport %translate-view) entity
    (let* ((model (c/transform:model entity))
           (eye (if %target
                    (v3:with-components ((v (m4:get-translation
                                             (c/transform:model %target))))
                      (v3:+ (m4:get-translation model)
                            (if %target-z-axis v (v3:vec vx vy))))
                    (m4:get-translation model)))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! %view eye target up)
      (unless %translate-view
        (m4:set-translation! %view %view v3:+zero+))
      (vp:configure %viewport))))

(defun zoom-camera (entity direction)
  (with-slots (%zoom) entity
    (setf %zoom (a:clamp (+ %zoom (/ direction 2)) 1 10))
    (set-camera-projection entity)))

;;; TODO: This is just a quick hack to be able to translate the camera for
;;; debugging purposes. Figure out a proper camera controlling system.
(defun camera-debug-transform (entity)
  (with-slots (%debug-p %debug-speed) entity
    (u:mvlet* ((x y dx dy (in:get-mouse-position))
               (viewport (vp:get-by-coordinates x y)))
      (when (and %debug-p (eq entity (vp:camera viewport)))
        (when (in:on-button-enabled :key :lctrl)
          (c/transform:translate-entity entity (v3:vec (* dx %debug-speed)
                                                       (* dy %debug-speed))))
        (when (in:on-button-enabled :key :lalt)
          (c/transform:translate-entity entity
                                        (v3:vec 0 0 (* dy %debug-speed))))))))

(defun get-current-camera ()
  (vp:camera (vp:active (vp:get-manager))))

;;; entity hooks

(ent:define-entity-hook :attach (entity camera)
  (let ((entity-viewport (first (vp:get-entity-viewports entity))))
    (unless viewport
      (error "Camera ~s does not have a viewport tag known to this scene."
             entity))
    (when active-p
      (setf (vp:camera entity-viewport) entity))
    (setf fov-y (float (* fov-y (/ pi 180)) 1f0)
          viewport entity-viewport)
    (set-camera-projection entity)))

(ent:define-entity-hook :update (entity camera)
  (camera-debug-transform entity)
  (set-camera-view entity))
