(in-package #:pyx)

(define-component camera ()
  ((%camera/active-p :reader camera/active-p
                     :initarg :camera/active-p
                     :initform t)
   (%camera/debug :reader camera/debug
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
                     :initform 'default)
   (%camera/clip-near :reader camera/clip-near
                      :initarg :camera/clip-near
                      :initform 0.1)
   (%camera/clip-far :reader camera/clip-far
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
   (%camera/target-z-axis-p :reader camera/target-z-axis-p
                            :initarg :camera/target-z-axis-p
                            :initform nil)
   (%camera/view :reader camera/view
                 :initform (m4:id))
   (%camera/projection :reader camera/projection
                       :initform (m4:id)))
  (:sorting :before render :after xform))

(defun set-camera-projection (entity)
  (%set-camera-projection entity (camera/mode entity)))

(defmethod %set-camera-projection ((entity camera) (mode (eql :perspective)))
  (with-slots (%camera/projection %camera/fov-y %camera/zoom %camera/clip-near
               %camera/clip-far %camera/viewport)
      entity
    (let ((aspect-ratio (/ (width %camera/viewport) (height %camera/viewport))))
      (m4:set-projection/perspective! %camera/projection
                                      (/ %camera/fov-y %camera/zoom)
                                      aspect-ratio
                                      %camera/clip-near
                                      %camera/clip-far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%camera/projection %camera/zoom %camera/clip-near
               %camera/clip-far %camera/viewport)
      entity
    (let ((w (/ (width %camera/viewport) %camera/zoom 2))
          (h (/ (height %camera/viewport) %camera/zoom 2))
          (clip/near (float %camera/clip-near 1f0))
          (clip/far (float %camera/clip-far 1f0)))
      (m4:set-projection/orthographic!
       %camera/projection (- w) w (- h) h clip/near clip/far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :isometric)))
  (let ((rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec (float (- (asin (/ (sqrt 3)))) 1f0)
                            0f0
                            math:pi/4)))))
    (%set-camera-projection entity :orthographic)
    (initialize-rotation entity rotation)))

(defun set-camera-view (entity)
  (with-slots (%camera/target %camera/target-z-axis-p %camera/viewport) entity
    (let* ((model (xform/model entity))
           (eye (if %camera/target
                    (v3:with-components ((v (m4:get-translation
                                             (xform/model %camera/target))))
                      (v3:+ (m4:get-translation model)
                            (if %camera/target-z-axis-p v (v3:vec vx vy 0f0))))
                    (m4:get-translation model)))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! (camera/view entity) eye target up)
      (configure-viewport %camera/viewport))))

(defun zoom-camera (entity direction)
  (with-slots (%camera/zoom) entity
    (setf %camera/zoom (a:clamp (+ %camera/zoom (/ direction 2)) 1 10))
    (set-camera-projection entity)))

;;; TODO: This is just a quick hack to be able to translate the camera for
;;; debugging purposes. Figure out a proper camera controlling system.
(defun camera-debug-transform (entity)
  (with-slots (%camera/debug %camera/debug-speed) entity
    (u:mvlet ((x y dx dy viewport (get-mouse-position)))
      (when (and %camera/debug (eq entity (camera viewport)))
        (when (input-enabled-p :key :lctrl)
          (translate-entity entity (v3:vec (* dx %camera/debug-speed)
                                           (/ dy %camera/debug-speed)
                                           0f0)))
        (when (input-enabled-p :key :lalt)
          (translate-entity entity (v3:vec 0f0 0f0 dy)))))))

;;; entity hooks

(define-hook :attach (entity camera)
  (let ((viewport (first (get-entity-viewports entity))))
    (unless viewport
      (error "Camera ~s does not have a viewport tag known to this scene."
             entity))
    (when camera/active-p
      (setf (camera viewport) entity))
    (setf camera/fov-y (float (* camera/fov-y (/ pi 180)) 1f0)
          camera/viewport viewport)
    (set-camera-projection entity)))

(define-hook :update (entity camera)
  (camera-debug-transform entity)
  (set-camera-view entity))
