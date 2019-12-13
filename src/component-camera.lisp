(in-package #:pyx)

(define-component camera ()
  ((%camera/active-p :reader camera/active-p
                     :initarg :camera/active-p
                     :initform t)
   (%camera/mode :reader camera/mode
                 :initarg :camera/mode
                 :initform :perspective)
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
               %camera/clip-far)
      entity
    (let ((aspect-ratio (float (/ (cfg :window-width)
                                  (cfg :window-height))
                               1f0)))
      (m4:set-projection/perspective! %camera/projection
                                      (/ %camera/fov-y %camera/zoom)
                                      aspect-ratio
                                      %camera/clip-near
                                      %camera/clip-far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%camera/projection %camera/zoom %camera/clip-near
               %camera/clip-far)
      entity
    (let ((w (float (/ (cfg :window-width) %camera/zoom 2) 1f0))
          (h (float (/ (cfg :window-height) %camera/zoom 2) 1f0))
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
                            (float (/ pi 4) 1f0))))))
    (%set-camera-projection entity :orthographic)
    (initialize-rotation entity rotation)))

(defun set-camera-view (entity)
  (with-slots (%camera/target %camera/target-z-axis-p) entity
    (let* ((model (xform/model entity))
           (eye (if %camera/target
                    (v3:with-components ((v (m4:get-translation
                                             (xform/model %camera/target))))
                      (v3:+ (m4:get-translation model)
                            (if %camera/target-z-axis-p v (v3:vec vx vy 0f0))))
                    (m4:get-translation model)))
           (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
           (up (m4:rotation-axis-to-vec3 model :y)))
      (m4:set-view! (camera/view entity) eye target up))))

(defun zoom-camera (entity direction)
  (with-slots (%camera/zoom) entity
    (setf %camera/zoom (a:clamp (+ %camera/zoom (/ direction 2)) 1 10))
    (set-camera-projection entity)))

;;; entity hooks

(define-hook :entity-create (entity camera)
  (when camera/active-p
    (setf (slot-value (current-scene *state*) '%camera) entity))
  (setf camera/fov-y (float (* camera/fov-y (/ pi 180)) 1f0))
  (set-camera-projection entity))

(define-hook :entity-update (entity camera)
  (set-camera-view entity))
