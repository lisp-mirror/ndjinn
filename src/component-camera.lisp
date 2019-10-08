(in-package #:pyx)

(define-component camera (:before render :after xform)
  (:active-p t
   :view (m4:id)
   :projection (m4:id)
   :mode :orthographic
   :clip-near -10000.0
   :clip-far 10000.0
   :fov-y 45.0
   :zoom 1))

(defun set-camera-projection (entity)
  (%set-camera-projection entity (camera/mode entity)))

(defmethod %set-camera-projection ((entity camera) (mode (eql :orthographic)))
  (with-slots (%camera/projection %camera/clip-near %camera/clip-far) entity
    (let* ((zoom (camera/zoom entity))
           (w (/ (cfg :window-width) zoom 2))
           (h (/ (cfg :window-height) zoom 2)))
      (m4:set-projection/orthographic!
       %camera/projection (- w) w (- h) h %camera/clip-near
       %camera/clip-far))))

(defmethod %set-camera-projection ((entity camera) (mode (eql :isometric)))
  (let ((rotation (q:inverse
                   (q:rotate-euler
                    q:+id+
                    (v3:vec (- (asin (/ (sqrt 3)))) 0 (/ pi 4))))))
    (%set-camera-projection entity :orthographic)
    (initialize-rotation entity rotation)))

(defun set-camera-view (entity)
  (let* ((model (xform/model entity))
         (eye (m4:get-translation model))
         (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
         (up (m4:rotation-axis-to-vec3 model :y)))
    (m4:set-view! (camera/view entity) eye target up)))

(defun zoom-camera (entity direction)
  (with-slots (%camera/zoom) entity
    (setf %camera/zoom (a:clamp (+ %camera/zoom (/ direction 2)) 1 10))
    (set-camera-projection entity)))

(defmethod on-component-added (entity (component (eql 'camera)))
  (when (camera/active-p entity)
    (setf (slot-value *state* '%camera) entity))
  (set-camera-projection entity))

(defmethod on-update progn ((entity camera))
  (set-camera-view entity))
