(in-package #:ndjinn)

(defstruct (free-look-state
            (:constructor %make-free-look-state)
            (:predicate nil)
            (:copier nil))
  camera
  (initial-state nil :type boolean)
  (initial-orientation (m4:mat 1) :type m4:mat)
  (key-state (u:dict #'eq) :type hash-table)
  (key-speed 30f0 :type single-float)
  (mouse-sensitivity 5f0 :type single-float))

(defun make-free-look-state (camera)
  (%make-free-look-state :camera camera))

(defun set-initial-free-look-orientation (state model)
  (unless (free-look-state-initial-state state)
    (setf (free-look-state-initial-orientation state) model
          (free-look-state-initial-state state) t)))

(defun reset-free-look-state (state)
  (let* ((model (free-look-state-initial-orientation state))
         (translation (m4:get-translation model))
         (rotation (q:from-mat4 model))
         (camera (free-look-state-camera state)))
    (translate-entity camera translation :replace t)
    (rotate-entity camera rotation :replace t)))

(defun update-free-look-key-state (state)
  (let ((key-state (free-look-state-key-state state)))
    (when (on-button-enter :key :backspace)
      (reset-free-look-state state))
    (cond
      ((on-button-enabled :key :w)
       (setf (u:href key-state :forward) t))
      ((on-button-exit :key :w)
       (setf (u:href key-state :forward) nil)))
    (cond
      ((on-button-enabled :key :s)
       (setf (u:href key-state :backward) t))
      ((on-button-exit :key :s)
       (setf (u:href key-state :backward) nil)))
    (cond
      ((on-button-enabled :key :a)
       (setf (u:href key-state :strafe-left) t))
      ((on-button-exit :key :a)
       (setf (u:href key-state :strafe-left) nil)))
    (cond
      ((on-button-enabled :key :d)
       (setf (u:href key-state :strafe-right) t))
      ((on-button-exit :key :d)
       (setf (u:href key-state :strafe-right) nil)))
    (cond
      ((on-button-enabled :key :left)
       (setf (u:href key-state :turn-left) t))
      ((on-button-exit :key :left)
       (setf (u:href key-state :turn-left) nil)))
    (cond
      ((on-button-enabled :key :right)
       (setf (u:href key-state :turn-right) t))
      ((on-button-exit :key :right)
       (setf (u:href key-state :turn-right) nil)))
    (cond
      ((on-button-enabled :key :pageup)
       (setf (u:href key-state :strafe-up) t))
      ((on-button-exit :key :pageup)
       (setf (u:href key-state :strafe-up) nil)))
    (cond
      ((on-button-enabled :key :pagedown)
       (setf (u:href key-state :strafe-down) t))
      ((on-button-exit :key :pagedown)
       (setf (u:href key-state :strafe-down) nil)))))

(defun free-look/key-move (camera key-state speed)
  (flet ((velocity (direction plus minus)
           (v3:scale (transform-direction camera direction)
                     (cond
                       ((u:href key-state plus) speed)
                       ((u:href key-state minus) (- speed))
                       (t 0f0)))))
    (let* ((x (velocity v3:+right+ :strafe-right :strafe-left))
           (y (velocity v3:+up+ :strafe-up :strafe-down))
           (z (velocity v3:+back+ :forward :backward))
           (vec (v3:+ (v3:+ x y) z))
           (angle (velocity v3:+up+ :turn-left :turn-right)))
      (translate-entity camera vec)
      (rotate-entity/velocity camera angle 1f0))))

(defun free-look/mouse-move (camera speed)
  (flet ((velocity (direction x)
           (v3:scale (transform-direction camera direction)
                     (* x speed))))
    (u:mvlet* ((mx my dx dy (get-mouse-position))
               (x (velocity v3:+right+ dx))
               (y (velocity v3:+up+ dy))
               (z (velocity v3:+forward+ dy))
               (vec (v3:+ x y))
               (angle (v3:+ (velocity v3:+down+ dx)
                            (velocity v3:+right+ dy))))
      (when (or (on-button-enter :key :lshift)
                (on-button-enter :key :lalt)
                (on-button-enter :key :lctrl))
        (enable-relative-motion))
      (when (on-button-enabled :key :lshift)
        (translate-entity camera vec))
      (when (on-button-enabled :key :lalt)
        (translate-entity camera z))
      (when (on-button-enabled :key :lctrl)
        (rotate-entity/velocity camera angle 1f0))
      (when (or (on-button-exit :key :lshift)
                (on-button-exit :key :lalt)
                (on-button-exit :key :lctrl))
        (disable-relative-motion)))))

(defun update-free-look-state (state)
  (u:mvlet* ((camera (free-look-state-camera state))
             (key-speed (* (free-look-state-key-speed state)
                           (get-frame-time)))
             (mouse-speed (* (free-look-state-mouse-sensitivity state)
                             (get-frame-time))))
    (update-free-look-key-state state)
    (free-look/key-move camera (free-look-state-key-state state) key-speed)
    (free-look/mouse-move camera mouse-speed)))