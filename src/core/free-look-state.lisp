(in-package #:pyx)

(defclass free-look-state ()
  ((%camera :reader camera
            :initarg :camera)
   (%initial-state :accessor initial-state
                   :initform nil)
   (%initial-orientation :accessor initial-orientation
                         :initform (m4:mat 1))
   (%move-state :accessor move-state
                :initform (u:dict #'eq))
   (%move-speed :reader move-speed
                :initform 50f0)))

(defun make-free-look-state (camera)
  (make-instance 'free-look-state :camera camera))

(defun set-initial-free-look-orientation (state model)
  (unless (initial-state state)
    (setf (initial-orientation state) model
          (initial-state state) t)))

(defun reset-free-look-state (state)
  (let* ((model (initial-orientation state))
         (translation (m4:get-translation model))
         (rotation (q:from-mat4 model)))
    (translate-entity (camera state) translation :replace-p t)
    (rotate-entity (camera state) rotation :replace-p t)))

(defun update-free-look-move-state (state)
  (let ((move-state (move-state state)))
    (when (pyx:on-button-enter :key :backspace)
      (reset-free-look-state state))
    (cond
      ((pyx:on-button-enabled :key :w)
       (setf (u:href move-state :forward) t))
      ((pyx:on-button-exit :key :w)
       (setf (u:href move-state :forward) nil)))
    (cond
      ((pyx:on-button-enabled :key :s)
       (setf (u:href move-state :backward) t))
      ((pyx:on-button-exit :key :s)
       (setf (u:href move-state :backward) nil)))
    (cond
      ((pyx:on-button-enabled :key :a)
       (setf (u:href move-state :strafe-left) t))
      ((pyx:on-button-exit :key :a)
       (setf (u:href move-state :strafe-left) nil)))
    (cond
      ((pyx:on-button-enabled :key :d)
       (setf (u:href move-state :strafe-right) t))
      ((pyx:on-button-exit :key :d)
       (setf (u:href move-state :strafe-right) nil)))
    (cond
      ((pyx:on-button-enabled :key :left)
       (setf (u:href move-state :turn-left) t))
      ((pyx:on-button-exit :key :left)
       (setf (u:href move-state :turn-left) nil)))
    (cond
      ((pyx:on-button-enabled :key :right)
       (setf (u:href move-state :turn-right) t))
      ((pyx:on-button-exit :key :right)
       (setf (u:href move-state :turn-right) nil)))
    (cond
      ((pyx:on-button-enabled :key :pageup)
       (setf (u:href move-state :strafe-up) t))
      ((pyx:on-button-exit :key :pageup)
       (setf (u:href move-state :strafe-up) nil)))
    (cond
      ((pyx:on-button-enabled :key :pagedown)
       (setf (u:href move-state :strafe-down) t))
      ((pyx:on-button-exit :key :pagedown)
       (setf (u:href move-state :strafe-down) nil)))))

(defun update-free-look-state (state)
  (let* ((camera (camera state))
         (move-state (move-state state))
         (dt (get-frame-time)))
    (update-free-look-move-state state)
    (labels ((move (plus minus)
               (cond
                 ((u:href move-state plus) 1)
                 ((u:href move-state minus) -1)
                 (t 0)))
             (axis (rate vector)
               (v3:scale vector (* rate dt (move-speed state))))
             (axis-rotate (rate axis)
               (q:orient :local axis (* rate dt))))
      (let ((forward (axis (move :forward :backward)
                           (transform-direction camera v3:+back+)))
            (strafe (v3:+ (axis (move :strafe-left :strafe-right)
                                (transform-direction camera v3:+left+))
                          (axis (move :strafe-down :strafe-up)
                                (transform-direction camera v3:+down+))))
            (turn (axis-rotate (move :turn-left :turn-right) v3:+up+)))
        (translate-entity camera (v3:+ forward strafe))
        (rotate-entity camera turn)))))
