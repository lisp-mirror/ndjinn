(in-package #:%pyx.viewport)

(defstruct (manager (:conc-name nil)
                    (:predicate nil)
                    (:copier nil))
  (table (u:dict #'eq))
  active
  default)

(defstruct (viewport (:constructor %make-viewport)
                     (:conc-name nil)
                     (:predicate nil)
                     (:copier nil))
  spec
  camera
  draw-order
  picking-ray
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(u:define-printer (viewport stream :identity t)
  (format stream "~s" (spec-name (spec viewport))))

(defun make-viewport (name order ray)
  (let* ((spec (u:href meta:=viewports= name))
         (viewport (%make-viewport :spec spec
                                   :draw-order order
                                   :picking-ray ray)))
    (configure viewport)
    viewport))

(defun get-manager ()
  (scene:viewports (ctx:current-scene)))

(defun get-by-coordinates (x y)
  (let ((manager (get-manager)))
    (u:do-hash-values (v (table manager))
      (when (and (<= (x v) x (+ (x v) (width v)))
                 (<= (y v) y (+ (y v) (height v))))
        (return-from get-by-coordinates v)))
    (default manager)))

(defun configure (viewport)
  (let ((spec (spec viewport)))
    (setf (x viewport) (a:lerp (spec-x spec) 0 cfg:=window-width=)
          (y viewport) (a:lerp (spec-y spec) 0 cfg:=window-height=)
          (width viewport) (a:lerp (spec-width spec) 0 cfg:=window-width=)
          (height viewport) (a:lerp (spec-height spec) 0 cfg:=window-height=)))
  (gl:viewport (x viewport)
               (y viewport)
               (width viewport)
               (height viewport)))

(defun get-entity-viewports (entity)
  (let ((scene-viewports (get-manager))
        (viewports nil))
    (dolist (id (c/id:views entity))
      (let ((viewport (u:href (table scene-viewports) id)))
        (pushnew viewport viewports)))
    (or viewports (list (default scene-viewports)))))

(util::on-recompile :viewport data ()
  (util::recompile :scene (scene:get-scene-name)))

;;; Public API

(defun get-viewport-dimensions ()
  (let* ((manager (get-manager))
         (viewport (or (active manager)
                       (default manager))))
    (v2:vec (width viewport) (height viewport))))
