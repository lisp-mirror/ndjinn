(in-package #:net.mfiano.lisp.pyx)

;;; spec

(defstruct (curve-spec
            (:constructor %make-curve-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (points nil :type list))

(defun update-curve-spec (name points)
  (let ((spec (u:href =meta/curves= name)))
    (setf (curve-spec-points spec) points)))

(defun make-curve-spec (name points)
  (let ((spec (%make-curve-spec :name name)))
    (setf (u:href =meta/curves= name) spec)
    (update-curve-spec name points)
    spec))

(defmacro define-curve (name options &body body)
  (declare (ignore options))
  (let ((points `(list ,@(mapcar (lambda (x) `(v3:vec ,@x)) body))))
    `(progn
       (unless (curve:point-count-valid-p (length ,points))
         (error "Point count invalid for curve ~a." ',name))
       (if (u:href =meta/curves= ',name)
           (update-curve-spec ',name ,points)
           (make-curve-spec ',name ,points)))))

;;; implementation

(defun evaluate-curve (entity parameter)
  (v3:+ (v3:* (curve:evaluate (curve/data entity)
                              parameter
                              :even-spacing (curve/even-spacing entity))
              (get-scale entity))
        (get-translation entity)))

(defun rescale-curve (curve &key viewport padding)
  (let* ((viewport-size (get-viewport-size viewport))
         (scale (v3:vec (v2:+ viewport-size (or padding (v2:vec))))))
    (scale-entity curve scale :replace t :force t)))

(defun flip-curve-points (entity)
  (let ((spec (u:href =meta/curves= (curve/name entity)))
        (invert (case (curve/flip entity)
                  (:x (v3:vec -1 1 1))
                  (:y (v3:vec 1 -1 1))
                  (t (v3:vec 1)))))
    (mapcar
     (lambda (x)
       (v3:* x invert))
     (if (eq (curve/flip entity) :y)
         (reverse (curve-spec-points spec))
         (curve-spec-points spec)))))
