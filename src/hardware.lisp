(in-package #:pyx)

(defstruct gpu-limits
  texture-size
  ssbo-bindings)

(defstruct (hardware (:constructor %make-hardware))
  (cpu (machine-version))
  (gpu-vendor (%get-gpu-vendor))
  (gpu-device (%get-gpu-device))
  (gpu-make/model (%get-gpu-make/model))
  (gpu-version (%get-gpu-version))
  gpu-limits)

(defun make-hardware ()
  (let* ((gpu-limits (make-gpu-limits
                      :texture-size (%get-gpu/max-texture-size)
                      :ssbo-bindings (%get-gpu/max-ssbo-bindings)))
         (hardware (%make-hardware :gpu-limits gpu-limits)))
    (setf (slot-value *state* '%hardware) hardware)))

;;; GPU information

(defun %get-gpu-parameter (name)
  (or (handler-case (gl:get* name)
        (error (e)
          (declare (ignore e))
          :unavailable))
      :unavailable))

(defun %get-gpu-vendor-full ()
  (%get-gpu-parameter :vendor))

(defun %get-gpu-vendor ()
  (let ((vendor (%get-gpu-vendor-full)))
    (cond
      ((search "Intel" vendor) :intel)
      ((search "NVIDIA" vendor) :nvidia)
      ((search "AMD" vendor) :amd)
      ((search "ATI" vendor) :amd)
      (t :unavailable))))

(defun %get-gpu-version ()
  (%get-gpu-parameter :version))

(defun %get-gpu-device ()
  (%get-gpu-parameter :renderer))

(defun %get-gpu-make/model ()
  (let ((vendor (%get-gpu-vendor-full)))
    (case vendor
      (:unavailable "Unknown")
      (t (format nil "~a - ~a" vendor (%get-gpu-device))))))

(defun %get-gpu/max-texture-size ()
  (%get-gpu-parameter :max-texture-size))

(defun %get-gpu/max-ssbo-bindings ()
  (%get-gpu-parameter :max-shader-storage-buffer-bindings))

;;; Internal API

(defun get-gpu-limit/ssbo-bindings ()
  (gpu-limits-ssbo-bindings (hardware-gpu-limits (hardware *state*))))

(defun get-gpu-limit/texture-size ()
  (gpu-limits-texture-size (hardware-gpu-limits (hardware *state*))))

(defun check-gpu-limit/texture-size (width height)
  (let ((max (get-gpu-limit/texture-size)))
    (if (< max (max width height))
        (error "Hardware does not support the texture size: ~dx~d.~%
                Maximum supported size: ~dx~d." width height max max)
        t)))

;;; Public API

(defun get-cpu ()
  (hardware-cpu (hardware *state*)))

(defun get-gpu-vendor ()
  (hardware-gpu-vendor (hardware *state*)))

(defun get-gpu-device ()
  (hardware-gpu-device (hardware *state*)))

(defun get-gpu-make/model ()
  (hardware-gpu-make/model (hardware *state*)))

(defun get-gpu-version ()
  (hardware-gpu-version (hardware *state*)))
