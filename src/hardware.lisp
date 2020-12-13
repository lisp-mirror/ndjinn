(in-package #:ndjinn)

(defstruct (hardware-info
            (:predicate nil)
            (:copier nil))
  (cpu "Unknown" :type string)
  (cpu-count 1 :type fixnum)
  (monitors nil :type list)
  (gpu-vendor :unavailable :type keyword)
  (gpu-device "Unknown" :type string)
  (gpu-make/model "Unknown" :type string)
  (gpu-version "Unknown" :type string)
  (max-texture-size 0 :type fixnum)
  (max-ssbo-bindings 0 :type fixnum))

(defun get-gpu-parameter (name)
  (or (handler-case (gl:get* name)
        (error (e)
          (declare (ignore e))
          "Unavailable"))
      "Unavailable"))

(defun get-gpu-vendor-full ()
  (get-gpu-parameter :vendor))

(defun get-gpu-vendor ()
  (let ((vendor (get-gpu-vendor-full)))
    (cond
      ((search "Intel" vendor) :intel)
      ((search "NVIDIA" vendor) :nvidia)
      ((search "AMD" vendor) :amd)
      ((search "ATI" vendor) :amd)
      (t :unavailable))))

(defun get-gpu-device ()
  (get-gpu-parameter :renderer))

(defun get-gpu-make/model ()
  (let ((vendor (get-gpu-vendor-full)))
    (case vendor
      (:unavailable "Unknown")
      (t (format nil "~a - ~a" vendor (get-gpu-device))))))

(defun get-gpu-version ()
  (get-gpu-parameter :version))

(defun get-gpu/max-texture-size ()
  (get-gpu-parameter :max-texture-size))

(defun get-gpu/max-ssbo-bindings ()
  (get-gpu-parameter :max-shader-storage-buffer-bindings))

(defun get-monitor-names ()
  (loop :for i :below (sdl2:get-num-video-displays)
        :collect (cons i (sdl2:get-display-name i))))

(defun load-hardware-info ()
  (log:debug :ndjinn "Reading hardware information...")
  (let* ((cpu (machine-version))
         (cpu-count (cl-cpus:get-number-of-processors))
         (monitors (get-monitor-names))
         (gpu-vendor (get-gpu-vendor))
         (gpu-device (get-gpu-device))
         (gpu-make/model (get-gpu-make/model))
         (gpu-version (get-gpu-version))
         (max-texture-size (get-gpu/max-texture-size))
         (max-ssbo-bindings (get-gpu/max-ssbo-bindings))
         (hardware-info (make-hardware-info
                         :cpu cpu
                         :cpu-count cpu-count
                         :monitors monitors
                         :gpu-vendor gpu-vendor
                         :gpu-device gpu-device
                         :gpu-make/model gpu-make/model
                         :gpu-version gpu-version
                         :max-texture-size max-texture-size
                         :max-ssbo-bindings max-ssbo-bindings)))
    (setf (hardware-info =context=) hardware-info)
    (log:debug :ndjinn "CPU: ~a (threads: ~d)" cpu cpu-count)
    (dolist (x monitors)
      (log:debug :ndjinn "Monitor ~d: ~a" (car x) (cdr x)))
    (log:debug :ndjinn "GPU: ~a (version: ~a)" gpu-make/model gpu-version)
    (log:debug :ndjinn "GPU limit - Maximum texture size: ~dx~d"
               max-texture-size
               max-texture-size)
    (log:debug :ndjinn "GPU limit - Maximum SSBO bindings: ~d"
               max-ssbo-bindings)))

(defgeneric get-hardware-info (key)
  (:method ((key (eql :cpu)))
    (hardware-info-cpu (hardware-info =context=)))
  (:method ((key (eql :cpu-count)))
    (hardware-info-cpu-count (hardware-info =context=)))
  (:method ((key (eql :monitors)))
    (hardware-info-monitors (hardware-info =context=)))
  (:method ((key (eql :gpu-vendor)))
    (hardware-info-gpu-vendor (hardware-info =context=)))
  (:method ((key (eql :gpu-device)))
    (hardware-info-gpu-device (hardware-info =context=)))
  (:method ((key (eql :gpu-make/model)))
    (hardware-info-gpu-make/model (hardware-info =context=)))
  (:method ((key (eql :gpu-version)))
    (hardware-info-gpu-version (hardware-info =context=)))
  (:method ((key (eql :max-texture-size)))
    (hardware-info-max-texture-size (hardware-info =context=)))
  (:method ((key (eql :max-ssbo-bindings)))
    (hardware-info-max-ssbo-bindings (hardware-info =context=))))
