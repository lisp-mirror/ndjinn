(in-package #:net.mfiano.lisp.pyx)

(defstruct (hardware-info
            (:predicate nil)
            (:copier nil))
  (cpu "Unknown" :type string)
  (cpu-count 1 :type fixnum)
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

(defun load-hardware-info ()
  (log:debug :pyx.core "Reading hardware information...")
  (let* ((cpu (machine-version))
         (cpu-count (cl-cpus:get-number-of-processors))
         (gpu-vendor (get-gpu-vendor))
         (gpu-device (get-gpu-device))
         (gpu-make/model (get-gpu-make/model))
         (gpu-version (get-gpu-version))
         (max-texture-size (get-gpu/max-texture-size))
         (max-ssbo-bindings (get-gpu/max-ssbo-bindings))
         (hardware-info (make-hardware-info
                         :cpu cpu
                         :cpu-count cpu-count
                         :gpu-vendor gpu-vendor
                         :gpu-device gpu-device
                         :gpu-make/model gpu-make/model
                         :gpu-version gpu-version
                         :max-texture-size max-texture-size
                         :max-ssbo-bindings max-ssbo-bindings)))
    (setf (hardware-info =context=) hardware-info)
    (log:debug :pyx.core "CPU: ~a (threads: ~d)" cpu cpu-count)
    (log:debug :pyx.core "GPU: ~a (version: ~a)" gpu-make/model gpu-version)
    (log:debug :pyx.core "GPU limit - Maximum texture size: ~dx~d"
               max-texture-size
               max-texture-size)
    (log:debug :pyx.core "GPU limit - Maximum SSBO bindings: ~d"
               max-ssbo-bindings)
    (log:debug :pyx.core "Finished reading hardware information")))

(defun get-hardware-info (key)
  (let ((global (u:format-symbol :net.mfiano.lisp.pyx "=~a=" key)))
    (when (boundp global)
      (symbol-value global))))
