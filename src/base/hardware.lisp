(in-package #:net.mfiano.lisp.pyx)

(glob:define-global-var =cpu= "Unknown")
(glob:define-global-var =cpu-count= 1)
(glob:define-global-var =gpu-vendor= "Unknown")
(glob:define-global-var =gpu-device= "Unknown")
(glob:define-global-var =gpu-make/model= "Unknown")
(glob:define-global-var =gpu-version= "Unknown")
(glob:define-global-var =max-texture-size= 0)
(glob:define-global-var =max-ssbo-bindings= 0)

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
  (setf =cpu= (machine-version)
        =cpu-count= (cl-cpus:get-number-of-processors)
        =gpu-vendor= (get-gpu-vendor)
        =gpu-device= (get-gpu-device)
        =gpu-make/model= (get-gpu-make/model)
        =gpu-version= (get-gpu-version)
        =max-texture-size= (get-gpu/max-texture-size)
        =max-ssbo-bindings= (get-gpu/max-ssbo-bindings))
  (log:debug :pyx.core "CPU: ~a (threads: ~d)" =cpu= =cpu-count=)
  (log:debug :pyx.core "GPU: ~a (version: ~a)" =gpu-make/model= =gpu-version=)
  (log:debug :pyx.core "GPU limit - Maximum texture size: ~dx~d"
             =max-texture-size=
             =max-texture-size=)
  (log:debug :pyx.core "GPU limit - Maximum SSBO bindings: ~d"
             =max-ssbo-bindings=)
  (log:debug :pyx.core "Finished reading hardware information"))

(defun get-hardware-info (key)
  (let ((global (u:format-symbol :net.mfiano.lisp.pyx "=~a=" key)))
    (when (boundp global)
      (symbol-value global))))
