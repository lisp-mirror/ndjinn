(in-package #:%pyx.package)

(define-package #:pyx.extension
  (:use #:cl)
  (:inherit-from #:%pyx.animation
                 #:opacity
                 #:sprite)
  ;; asset pools
  (:export
   #:meshes
   #:metadata)
  ;; textures
  (:export
   #:brdf-lut
   #:debug
   #:papermill-diffuse
   #:papermill-specular
   #:sky)
  ;; materials
  (:export
   #:debug
   #:full-quad
   #:mesh
   #:quad)
  ;; meshes
  (:export
   #:plane)
  ;; font
  (:export
   #:font))
