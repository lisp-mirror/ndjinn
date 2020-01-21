(in-package #:pyx)

;;; textures

(define-texture debug ()
  (:source "debug.png"))

;;; materials

(define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

(define-material mesh ()
  (:shader pyx.shader:mesh
   :uniforms (:sampler 'debug)))

(define-material collider ()
  (:shader pyx.shader:collider
   :uniforms (:hit-color (v4:vec 0f0 1f0 0f0 1f0)
              :miss-color (v4:vec 1f0 0f0 0f0 1f0))
   :features (:enable (:line-smooth)
              :polygon-mode :line
              :line-width 1.0)))

;;; collider plans

(define-collider-plan default ())

;;; render passes

(define-render-pass default ()
  (:clear-color (v4:vec 0f0 0f0 0f0 1f0)
   :clear-buffers (:color :depth)))

;;; views

(define-view default ()
  (:x 0f0 :y 0f0 :width 1f0 :height 1f0))
