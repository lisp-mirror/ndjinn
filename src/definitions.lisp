(in-package #:pyx)

;;; textures

(define-texture debug ()
  (:source "debug.png"))

;;; materials

(define-material mesh ()
  (:shader pyx.shader:mesh
   :uniforms (:sampler 'debug)
   :features (:enable ()
              :polygon-mode :line
              :line-width 0.5)))

(define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

;;; collider plans

(define-collider-plan :default ())

;;; render passes

(define-render-pass :default ()
  (:clear-color (v4:vec 0f0 0f0 0f0 1f0)
   :clear-buffers (:color :depth)))
