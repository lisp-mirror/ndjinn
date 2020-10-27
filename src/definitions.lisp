(in-package #:net.mfiano.lisp.pyx)

;;; defaults

(define-config default ()
  (:anti-alias t
   :delta-time 1/60
   :log-asset-pool nil
   :log-repl-level :debug
   :log-repl-categories (:pyx)
   :opengl-version "4.3"
   :title "Pyx Engine"
   :vsync t))

(define-collider-plan default ())

(define-viewport default ()
  (:x 0 :y 0 :width 1 :height 1))

(define-render-pass default ()
  (:clear-color (v4:vec 0 0 0 1)
   :clear-buffers (:color :depth)))

;;; materials

(define-material collider ()
  (:shader shader:collider
   :uniforms (:hit-color (v4:vec 0 1 0 0.35)
              :miss-color (v4:vec 1 0 0 0.35))
   :features (:enable (:line-smooth)
              :polygon-mode :line
              :line-width 4.0)))

;;; geometry

(define-geometry-layout text ()
  (:data (:format :interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))
