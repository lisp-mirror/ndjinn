(in-package #:ndjinn)

;;; defaults

(define-config default ()
  (:anti-alias t
   :delta-time 1/60
   :log-asset-pool nil
   :log-repl-level :debug
   :log-repl-categories (:ndjinn)
   :opengl-version "3.3"
   :project nil
   :scene nil
   :title "Ndjinn"))

(define-collision-plan default ())

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

(define-material curve ()
  (:shader shader:default
   :uniforms (:color (v3:vec 0 0.4 0.7)
              :opacity 1)
   :features (:line-width 3
              :depth-mode :lequal)))

;;; geometry

(define-geometry-layout text ()
  (:data (:format :interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))

(define-geometry text ()
  (:layout text
   :vertex-count 6
   :primitive :triangles))

(define-geometry-layout point ()
  (:data (:format :interleaved)
         (position :type float :count 3)))

(define-geometry line-segments ()
  (:layout point
   :vertex-count 2
   :primitive :lines))
