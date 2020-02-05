(in-package #:pyx.extension)

;;; default resources

(render:define-render-pass default ()
  (:clear-color (v4:vec 0 0 0 1)
   :clear-buffers (:color :depth)))

(cd:define-collider-plan default ())

(vp:define-viewport default ()
  (:x 0 :y 0 :width 1 :height 1))

;;; built-in resources

(tex:define-texture debug ()
  (:source "data/textures/debug.png"))

(mat:define-material full-quad ()
  (:shader pyx.shader:full-quad
   :uniforms (:sampler 'debug)))

(mat:define-material quad ()
  (:shader pyx.shader:quad
   :uniforms (:sampler 'debug)))

(mat:define-material mesh ()
  (:shader pyx.shader:mesh
   :uniforms (:sampler 'debug)))

(mat:define-material collider ()
  (:shader pyx.shader:collider
   :uniforms (:hit-color (v4:vec 0 1 0 1)
              :miss-color (v4:vec 1 0 0 1))
   :features (:enable (:line-smooth)
              :polygon-mode :line
              :line-width 1.0)))
