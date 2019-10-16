(in-package #:pyx.examples)

(pyx:define-material base ()
  (:uniforms (:model (m4:id)
              :view (m4:id)
              :proj (m4:id))))
