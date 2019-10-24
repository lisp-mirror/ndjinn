(in-package #:pyx.examples)

(pyx:define-material base ()
  (:uniforms (:model (m4:id)
              :view (m4:id)
              :proj (m4:id))))

(pyx:define-prototype camera/orthographic ()
  (pyx:camera :mode :orthographic))

(pyx:define-prototype camera/isometric ()
  (pyx:camera :mode :isometric))
