(in-package #:pyx)

(a:define-constant +gl-capabilities/enabled+
    '(:blend :cull-face :depth-test :dither :multisample)
  :test #'equal)

(a:define-constant +gl-capabilities/disabled+
    '(:clip-distance0 :clip-distance1 :clip-distance2 :clip-distance3
      :clip-distance4 :clip-distance5 :clip-distance6 :clip-distance7
      :color-logic-op :debug-output :debug-output-synchronous :depth-clamp
      :framebuffer-srgb :line-smooth :polygon-offset-fill :polygon-offset-line
      :polygon-offset-point :polygon-smooth :primitive-restart
      :primitive-restart-fixed-index :rasterizer-discard
      :sample-alpha-to-coverage :sample-alpha-to-one
      :sample-coverage :sample-shading :sample-mask :scissor-test :stencil-test
      :texture-cube-map-seamless :program-point-size)
  :test #'equal)
