(in-package #:net.mfiano.lisp.pyx.examples.shader)

(defun effect/toroidal-trip/check-ray ((distance :float))
  (< distance 1e-3))

(defun effect/toroidal-trip/frag (&uniforms
                                  (res :vec2)
                                  (time :float))
  (let* ((rtime (* time 0.5))
         (uv (* (/ (- (.xy gl-frag-coord) (* res 0.5)) (.y res))
                (mat2 (cos rtime) (- (sin rtime)) (sin rtime) (cos rtime))))
         (ray-origin (vec3 0 0 -1))
         (look-at (mix (vec3 0) (vec3 -1 0 -1) (sin (+ (* time 0.5) 0.5))))
         (zoom (mix 0.2 0.7 (+ (* (sin time) 0.5) 0.5)))
         (forward (normalize (- look-at ray-origin)))
         (right (normalize (cross (vec3 0 1 0) forward)))
         (up (cross forward right))
         (center (+ ray-origin (* forward zoom)))
         (intersection (+ (* (.x uv) right)
                          (* (.y uv) up)
                          center))
         (ray-direction (normalize (- intersection ray-origin)))
         (distance-surface 0.0)
         (distance-origin 0.0)
         (point (vec3 0))
         (radius (mix 0.3 0.8 (+ 0.5 (* 0.5 (sin (* time 0.4)))))))
    (dotimes (i 1000)
      (setf point (+ (* ray-direction distance-origin)
                     ray-origin)
            distance-surface (- (- (length (vec2 (1- (length (.xz point)))
                                                 (.y point)))
                                   radius)))
      (when (effect/toroidal-trip/check-ray distance-surface)
        (break))
      (incf distance-origin distance-surface))
    (let ((color (vec3 0)))
      (when (effect/toroidal-trip/check-ray distance-surface)
        (let* ((x (+ (atan (.x point) (- (.z point))) (* time 0.4)))
               (y (atan (1- (length (.xz point))) (.y point)))
               (ripples (+ (* (sin (* (+ (* y 60) (- (* x 20))) 3)) 0.5) 0.5))
               (waves (sin (+ (* x 2) (+ (- (* y 6)) (* time 5)))))
               (bands (sin (+ (* x 30) (* y 10))))
               (b1 (smoothstep -0.2 0.2 bands))
               (b2 (smoothstep -0.2 0.2 (- bands 0.5)))
               (noise (vec3 (+ (* 0.4 (umbra.noise:perlin (* (vec2 x y) 10)))
                               (* 0.7 (umbra.noise:cellular (* (vec2 x y) 50)))
                               (* 0.3 (umbra.noise:perlin-surflet
                                       (* (vec2 x y) 200))))))
               (noise (umbra.color:color-filter noise
                                                (vec3 (sin x) (sin y) 0.5)
                                                1))
               (blend (+ (max (* b1 (- 1 b2)) (* ripples b2 waves))
                         (* waves 0.5 b2)))
               (blend (mix blend
                           (- 1 blend)
                           (smoothstep -0.3 0.3 (sin (+ (* x 2) time))))))
          (setf color (mix (vec3 blend) noise 0.5))))
      (vec4 color 1))))

(define-shader effect/toroidal-trip ()
  (:vertex (full-quad-no-uv/vert :vec3))
  (:fragment (effect/toroidal-trip/frag)))
