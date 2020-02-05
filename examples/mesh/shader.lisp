(in-package #:pyx.examples.shader)

(defconstant +albedo+ 0)
(defconstant +ao+ 1)
(defconstant +emissive+ 2)
(defconstant +metal-roughness+ 3)
(defconstant +normal+ 4)

(defstruct material-info
  (perceptual-roughness :float :accessor perceptual-roughness)
  (reflectance0 :vec3 :accessor reflectance0)
  (alpha-roughness :float :accessor alpha-roughness)
  (diffuse-color :vec3 :accessor diffuse-color)
  (reflectance90 :vec3 :accessor reflectance90)
  (specular-color :vec3 :accessor specular-color))

(defstruct angular-info
  (n-dot-l :float :accessor n-dot-l)
  (n-dot-v :float :accessor n-dot-v)
  (n-dot-h :float :accessor n-dot-h)
  (l-dot-h :float :accessor l-dot-h)
  (v-dot-h :float :accessor v-dot-h))

(defstruct light-info
  (direction :vec3 :accessor direction)
  (color :vec3 :accessor color)
  (intensity :float :accessor intensity))

(defun get-angular-info ((point-to-light :vec3)
                         (normal :vec3)
                         (view :vec3))
  (let* ((n (normalize normal))
         (v (normalize view))
         (l (normalize point-to-light))
         (h (normalize (+ l v))))
    (make-angular-info (clamp (dot n l) 0 1)
                       (clamp (dot n v) 0 1)
                       (clamp (dot n h) 0 1)
                       (clamp (dot l h) 0 1)
                       (clamp (dot v h) 0 1))))

(defun diffuse ((material-info material-info))
  (/ (diffuse-color material-info) +pi+))

(defun specular-reflection ((material-info material-info)
                            (angular-info angular-info))
  (with-slots (reflectance0 reflectance90) material-info
    (+ reflectance0
       (* (- reflectance90 reflectance0)
          (pow (clamp (- 1 (v-dot-h angular-info)) 0 1) 5)))))

(defun visibility-occlusion ((material-info material-info)
                             (angular-info angular-info))
  (with-slots (n-dot-l n-dot-v) angular-info
    (with-slots (alpha-roughness) material-info
      (let* ((alpha-roughness-squared (* alpha-roughness alpha-roughness))
             (ggxv (* n-dot-l
                      (sqrt (+ (* n-dot-v n-dot-v (- 1 alpha-roughness-squared))
                               alpha-roughness-squared))))
             (ggxl (* n-dot-v
                      (sqrt (+ (* n-dot-l n-dot-l (- 1 alpha-roughness-squared))
                               alpha-roughness-squared))))
             (ggx (+ ggxv ggxl)))
        (if (plusp ggx)
            (/ 0.5 ggx)
            0.0)))))

(defun microfacet-distribution ((material-info material-info)
                                (angular-info angular-info))
  (with-slots (alpha-roughness) material-info
    (with-slots (n-dot-h) angular-info
      (let* ((alpha-roughness-squared (* alpha-roughness alpha-roughness))
             (f (1+ (* (- (* n-dot-h alpha-roughness-squared)
                          n-dot-h)
                       n-dot-h))))
        (/ alpha-roughness-squared (* +pi+ f f))))))

(defun get-point-shade ((point-to-light :vec3)
                        (material-info material-info)
                        (normal :vec3)
                        (view :vec3))
  (let ((angular-info (get-angular-info point-to-light normal view)))
    (if (or (plusp (n-dot-l angular-info))
            (plusp (n-dot-v angular-info)))
        (let* ((f (specular-reflection material-info angular-info))
               (vis (visibility-occlusion material-info angular-info))
               (d (microfacet-distribution material-info angular-info))
               (diffuse (* (- 1 f) (diffuse material-info)))
               (specular (* f vis d)))
          (* (n-dot-l angular-info) (+ diffuse specular)))
        (vec3 0))))

(defun get-range-attenuation ((range :float)
                              (distance :float))
  (if (<= range 0)
      1.0
      (/ (max (min (- 1 (pow (/ distance range) 4)) 1) 0)
         (pow distance 2))))

(defun get-spot-attenuation ((point-to-light :vec3)
                             (spot-direction :vec3)
                             (outer-cone-cos :float)
                             (inner-cone-cos :float))
  (let ((actual-cos (dot (normalize spot-direction)
                         (normalize (- point-to-light)))))
    (if (> actual-cos outer-cone-cos)
        (if (< actual-cos inner-cone-cos)
            (smoothstep outer-cone-cos inner-cone-cos actual-cos)
            1.0)
        0.0)))

(defun apply-directional-light ((light light-info)
                                (material-info material-info)
                                (normal :vec3)
                                (view :vec3))
  (let* ((point-to-light (- (normalize (direction light))))
         (shade (get-point-shade point-to-light material-info normal view)))
    (* (intensity light) (color light) shade)))

(defun get-normal ((sampler :sampler-2d-array)
                   (normal-scale :float)
                   (uv :vec2)
                   (world-position :vec3)
                   (tbn :mat3))
  (let* ((pos-dx (d-fdx world-position))
         (pos-dy (d-fdy world-position))
         (tex-dx (d-fdx (vec3 uv 0)))
         (tex-dy (d-fdy (vec3 uv 0)))
         (tv (/ (- (* (.t tex-dy) pos-dx)
                   (* (.t tex-dx) pos-dy))
                (- (* (.s tex-dx) (.t tex-dy))
                   (* (.s tex-dy) (.t tex-dx)))))
         (ng (cross pos-dx pos-dy))
         (n (.rgb (texture sampler (vec3 uv +normal+)))))
    (normalize (* tbn (- (* n 2) (vec3 1))
                  (vec3 normal-scale normal-scale 1)))))

(defun pbr-mesh/vert ((mesh-attrs mesh-attrs)
                      &uniforms
                      (model :mat4)
                      (view :mat4)
                      (proj :mat4)
                      (normal-matrix :mat4))
  (with-slots (mesh/pos mesh/normal mesh/tangent mesh/uv1) mesh-attrs
    (let* ((pos (* model (vec4 mesh/pos 1)))
           (world-pos (/ (.xyz pos) (.w pos)))
           (normal-w (normalize (vec3 (* normal-matrix (vec4 mesh/normal 0)))))
           (tangent-w (normalize (vec3 (* model (vec4 (.xyz mesh/tangent) 0)))))
           (bitangent-w (* (cross normal-w tangent-w)) (.w mesh/tangent))
           (tbn (mat3 tangent-w bitangent-w normal-w))
           (camera-pos (.xyz (aref (inverse view) 3))))
      (values (* proj view pos)
              world-pos
              camera-pos
              tbn
              mesh/uv1))))

(defun pbr-mesh/frag ((world-pos :vec3)
                      (camera-pos :vec3)
                      (tbn :mat3)
                      (uv :vec2)
                      &uniforms
                      (view :mat4)
                      (light light-info)
                      (sampler :sampler-2d-array)
                      (base-color-factor :vec4)
                      (metallic-factor :float)
                      (roughness-factor :float)
                      (normal-scale :float)
                      (occlusion-strength :float)
                      (emissive-factor :float))
  (let* ((uv (vec2 (.x uv) (- 1 (.y uv))))
         (f0 (vec3 0.04))
         (mr-sample (texture sampler (vec3 uv +metal-roughness+)))
         (perceptual-roughness (* (.g mr-sample) roughness-factor))
         (metallic (* (.b mr-sample) metallic-factor))
         (base-color (* (umbra.color:srgb->rgb
                         (texture sampler (vec3 uv +albedo+)))
                        base-color-factor))
         (diffuse-color (* (.rgb base-color)
                           (- (vec3 1) f0)
                           (- 1 metallic)))
         (specular-color (mix f0 (.rgb base-color) metallic))
         (perceptual-roughness (clamp perceptual-roughness 0 1))
         (metallic (clamp metallic 0 1))
         (alpha-roughness (* perceptual-roughness perceptual-roughness))
         (reflectance (max (max (.r specular-color) (.g specular-color))
                           (.b specular-color)))
         (specular-environment-r0 (.rgb specular-color))
         (specular-environment-r90 (vec3 (clamp (* reflectance 50) 0 1)))
         (material-info (make-material-info perceptual-roughness
                                            specular-environment-r0
                                            alpha-roughness
                                            diffuse-color
                                            specular-environment-r90
                                            specular-color))
         (normal (get-normal sampler normal-scale uv world-pos tbn))
         (color (apply-directional-light light material-info normal camera-pos))
         (ao (.r (texture sampler (vec3 uv +ao+))))
         (color (mix color (* color ao) occlusion-strength))
         (emissive (* (.rgb (umbra.color:srgb->rgb
                             (texture sampler (vec3 uv +emissive+))))
                      emissive-factor))
         (color (+ color emissive)))
    (vec4 (umbra.color:rgb->srgb
           (umbra.color:tone-map/aces color 1))
          (.a base-color))))

(define-shader pbr-mesh ()
  (:vertex (pbr-mesh/vert mesh-attrs))
  (:fragment (pbr-mesh/frag :vec3 :vec3 :mat3 :vec2)))