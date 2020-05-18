(in-package #:net.mfiano.lisp.pyx.examples.shader)

(defstruct world-data
  (width :uint :accessor width)
  (height :uint :accessor height)
  (cells/floor (:uvec2 75625) :accessor cells/floor)
  (cells/wall (:uvec2 75625) :accessor cells/wall)
  (cells/door/v (:uvec2 75625) :accessor cells/door/v)
  (cells/door/h (:uvec2 75625) :accessor cells/door/h))

(defun get-cell-coords ((world world-data)
                        (cell-type :int))
  (with-slots (cells/floor cells/wall cells/door/v cells/door/h) world
    (case cell-type
      (0 (aref cells/floor gl-instance-id))
      (1 (aref cells/wall gl-instance-id))
      (2 (aref cells/door/v gl-instance-id))
      (3 (aref cells/door/h gl-instance-id))
      (otherwise (aref cells/floor gl-instance-id)))))

(defstruct light/directional
  (position :vec3 :accessor position)
  (ambient :vec4 :accessor ambient)
  (diffuse :vec4 :accessor diffuse)
  (specular :vec4 :accessor specular))

(defstruct material-data
  (ambient :vec4 :accessor ambient)
  (diffuse :vec4 :accessor diffuse)
  (specular :vec4 :accessor specular)
  (shininess :float :accessor shininess))

(defun world/v ((mesh-attrs mesh-attrs)
                &uniforms
                (model :mat4)
                (view :mat4)
                (proj :mat4)
                (cell-type :int)
                (world world-data :ssbo :std-430))
  (with-slots (mesh/pos mesh/normal) mesh-attrs
    (with-slots (width height) world
      (let* ((normal-mat (transpose (inverse (mat3 (* view model)))))
             (world-normal (normalize (* normal-mat mesh/normal)))
             (to-camera (normalize (.xyz (aref (inverse view) 2))))
             (map-size (vec2 width height))
             (coords (get-cell-coords world cell-type))
             (offset (vec3 (- (* coords 2) map-size) 0))
             (model-pos (vec3 (* model (vec4 (+ offset mesh/pos) 1)))))
        (values (* proj view (vec4 model-pos 1))
                model-pos
                world-normal
                to-camera)))))

(defun calculate-lighting ((light light/directional)
                           (material material-data)
                           (to-camera :vec3)
                           (normal :vec3))
  (with-accessors ((light/position position)
                   (light/ambient ambient)
                   (light/diffuse diffuse)
                   (light-specular specular))
      light
    (with-accessors ((material/ambient ambient)
                     (material/diffuse diffuse)
                     (material/specular specular)
                     (material/shininess shininess))
        material
      (let* ((light-from-dir (- (normalize light/position)))
             (normal-dir (normalize normal))
             (camera-dir (normalize to-camera))
             (n-dot-l (dot normal-dir light-from-dir))
             (ambient (* material/ambient light/ambient))
             (diffuse-factor (clamp n-dot-l 0 1))
             (diffuse (* material/diffuse light/diffuse diffuse-factor))
             (specular-factor 0.0))
        (when (plusp n-dot-l)
          (let ((light-camera-half (normalize (+ light-from-dir camera-dir))))
            (setf specular-factor (pow (dot normal-dir light-camera-half)
                                       material/shininess))))
        (let ((specular (* light-specular specular-factor)))
          (vec3 (+ ambient diffuse specular)))))))

(defun generate-texture/floor ((frag-pos :vec3))
  (let* ((base-1 (vec3 (+ (* 0.3 (umbra.noise:perlin-surflet
                                  (* frag-pos 0.024)))
                          (* 0.4 (umbra.noise:cellular-fast (* frag-pos 0.08)))
                          (* 0.4 (umbra.noise:cellular-fast (* frag-pos 0.06)))
                          (* 0.3 (umbra.noise:simplex-perlin
                                  (* frag-pos 0.5))))))
         (base-1 (umbra.color:set-brightness base-1 -0.4))
         (base-1 (umbra.color:set-gamma base-1 3))
         (base-1 (umbra.color:set-contrast base-1 0.8))
         (base-2 (vec3 (umbra.noise:simplex-perlin (* frag-pos 0.0088))))
         (base-2 (umbra.color:set-contrast base-2 0.34))
         (base (mix base-1 base-2 0.5))
         (base-color (* base (vec3 0.17 0.27 0.39)))
         (base-color (umbra.color:set-saturation base-color 0.8))
         (base-color (umbra.color:set-contrast base-color 1.6)))
    (+ base base-color)))

(defun generate-texture/wall ((frag-pos :vec3))
  (let* ((base-1 (vec3 (+ (* 0.45 (umbra.noise:simplex-perlin
                                   (vec3 (* frag-pos 0.24))))
                          (* 0.55 (umbra.noise:cellular-fast
                                   (vec3 (* frag-pos 0.084)))))))
         (base-2 (vec3 (* 0.65 (umbra.noise:simplex-perlin
                                (vec3 (* frag-pos 0.0024))))))
         (base-2 (umbra.color:set-contrast base-2 0.45))
         (base (mix base-1 base-2 0.55))
         (base-color (umbra.color:set-saturation
                      (* base (vec3 0 0.21 0.29)) 0.74))
         (base-color (umbra.color:set-contrast base-color 1.15)))
    (umbra.color:set-contrast
     (+ base base-color)
     1.55)))

(defun generate-texture ((cell-type :int)
                         (frag-pos :vec3))
  (case cell-type
    (0 (generate-texture/floor frag-pos))
    (1 (generate-texture/wall frag-pos))
    (otherwise (vec3 0))))

(defun world/f ((frag-pos :vec3)
                (normal :vec3)
                (to-camera :vec3)
                &uniforms
                (cell-type :int)
                (light light/directional)
                (material material-data)
                (opacity :float))
  (let ((lighting (calculate-lighting light material to-camera normal))
        (texture (generate-texture cell-type frag-pos)))
    (vec4 (mix texture lighting 0.5) opacity)))

(define-shader world ()
  (:vertex (world/v mesh-attrs))
  (:fragment (world/f :vec3 :vec3 :vec3)))
