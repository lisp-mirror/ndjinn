(in-package #:net.mfiano.lisp.pyx)

(defclass texture-spec ()
  ((%name :reader name
          :initarg :name)
   (%type :accessor texture-type)
   (%width :accessor width)
   (%height :accessor height)
   (%pixel-format :accessor pixel-format)
   (%pixel-type :accessor pixel-type)
   (%internal-format :accessor internal-format)
   (%generate-mipmaps :accessor generate-mipmaps)
   (%parameters :accessor parameters)
   (%source :accessor source)))

(u:define-printer (texture-spec stream)
  (format stream "~s" (name texture-spec)))

(defun find-texture-spec (name)
  (or (u:href (metadata-textures =metadata=) name)
      (error "Texture ~s is not defined." name)))

(defun update-texture-spec (name type source width height pixel-format
                            pixel-type internal-format generate-mipmaps
                            parameters)
  (let ((spec (find-texture-spec name)))
    (setf (texture-type spec) type
          (width spec) width
          (height spec) height
          (pixel-format spec) (or pixel-format :rgba)
          (pixel-type spec) (or pixel-type :unsigned-byte)
          (internal-format spec) (or internal-format :rgba8)
          (generate-mipmaps spec) generate-mipmaps
          (parameters spec) parameters
          (source spec) source)
    (enqueue :recompile (list :texture name))))

(defun make-texture-spec (name &rest args)
  (let ((spec (make-instance 'texture-spec :name name)))
    (setf (u:href (metadata-textures =metadata=) name) spec)
    (apply #'update-texture-spec name args)
    spec))

(defun make-texture-parameters (args)
  (destructuring-bind (&key (depth-stencil-mode :depth-component) (base-level 0)
                         (border-color (v4:vec)) (compare-func :lequal)
                         (compare-mode :none) (lod-bias 0f0)
                         (min-filter :linear-mipmap-linear)
                         (mag-filter :linear) (min-lod -1000) (max-lod 1000)
                         (max-level 1000) (swizzle-r :red)
                         (swizzle-g :green) (swizzle-b :blue)
                         (swizzle-a :alpha) (wrap-s :repeat) (wrap-t :repeat)
                         (wrap-r :repeat) &allow-other-keys)
      args
    (list :depth-stencil-texture-mode depth-stencil-mode
          :texture-base-level base-level
          :texture-border-color border-color
          :texture-compare-func compare-func
          :texture-compare-mode compare-mode
          :texture-lod-bias lod-bias
          :texture-min-filter min-filter
          :texture-mag-filter mag-filter
          :texture-min-lod min-lod
          :texture-max-lod max-lod
          :texture-max-level max-level
          :texture-swizzle-r swizzle-r
          :texture-swizzle-g swizzle-g
          :texture-swizzle-b swizzle-b
          :texture-swizzle-a swizzle-a
          :texture-wrap-s wrap-s
          :texture-wrap-t wrap-t
          :texture-wrap-r wrap-r)))

(defmacro define-texture (name (&optional (type :2d)) &body body)
  (destructuring-bind (&rest args
                       &key source width height (generate-mipmaps t)
                         pixel-format pixel-type internal-format
                       &allow-other-keys)
      (car body)
    (let ((parameters (make-texture-parameters args)))
      `(if (u:href (metadata-textures =metadata=) ',name)
           (update-texture-spec ',name ,type ',source ,width ,height
                                ,pixel-format ,pixel-type ,internal-format
                                ,generate-mipmaps ',parameters)
           (make-texture-spec ',name ,type ',source ,width ,height ,pixel-format
                              ,pixel-type ,internal-format ,generate-mipmaps
                              ',parameters)))))
