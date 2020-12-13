(in-package #:ndjinn)

(defstruct (texture-spec
            (:constructor %make-texture-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (type nil :type symbol)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type (member :red :rg :rgb :rgba))
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (generate-mipmaps t :type boolean)
  (parameters nil :type list)
  (source nil :type (or list (integer 1) null)))

(u:define-printer (texture-spec stream)
  (format stream "~s" (texture-spec-name texture-spec)))

(defun find-texture-spec (name)
  (or (u:href =meta/textures= name)
      (error "Texture ~s is not defined." name)))

(defun update-texture-spec (name type source width height pixel-format
                            pixel-type internal-format generate-mipmaps
                            parameters)
  (let ((spec (find-texture-spec name)))
    (setf (texture-spec-type spec) type
          (texture-spec-width spec) width
          (texture-spec-height spec) height
          (texture-spec-pixel-format spec) (or pixel-format :rgba)
          (texture-spec-pixel-type spec) (or pixel-type :unsigned-byte)
          (texture-spec-internal-format spec) (or internal-format :rgba8)
          (texture-spec-generate-mipmaps spec) generate-mipmaps
          (texture-spec-parameters spec) parameters
          (texture-spec-source spec) source)
    (enqueue :recompile (list :texture name))))

(defun make-texture-spec (name &rest args)
  (let ((spec (%make-texture-spec :name name)))
    (setf (u:href =meta/textures= name) spec)
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
                         (wrap-r :repeat)
                       &allow-other-keys)
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
      `(if (u:href =meta/textures= ',name)
           (update-texture-spec ',name ,type ',source ,width ,height
                                ,pixel-format ,pixel-type ,internal-format
                                ,generate-mipmaps ',parameters)
           (make-texture-spec ',name ,type ',source ,width ,height ,pixel-format
                              ,pixel-type ,internal-format ,generate-mipmaps
                              ',parameters)))))
