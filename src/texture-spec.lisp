(in-package #:%pyx.texture)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  source
  spec-width
  spec-height
  pixel-format
  pixel-type
  internal-format
  generate-mipmaps
  parameters)

(u:define-printer (spec stream)
  (format stream "~s" (name spec)))

(defun find-spec (name)
  (or (u:href meta:=textures= name)
      (error "Texture ~s is not defined." name)))

(defun update-spec (name source width height pixel-format pixel-type
                    internal-format generate-mipmaps parameters)
  (let ((spec (find-spec name)))
    (setf (source spec) source
          (spec-width spec) width
          (spec-height spec) height
          (pixel-format spec) pixel-format
          (pixel-type spec) pixel-type
          (internal-format spec) internal-format
          (generate-mipmaps spec) generate-mipmaps
          (parameters spec) parameters)
    (tp:enqueue :recompile (list :texture name))))

(defun make-spec (name &rest args)
  (let ((spec (%make-spec :name name)))
    (setf (u:href meta:=textures= name) spec)
    (apply #'update-spec name args)
    spec))

(defun make-parameters (args)
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

;;; Public API

(defmacro define-texture (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&rest args
                       &key source width height (generate-mipmaps t)
                         pixel-format pixel-type internal-format
                       &allow-other-keys)
      (car body)
    (let ((parameters (make-parameters args)))
      `(if (u:href meta:=textures= ',name)
           (update-spec ',name ',source ,width ,height ,pixel-format
                        ,pixel-type ,internal-format ,generate-mipmaps
                        ',parameters)
           (make-spec ',name ',source ,width ,height ,pixel-format
                      ,pixel-type ,internal-format ,generate-mipmaps
                      ',parameters)))))
