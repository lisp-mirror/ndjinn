(in-package #:pyx)

(defclass texture-spec ()
  ((%name :reader name
          :initarg :name)
   (%source :reader source)
   (%width :reader width)
   (%height :reader height)
   (%pixel-format :reader pixel-format)
   (%pixel-type :reader pixel-type)
   (%internal-format :reader internal-format)
   (%generate-mipmaps-p :reader generate-mipmaps-p)
   (%parameters :reader parameters)))

(u:define-printer (texture-spec stream)
  (format stream "~s" (name texture-spec)))

(define-event-handler :recompile :texture recompile-texture)

(defun find-texture-spec (texture-name)
  (u:if-found (spec (meta :textures texture-name))
              (values spec t)
              (meta :textures 'debug)))

(defun make-texture-spec (name source width height pixel-format pixel-type
                          internal-format generate-mipmaps-p parameters)
  (let ((spec (make-instance 'texture-spec :name name)))
    (setf (meta :textures name) spec)
    (update-texture-spec name source width height pixel-format pixel-type
                         internal-format generate-mipmaps-p parameters)
    spec))

(defun update-texture-spec (name source width height pixel-format pixel-type
                            internal-format generate-mipmaps-p parameters)
  (with-slots (%name %source %width %height %pixel-format %pixel-type
               %internal-format %generate-mipmaps-p %parameters)
      (meta :textures name)
    (setf %source source
          %width width
          %height height
          %pixel-format pixel-format
          %pixel-type pixel-type
          %internal-format internal-format
          %generate-mipmaps-p generate-mipmaps-p
          %parameters parameters)
    (enqueue :recompile (list :texture %name))))

(defmacro define-texture (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (parameters)
    (destructuring-bind (&key source width height (generate-mipmaps t)
                           pixel-format pixel-type internal-format
                           (min-filter :linear-mipmap-linear)
                           (mag-filter :linear) (swizzle-r :red)
                           (swizzle-g :green) (swizzle-b :blue)
                           (swizzle-a :alpha))
        (car body)
      `(let ((,parameters (u:dict #'eq
                                  :texture-min-filter ,min-filter
                                  :texture-mag-filter ,mag-filter
                                  :texture-swizzle-r ,swizzle-r
                                  :texture-swizzle-g ,swizzle-g
                                  :texture-swizzle-b ,swizzle-b
                                  :texture-swizzle-a ,swizzle-a)))
         (unless (meta :textures)
           (setf (meta :textures) (u:dict #'eq)))
         (if (meta :textures ',name)
             (update-texture-spec ',name ',source ,width ,height ,pixel-format
                                  ,pixel-type ,internal-format
                                  ,generate-mipmaps ,parameters)
             (make-texture-spec ',name ',source ,width ,height
                                ,pixel-format ,pixel-type ,internal-format
                                ,generate-mipmaps ,parameters))))))

(define-texture default ()
  (:source "debug.png"))
