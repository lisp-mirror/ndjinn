(in-package #:pyx)

(defclass texture-spec ()
  ((%name :reader name
          :initarg :name)
   (%source :reader source
            :initarg :source)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)
   (%generate-mipmaps-p :reader generate-mipmaps-p
                        :initarg :generate-mipmaps-p)
   (%parameters :reader parameters
                :initarg :parameters)))

(defun find-texture-spec (texture-name)
  (u:if-found (spec (meta :textures texture-name))
              (values spec
                      t)
              (values (meta :textures 'debug)
                      nil)))

(defmacro define-texture (name &body body)
  (a:with-gensyms (parameters)
    (destructuring-bind (&key source width height (generate-mipmaps-p t)
                           (min-filter :nearest-mipmap-linear)
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
         (setf (meta :textures ',name)
               (make-instance 'texture-spec
                              :name ',name
                              :source ',source
                              :width ,width
                              :height ,height
                              :generate-mipmaps-p ,generate-mipmaps-p
                              :parameters ,parameters))))))

(define-texture debug
  (:source "debug.png"))
