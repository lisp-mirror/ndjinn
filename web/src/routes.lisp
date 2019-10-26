(in-package #:pyx.web)

(defmacro define-route (target method args &body body)
  (let* ((splice-p (find #\: target :test #'char=))
         (parameters (if splice-p
                         args
                         (mapcar
                          (lambda (x)
                            (a:format-symbol *package* "~(~a~)" x))
                          args)))
         (all-args (u:interleave
                    (mapcar #'a:make-keyword args)
                    parameters)))
    `(progn
       (unless *app*
         (setf *app* (make-instance 'app)))
       (web:defroute (,target :method ,method) (&key ,@parameters)
         (let (,@(unless splice-p (mapcar #'list args parameters))
               ,@(when all-args `((@ `(,,@all-args)))))
           (declare (ignorable ,@(when all-args `(@))
                               ,@(unless splice-p args)))
           ,@body)))))

(defmethod web:on-exception ((app app) (code (eql 404)))
  (render :404
          :section "Page Not Found"))

(define-route "/" :get ()
  (web:redirect "/overview"))

(define-route "/overview" :get ()
  (render :overview
          :section "Overview"))

(define-route "/get-started" :get ()
  (render :get-started
          :section "Get Started"))

(define-route "/reference" :get ()
  (render :reference
          :section "Reference"))

(define-route "/reference/math" :get ()
  (web:redirect "/reference/math/overview"))

(define-route "/reference/math/overview" :get ()
  (render :reference-math-overview
          :section "Reference"))

(define-route "/reference/math/syntax" :get ()
  (render :reference-math-syntax
          :section "Reference"))

(define-route "/reference/math/vectors" :get ()
  (render :reference-math-vectors
          :section "Reference"))

(define-route "/reference/math/matrices" :get ()
  (render :reference-math-matrices
          :section "Reference"))

(define-route "/reference/math/quaternions" :get ()
  (render :reference-math-quaternions
          :section "Reference"))
