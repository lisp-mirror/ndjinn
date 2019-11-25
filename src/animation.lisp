(in-package #:pyx)

(defclass animation-state ()
  ((%name :reader name
          :initarg :name)
   (%elapsed :accessor elapsed
             :initform 0)
   (%duration :reader duration
              :initarg :duration
              :initform 1)
   (%progress :reader progress
              :initform 0)
   (%finished-p :accessor finished-p
                :initarg :finished-p
                :initform nil)
   (%self-finishing-p :reader self-finishing-p
                      :initform nil)
   (%blocking-p :reader blocking-p
                :initarg :blocking-p
                :initform nil)
   (%repeat-p :reader repeat-p
              :initarg :repeat-p
              :initform nil)
   (%reverse-p :accessor reverse-p
               :initform nil)
   (%interpolation :reader interpolation
                   :initarg :interpolation
                   :initform 'origin.shaping:linear)))

(u:define-printer (animation-state stream :type nil)
  (format stream "ANIMATION-STATE: ~s" (name animation-state)))

(defun register-animation-state (entity state &key (where :tail) target)
  (doubly-linked-list:insert-dlist-node where
                                        (animate/states entity)
                                        (name state)
                                        state
                                        :target-key target)
  (%on-animation-start entity state))

(defun deregister-animation-state (entity state)
  (doubly-linked-list:remove-dlist-node (animate/states entity) (name state)))

(defun replace-animation-state (state name &rest args)
  (with-slots (%elapsed %finished-p) state
    (apply #'change-class
           state
           (class-name (meta :animation-states name))
           args)
    (setf %elapsed 0
          %finished-p nil)))

(defun tick-animation-state (state)
  (with-slots (%interpolation %elapsed %duration %progress) state
    (setf %progress (funcall %interpolation
                             (a:clamp (/ %elapsed %duration) 0f0 1f0)))))

(defun process-animation-states (entity)
  (loop :with states = (animate/states entity)
        :for (nil . state) :in (doubly-linked-list:dlist-elements states)
        :do (%on-animation-update entity state)
        :when (finished-p state)
          :do (%on-animation-finish entity state)
        :when (blocking-p state)
          :do (return)))

(ff:define-filtered-function %on-animation-start (entity state)
  (:method (entity state)
    (declare (ignore entity state)))
  (:filters (:state (list #'identity #'name))))

(ff:define-filtered-function %on-animation-finish (entity state)
  (:method (entity state)
    (declare (ignore entity state)))
  (:method :around (entity state)
    (with-slots (%elapsed %finished-p %repeat-p %reverse-p) state
      (when %repeat-p
        (setf %reverse-p (not %reverse-p)
              %elapsed 0
              %finished-p nil))
      (call-next-method)
      (unless %repeat-p
        (deregister-animation-state entity state))))
  (:filters (:state (list #'identity #'name))))

(ff:define-filtered-function %on-animation-update (entity state)
  (:method (entity state)
    (declare (ignore entity state)))
  (:method :before (entity state)
    (declare (ignore entity))
    (with-slots (%elapsed %duration %self-finishing-p %finished-p) state
      (incf %elapsed (clock-frame-time (clock *state*)))
      (when (and (not %self-finishing-p)
                 (>= %elapsed %duration))
        (setf %finished-p t))
      (tick-animation-state state)))
  (:filters (:state (list #'identity #'name))))

(defmacro define-animation-state (name options &body body)
  (declare (ignore options))
  (let ((class-name (a:make-gensym (a:symbolicate '#:animation-state/ name))))
    `(u:eval-always
       (defclass ,class-name (animation-state)
         ,(loop :for (key value) :on (car body) :by #'cddr
                :for accessor = (a:symbolicate key)
                :for slot = (a:symbolicate '#:% accessor)
                :for initarg = (a:make-keyword accessor)
                :collect `(,slot :accessor ,accessor
                                 :initarg ,initarg
                                 :initform ,value))
         (:default-initargs :name ',name))
       (unless (meta :animation-states)
         (setf (meta :animation-states) (u:dict #'eq)))
       (setf (meta :animation-states ',name) (find-class ',class-name)))))

(defmacro define-animation-state-hook (name entity state hook &body body)
  (let ((hook-types '(:start :finish :update)))
    `(progn
       (unless (member ,hook ',hook-types)
         (error "Hook type must be one of: ~{~s~^, ~}" ',hook-types))
       (defmethod ,(a:format-symbol :pyx "%ON-ANIMATION-~a" hook) :filter :state
           (,entity (,state (eql ',name)))
         (declare (ignorable ,entity ,state))
         ,@body))))

(defmacro define-animation-sequence (name options &body body)
  (declare (ignore options))
  (a:with-gensyms (state entity)
    `(progn
       (unless (meta :animation-sequences)
         (setf (meta :animation-sequences) (u:dict #'eq)))
       (setf (meta :animation-sequences ',name)
             (lambda (,entity)
               ,@(mapcar
                  (lambda (spec)
                    (destructuring-bind (name . args) spec
                      `(let ((,state (apply #'make-instance
                                            (meta :animation-states ',name)
                                            ',args)))
                         (register-animation-state ,entity ,state))))
                  body))))))
