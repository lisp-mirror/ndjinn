(in-package #:pyx)

(defclass action-manager ()
  ((%action-list :reader action-list
                 :initform (doubly-linked-list:make-dlist :test #'eq))
   (renderer :reader renderer
             :initarg :renderer)))

(defclass action ()
  ((%manager :reader manager
             :initarg :manager)
   (%node :accessor node)
   (%type :reader action-type
          :initarg :type)
   (%elapsed :accessor elapsed
             :initarg :elapsed
             :initform 0)
   (%duration :reader duration
              :initarg :duration
              :initform 1)
   (%finished-p :accessor finished-p
                :initarg :finished-p
                :initform nil)
   (%self-finishing-p :reader self-finishing-p
                      :initarg :self-finishing-p
                      :initform nil)
   (%blocking-p :reader blocking-p
                :initarg :blocking-p
                :initform nil)
   (%repeat-p :reader repeat-p
              :initarg :repeat-p
              :initform nil)
   (%shape :reader shape
           :initarg :shape
           :initform 'origin.shaping:linear)
   (%attrs :reader attrs
           :initarg :attrs
           :initform nil)))

(defmethod initialize-instance :after ((instance action) &key &allow-other-keys)
  (with-slots (%attrs) instance
    (setf %attrs (u:plist->hash %attrs :test #'eq))))

(defun insert-action (action where &key target)
  (with-slots (%manager %type) action
    (let* ((action-list (action-list %manager))
           (node (doubly-linked-list:insert-dlist-node
                  where action-list %type action :target-key target)))
      (setf (node action) node)
      (on-insert action %type)
      action)))

(defun remove-action (action)
  (with-slots (%manager %type) action
    (doubly-linked-list:remove-dlist-node (action-list %manager) %type)))

(defun replace (action type &rest args)
  (let ((action (apply #'reinitialize-instance action
                       :type type
                       :elapsed 0
                       :finished-p nil
                       args)))
    (doubly-linked-list:update-dlist-node-key (node action) type)))

(defun step (action)
  (with-slots (%shape %elapsed %duration) action
    (funcall %shape (a:clamp (/ %elapsed %duration) 0f0 1f0))))

(defun make-action ()
  (let ((action (apply #'make-instance 'action
                       :owner)))))

(defun process-actions (actions)
  (loop :for (nil . action) :in (doubly-linked-list:dlist-elements actions)
        :do (on-action-update action)
        :when (finished-p action)
          :do (on-action-finish action)
        :when (blocking-p action)
          :do (return)))

;;; Action event hooks

(defgeneric on-action-insert (action)
  (:method (action)))

(defgeneric on-action-finish (action)
  (:method (action))
  (:method :around (action)
    (with-slots (%owner %elapsed %finished-p %repeat-p %reverse-p) action
      (when %repeat-p
        (setf %reverse-p (not %reverse-p)
              %elapsed 0
              %finished-p nil))
      (call-next-method)
      (unless %repeat-p
        (remove-action action)))))

(defgeneric on-action-update (action)
  (:method (action))
  (:method :before (action)
    (with-slots (%elapsed %duration %self-finishing-p %finished-p) action
      (incf %elapsed (clock-frame-time (clock *state*)))
      (when (and (not %self-finishing-p)
                 (>= %elapsed %duration))
        (setf %finished-p t)))))
