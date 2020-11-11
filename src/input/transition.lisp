(in-package #:ndjinn)

(defstruct (input-transition
            (:predicate nil)
            (:copier nil))
  (enter t :type boolean)
  (enabled t :type boolean)
  (exit nil :type boolean))

;;; Internal

(defun input-transition-in (&rest input)
  (let* ((data (input-data =context=))
         (states (input-data-states data)))
    (u:if-let ((state (u:href states input)))
      (progn
        (setf (input-transition-enter state) t
              (input-transition-enabled state) t
              (input-transition-exit state) nil))
      (let ((transition (make-input-transition)))
        (setf (u:href states input) transition)))
    (push input (u:href (input-data-entering data) (car input)))))

(defun input-transition-out (&rest input)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) input)))
    (setf (input-transition-enter state) nil
          (input-transition-enabled state) nil
          (input-transition-exit state) t)
    (push input (u:href (input-data-exiting data) (car input)))))

(defun input-transition-enable-entering ()
  (let* ((data (input-data =context=))
         (entering (input-data-entering data)))
    (u:do-hash-values (type entering)
      (dolist (key type)
        (let ((state (u:href (input-data-states data) key)))
          (setf (input-transition-enter state) nil
                (input-transition-enabled state) t
                (input-transition-exit state) nil))))
    (clrhash entering)))

(defun input-transition-disable-exiting ()
  (let* ((data (input-data =context=))
         (exiting (input-data-exiting data)))
    (u:do-hash-values (type exiting)
      (dolist (key type)
        (let ((state (u:href (input-data-states data) key)))
          (setf (input-transition-enter state) nil
                (input-transition-enabled state) nil
                (input-transition-exit state) nil))))
    (clrhash exiting)))

;;; Interface

(defun on-button-enter (&rest args)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) args)))
    (input-transition-enter state)))

(defun on-button-enabled (&rest args)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) args)))
    (input-transition-enabled state)))

(defun on-button-exit (&rest args)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) args)))
    (input-transition-exit state)))
