(in-package #:pyx)

(defstruct input-button-state enter enabled exit)

(defun button-transition-in (data input)
  (symbol-macrolet ((state (u:href (states data) input)))
    (if state
        (setf (input-button-state-enter state) t
              (input-button-state-enabled state) t
              (input-button-state-exit state) nil)
        (setf state (make-input-button-state :enter t :enabled t)))
    (push input (u:href (entering data) :button))))

(defun button-transition-out (data input)
  (a:when-let ((state (u:href (states data) input)))
    (setf (input-button-state-enter state) nil
          (input-button-state-enabled state) nil
          (input-button-state-exit state) t)
    (push input (u:href (exiting data) :button))))

(defun button-enable-entering (data)
  (symbol-macrolet ((entering (u:href (entering data) :button)))
    (dolist (button entering)
      (let ((state (u:href (states data) button)))
        (setf (input-button-state-enter state) nil
              (input-button-state-enabled state) t
              (input-button-state-exit state) nil)))
    (setf entering nil)))

(defun button-disable-exiting (data)
  (symbol-macrolet ((exiting (u:href (exiting data) :button)))
    (dolist (button exiting)
      (let ((state (u:href (states data) button)))
        (setf (input-button-state-enter state) nil
              (input-button-state-enabled state) nil
              (input-button-state-exit state) nil)))
    (setf exiting nil)))

(defun on-button-enter (&rest args)
  (a:when-let* ((data (input-data *state*))
                (state (u:href (states data) args)))
    (input-button-state-enter state)))

(defun on-button-enabled (&rest args)
  (a:when-let* ((data (input-data *state*))
                (state (u:href (states data) args)))
    (input-button-state-enabled state)))

(defun on-button-exit (&rest args)
  (a:when-let* ((data (input-data *state*))
                (state (u:href (states data) args)))
    (input-button-state-exit state)))
