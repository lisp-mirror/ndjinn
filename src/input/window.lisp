(in-package #:ndjinn)

(u:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

;;; Internal event hooks

(defun event/window-show ()
  (input-transition-in :window :visible))

(defun event/window-hide ()
  (input-transition-out :window :visible))

(defun event/window-move (x y)
  (set-display-properties (display =context=))
  (setf (window-position) (v2:- (v2:vec x y) (get-display-position))))

(defun event/window-resize (width height)
  (let ((old (window-size))
        (new (v2:vec width height)))
    (setf (window-size) new)
    (reconfigure-viewports)
    (invoke-entity-window-resize-hook old new)))

(defun event/window-minimize ()
  (let ((states (input-data-states (input-data =context=))))
    (cond
      ((u:when-let ((maximize (u:href states '(:window :maximize))))
         (input-transition-enabled maximize))
       (input-transition-out :window :maximize))
      ((u:when-let ((restore (u:href states '(:window :restore))))
         (input-transition-enabled restore))
       (input-transition-out :window :restore)))
    (input-transition-in :window :minimize)))

(defun event/window-maximize ()
  (let ((states (input-data-states (input-data =context=))))
    (cond
      ((u:when-let ((minimize (u:href states '(:window :minimize))))
         (input-transition-enabled minimize))
       (input-transition-out :window :minimize))
      ((u:when-let ((restore (u:href states '(:window :restore))))
         (input-transition-enabled restore))
       (input-transition-out :window :restore)))
    (input-transition-in :window :maximize)))

(defun event/window-restore ()
  (let ((states (input-data-states (input-data =context=))))
    (cond
      ((u:when-let ((minimize (u:href states '(:window :minimize))))
         (input-transition-enabled minimize))
       (input-transition-out :window :minimize))
      ((u:when-let ((maximize (u:href states '(:window :maximize))))
         (input-transition-enabled maximize))
       (input-transition-out :window :maximize)))
    (input-transition-in :window :restore)))

(defun event/window-mouse-focus-enter ()
  (input-transition-in :window :mouse-focus))

(defun event/window-mouse-focus-exit ()
  (input-transition-out :window :mouse-focus))

(defun event/window-keyboard-focus-enter ()
  (input-transition-in :window :keyboard-focus))

(defun event/window-keyboard-focus-exit ()
  (input-transition-out :window :keyboard-focus))

(defun event/window-close ()
  (input-transition-in :window :close))

;;; Interface

(defun on-window-event-enter (event)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) (list :window event))))
    (input-transition-enter state)))

(defun on-window-event-enabled (event)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) (list :window event))))
    (input-transition-enabled state)))

(defun on-window-event-exit (event)
  (u:when-let* ((data (input-data =context=))
                (state (u:href (input-data-states data) (list :window event))))
    (input-transition-exit state)))
