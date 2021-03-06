(in-package #:ndjinn)

(defstruct (clock (:constructor %make-clock)
                  (:predicate nil)
                  (:copier nil))
  (accumulator 0d0 :type u:f64)
  (delta-buffer 0d0 :type u:f64)
  (fps/current 0d0 :type u:f64)
  (fps/average 0d0 :type u:f64)
  (fps/average/10s 0d0 :type u:f64)
  (fps/average/30s 0d0 :type u:f64)
  (fps/average/60s 0d0 :type u:f64)
  (frame-count 0 :type fixnum)
  (frame-time 0d0 :type u:f64)
  (init-time 0 :type fixnum)
  (delta-time 0f0 :type u:f32)
  (alpha 0f0 :type u:f32)
  (period-elapsed 0d0 :type u:f64)
  (period-interval 0.25d0 :type u:f64)
  (previous-time 0d0 :type u:f64)
  (running-time 0d0 :type u:f64)
  (pause-time 0d0 :type u:f64))

(defun %get-time (clock)
  (/ (- (get-internal-real-time)
        (clock-init-time clock))
     (float internal-time-units-per-second 1d0)))

(defun make-clock ()
  (let ((clock (%make-clock))
        (delta-time (float (or (cfg :delta-time) (/ (get-refresh-rate))) 1f0)))
    (setf (clock-init-time clock) (get-internal-real-time)
          (clock-running-time clock) (%get-time clock)
          (clock-delta-time clock) delta-time)
    (setf (clock =context=) clock)
    (log:debug :ndjinn "Initialized game clock: delta: ~,3f ms/frame"
               (* delta-time 1000f0))))

(defun smooth-delta-time (clock refresh-rate)
  (symbol-macrolet ((frame-time (clock-frame-time clock)))
    (incf frame-time (clock-delta-buffer clock))
    (let ((frame-count (max 1 (truncate (1+ (* frame-time refresh-rate)))))
          (previous frame-time))
      (setf frame-time (/ frame-count refresh-rate 1d0)
            (clock-delta-buffer clock) (- previous frame-time)))))

(defun calculate-frame-rate (clock)
  (let* ((time (clock-frame-time clock))
         (fps (/ 1d0 time))
         (alpha10 (- 1 (exp (- (/ time 10)))))
         (alpha30 (- 1 (exp (- (/ time 30)))))
         (alpha60 (- 1 (exp (- (/ time 60)))))
         (frame-count (clock-frame-count clock)))
    (symbol-macrolet ((average/10s (clock-fps/average/10s clock))
                      (average/30s (clock-fps/average/30s clock))
                      (average/60s (clock-fps/average/60s clock))
                      (average (clock-fps/average clock)))
      (setf (clock-fps/current clock) fps)
      (if (> (clock-running-time clock) 3)
          (setf average/10s (+ (* alpha10 fps) (* (- 1 alpha10) average/10s))
                average/30s (+ (* alpha30 fps) (* (- 1 alpha30) average/30s))
                average/60s (+ (* alpha60 fps) (* (- 1 alpha60) average/60s))
                average (/ (+ fps (* (1- frame-count) average)) frame-count))
          (setf average/10s fps
                average/30s fps
                average/60s fps
                average fps)))))

(defun clock-update (clock func)
  (let ((delta-time (clock-delta-time clock)))
    (symbol-macrolet ((accumulator (clock-accumulator clock)))
      (incf accumulator (clock-frame-time clock))
      (when (zerop (clock-frame-count clock))
        (funcall func))
      (u:while (>= accumulator delta-time)
        (funcall func)
        (decf accumulator delta-time))
      (setf (clock-alpha clock) (float (/ accumulator delta-time) 1f0)))))

(defun clock-update/periodic (clock func)
  (let ((current (clock-running-time clock)))
    (symbol-macrolet ((elapsed (clock-period-elapsed clock)))
      (when (>= (- current elapsed) (clock-period-interval clock))
        (funcall func)
        (setf elapsed current)))))

(defun tick-clock (clock refresh-rate update-func periodic-func)
  (let* ((pause (clock-pause-time clock))
         (previous (+ (clock-running-time clock) pause))
         (current (- (%get-time clock) pause)))
    (setf (clock-previous-time clock) previous
          (clock-running-time clock) current)
    (if (zerop (clock-frame-count clock))
        (setf (clock-frame-time clock) (float (clock-delta-time clock) 1d0))
        (setf (clock-frame-time clock) (- current previous)))
    (smooth-delta-time clock refresh-rate)
    (clock-update clock update-func)
    (clock-update/periodic clock periodic-func)
    (when (plusp (clock-frame-count clock))
      (calculate-frame-rate clock))))

(defun get-time ()
  (%get-time (clock =context=)))

(defun get-alpha ()
  (clock-alpha (clock =context=)))

(defun get-fps ()
  (let ((clock (clock =context=)))
    (values (clock-fps/current clock)
            (clock-fps/average clock)
            (clock-fps/average/10s clock)
            (clock-fps/average/30s clock)
            (clock-fps/average/60s clock))))

(defun get-frame-count ()
  (clock-frame-count (clock =context=)))

(defun get-frame-time ()
  (float (clock-frame-time (clock =context=)) 1f0))

(defun get-running-time ()
  (clock-running-time (clock =context=)))

(defun pause-time ()
  (clock-pause-time (clock =context=)))

(defun (setf pause-time) (value)
  (setf (clock-pause-time (clock =context=)) value))
