(in-package #:pyx)

(defun calculate-font-space-width (font-spec)
  (let ((chars (font:chars font-spec)))
    (or (getf (u:href chars #\space) :xadvance)
        (getf (u:href chars #\n) :xadvance)
        (/ (loop :for char :in (u:hash-values chars)
                 :sum (or (getf char :xadvance) 0))
           (hash-table-count chars)))))

(defun get-font-character-data (font-spec char)
  (let ((chars (font:chars font-spec)))
    (or (u:href chars char)
        (u:href chars :invalid)
        (u:href chars (code-char #xfffd))
        '(:xoffset 0 :yoffset 0 :x 0 :y 0 :width 0 :height 0 :xadvance 0))))

(defun get-font-dimensions (font-spec string)
  (loop :with x = 0
        :with y = 0
        :with kernings = (font:kernings font-spec)
        :with line-height = (font:line-height font-spec)
        :with line-widths = '(0)
        :with space-width = (calculate-font-space-width font-spec)
        :for i :from 0 :below (length string)
        :for char = (aref string i)
        :for char-data = (get-font-character-data font-spec char)
        :for p = nil :then char
        :for kerning = (or (u:href kernings (cons p char)) 0)
        :do (case char
              (#\newline
               (push x line-widths)
               (setf x 0
                     y line-height))
              (#\space
               (incf x space-width))
              (#\tab
               (incf x (* space-width 8)))
              (t
               (incf x kerning)
               (incf x (getf char-data :xadvance))))
        :finally (return (values (float (max x (apply #'max line-widths)) 1f0)
                                 (float (+ y (font:base font-spec)) 1f0)))))

(defun get-font-position (font)
  (with-slots (%font/dimensions %font/position %font/offset) font
    (v2:with-components ((fd %font/dimensions)
                         (fo %font/offset)
                         (vp (get-viewport-dimensions)))
      (v3:with-components ((s (current (xform/scaling font))))
        (case %font/position
          (:top-left
           (v3:vec (+ (/ vpx -2) fox)
                   (- (/ (- vpy (* fdy sy)) 2) foy)
                   0))
          (:bottom-left
           (v3:vec (+ (/ vpx -2) fox)
                   (+ (/ (- vpy (* fdy sy)) -2) foy)
                   0))
          (:bottom-right
           (v3:vec (- (/ vpx 2) (* fdx sx) fox)
                   (+ (/ (- vpy (* fdy sy)) -2) foy)
                   0))
          (:top-right
           (v3:vec (- (/ vpx 2) (* fdx sx) fox)
                   (- (/ (- vpy (* fdy sy)) 2) foy)
                   0))
          (:center-right
           (v3:vec (- (/ vpx 2) (* fdx sx) fox)
                   (/ fdy 2)
                   0))
          (:center-left
           (v3:vec (+ (/ vpx -2) fox)
                   (/ fdy 2)
                   0))
          (:center-top
           (v3:vec (/ (* fdx sx) -2)
                   (- (/ (- vpy (* fdy sy)) 2) foy)
                   0))
          (:center-bottom
           (v3:vec (/ (* fdx sx) -2)
                   (+ (/ (- vpy (* fdy sy)) -2) foy)
                   0))
          (:center
           (v3:vec (/ (* fdx sx) -2)
                   (/ fdy 2)
                   0))
          (t (v3:vec)))))))
