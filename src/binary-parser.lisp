(in-package #:%pyx.binary-parser)

(defun get-string-length (buffer byte-count null-terminated-p)
  (let* ((sequence (fast-io:input-buffer-vector buffer))
         (max-length (or byte-count (length sequence)))
         (start (fast-io:buffer-position buffer))
         (end (min (length sequence) (+ start max-length)))
         (index (if null-terminated-p
                    (position 0 sequence :start start :end end)
                    end)))
    (- index start)))

(defun split-string (string delimiter)
  (let ((pos (position delimiter string)))
    (values (subseq string 0 pos)
            (subseq string (1+ pos)))))

(defun octets= (octets1 octets2)
  (equalp octets1 (fast-io:octets-from octets2)))

(defun parse-bytes (buffer count)
  (let ((octet-vector (fast-io:make-octet-vector count)))
    (fast-io:fast-read-sequence octet-vector buffer)
    octet-vector))

(defun parse-uint/be (buffer byte-count)
  (loop :with value = 0
        :for i :from (* (1- byte-count) 8) :downto 0 :by 8
        :for byte = (fast-io:fast-read-byte buffer)
        :do (setf (ldb (byte 8 i) value) byte)
        :finally (return value)))

(defun parse-uint/le (buffer byte-count)
  (loop :with value = 0
        :for i :below (* byte-count 8) :by 8
        :for byte = (fast-io:fast-read-byte buffer)
        :do (setf (ldb (byte 8 i) value) byte)
        :finally (return value)))

(defun parse-int/be (buffer byte-count)
  (let ((value (parse-uint/be buffer byte-count))
        (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun parse-int/le (buffer byte-count)
  (let* ((value (parse-uint/le buffer byte-count))
         (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun parse-string (buffer &key byte-count (encoding :ascii) null-terminated-p)
  (let ((octet-vector (fast-io:make-octet-vector
                       (get-string-length buffer
                                          byte-count
                                          null-terminated-p))))
    (fast-io:fast-read-sequence octet-vector buffer)
    (when null-terminated-p
      (fast-io:fast-read-byte buffer))
    (babel:octets-to-string octet-vector :encoding encoding)))
