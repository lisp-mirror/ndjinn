(in-package #:%pyx.geometry)

(defstruct (attribute (:conc-name nil)
                      (:predicate nil)
                      (:copier nil))
  attribute-name
  normalize-p
  attribute-type
  out-type
  element-count)

(defun make-attributes (spec)
  (let ((attrs (u:dict #'eq))
        (order))
    (dolist (attribute spec)
      (destructuring-bind (name &key normalize (type :float) (out-type type)
                                  (count 1))
          attribute
        (push name order)
        (setf (u:href attrs name)
              (make-attribute :attribute-name name
                              :normalize-p normalize
                              :attribute-type (a:make-keyword type)
                              :out-type (a:make-keyword out-type)
                              :element-count count))))
    (values attrs (nreverse order))))

(defun get-attribute-size (attribute)
  (* (element-count attribute)
     (ecase (attribute-type attribute)
       ((:byte :unsigned-byte) 1)
       ((:short :unsigned-short :half-float) 2)
       ((:int :unsigned-int :float :fixed) 4)
       (:double 8))))

(defun configure-attribute (attribute index stride offset divisor)
  (let ((normalize-p (if (normalize-p attribute) 1 0)))
    (ecase (out-type attribute)
      ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
       (%gl:vertex-attrib-ipointer index
                                   (element-count attribute)
                                   (attribute-type attribute)
                                   stride
                                   offset))
      ((:half-float :float :fixed)
       (%gl:vertex-attrib-pointer index
                                  (element-count attribute)
                                  (attribute-type attribute)
                                  normalize-p
                                  stride
                                  offset))
      (:double
       (%gl:vertex-attrib-lpointer index
                                   (element-count attribute)
                                   (attribute-type attribute)
                                   stride
                                   offset)))
    (%gl:vertex-attrib-divisor index divisor)))
