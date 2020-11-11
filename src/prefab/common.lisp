(in-package #:ndjinn)

(defclass prefab ()
  ((%name :reader name
          :initarg :name)
   (%data :reader data
          :initarg :data)
   (%root :reader root)
   (%nodes :reader nodes
           :initform (u:dict #'equal))
   (%masters :accessor masters
             :initform nil)
   (%slaves :accessor slaves
            :initform nil)
   (%factory :reader factory
             :initarg :factory)))

(defclass prefab-node ()
  ((%prefab :reader prefab
            :initarg :prefab)
   (%path :reader path
          :initarg :path)
   (%parent :reader parent
            :initarg :parent)
   (%options :reader options
             :initarg :options
             :initform nil)
   (%template :reader template
              :initarg :template
              :initform nil)
   (%component-types :reader component-types
                     :initform (u:dict #'eq))
   (%component-args :reader component-args
                    :initarg :component-args
                    :initform (make-nested-dict #'eq :self :resolved))))

(defclass prefab-factory ()
  ((%prefab-name :reader prefab-name
                 :initarg :prefab-name)
   (%current-node :reader current-node
                  :initform nil)
   (%entities :reader entities
              :initform (u:dict #'equal))
   (%func :reader func)))

(defclass prefab-reference ()
  ((%func :reader func
          :initarg :func)))

(u:define-printer (prefab stream)
  (format stream "~s" (name prefab)))

(u:define-printer (prefab-node stream)
  (format stream "~{~a~^/~}" (path prefab-node)))

(u:define-printer (prefab-factory stream)
  (format stream "~s" (prefab-name prefab-factory)))

;; Create a fresh prefab object. This only creates an instance with the raw data
;; needed to begin parsing. NOTE: This is only called if a prefab with the given
;; name does not yet exist when defining a prefab.
(defun make-prefab (name data)
  (let ((prefab (make-instance 'prefab
                               :name name
                               :data data
                               :factory (make-prefab-factory name))))
    (setf (u:href =meta/prefabs= name) prefab)))

;; Reset an existing prefab object. This resets an existing prefab instance with
;; new raw data so we don't lose the object identity. NOTE: This is only called
;; if a prefab with the given name already exists when defining a prefab.
(defun reset-prefab (name data)
  (let ((prefab (u:href =meta/prefabs= name)))
    (with-slots (%data %root %nodes %factory) prefab
      (setf %data data
            %root nil
            %factory (make-prefab-factory name))
      (clrhash %nodes))))

(defun make-prefab-factory (name)
  (make-instance 'prefab-factory :prefab-name name))

(defun find-prefab-node-template (spec path)
  (let* ((spec (u:ensure-list spec))
         (prefab (u:href =meta/prefabs= (first spec))))
    (or (and prefab (u:href (nodes prefab) spec))
        (error "Template ~{~a~^/~} not found for prefab node ~{~a~^/~}."
               (u:ensure-list spec) path))))
