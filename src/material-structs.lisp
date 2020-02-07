(in-package #:%pyx.material)

(defstruct (spec (:constructor %make-spec)
                 (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  master
  slaves
  shader
  (spec-uniforms (util:make-nested-dict #'eq :self :resolved))
  pass
  spec-framebuffer
  spec-attachments
  (render-func (constantly nil)))

(defstruct (material (:constructor %make-material)
                     (:conc-name nil)
                     (:predicate nil)
                     (:copier nil))
  spec
  entity
  (uniforms (u:dict #'eq))
  framebuffer
  attachments
  (texture-unit-state 0))

(defstruct (uniform (:predicate nil)
                    (:copier nil))
  key
  type
  resolved-type
  value
  func)

(u:define-printer (material stream :type t :identity t)
  (format stream "~s" (name (spec material))))
