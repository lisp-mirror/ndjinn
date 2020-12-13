(in-package :ndjinn)

(defstruct (scene-spec
            (:constructor %make-scene-spec)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (pass-order nil :type list)
  (pass-table (u:dict #'eq) :type hash-table)
  (draw-order (u:dict #'eq) :type hash-table)
  (collision-plan nil :type symbol)
  (sub-trees nil :type list)
  (viewports nil :type list))

(defstruct (scene
            (:predicate nil)
            (:copier nil))
  (spec nil :type (or scene-spec null))
  (loaded-p nil :type boolean)
  viewports
  node-tree
  (materials (u:dict #'eq) :type hash-table)
  (passes nil :type list)
  (prefabs (u:dict #'eq) :type hash-table)
  collision-system
  picked-entity
  (uuids (u:dict #'eq) :type hash-table))

(u:define-printer (scene-spec stream)
  (format stream "~s" (scene-spec-name scene-spec)))

(u:define-printer (scene stream :identity t)
  (format stream "~s" (scene-spec-name (scene-spec scene))))
