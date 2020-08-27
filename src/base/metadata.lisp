(in-package #:net.mfiano.lisp.pyx)

(u:eval-always
  (defstruct (metadata
              (:predicate nil)
              (:copier nil))
    (asset-pools (u:dict #'eq) :type hash-table)
    (collider-plans (u:dict #'eq) :type hash-table)
    (components-type-order (u:dict #'eq) :type hash-table)
    (components-initargs (u:dict #'eq) :type hash-table)
    (components-static nil :type list)
    (config (u:dict #'eq) :type hash-table)
    (entity-mixins (u:dict #'equal) :type hash-table)
    (framebuffers (u:dict #'eq) :type hash-table)
    (geometry (u:dict #'eq) :type hash-table)
    (geometry-layouts (u:dict #'eq) :type hash-table)
    (materials (u:dict #'eq) :type hash-table)
    (prefabs (u:dict #'eq) :type hash-table)
    (render-passes (u:dict #'eq) :type hash-table)
    (scenes (u:dict #'eq) :type hash-table)
    (textures (u:dict #'eq) :type hash-table)
    (viewports (u:dict #'eq) :type hash-table)))

(glob:define-global-var =metadata= (make-metadata))
