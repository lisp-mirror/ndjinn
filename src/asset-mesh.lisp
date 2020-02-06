(in-package #:%pyx.asset.mesh)

(a:define-constant +attribute-locations+
    '(("POSITION" . 0)
      ("NORMAL" . 1)
      ("TANGENT" . 2)
      ("COLOR_0" . 3)
      ("TEXCOORD_0" . 4)
      ("TEXCOORD_1" . 5)
      ("JOINTS_0" . 6)
      ("WEIGHTS_0" . 7))
  :test #'equal)

(defstruct (gltf (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  file-name
  buffer
  parse-tree
  json
  buffers
  allocated-views
  (meshes (u:dict #'equalp)))

(defstruct (datastream (:conc-name nil)
                       (:predicate nl)
                       (:copier nil))
  header
  chunks)

(defstruct (header (:conc-name nil)
                   (:predicate nil)
                   (:copier nil))
  magic
  version
  format-length)

(defstruct (chunk (:predicate nil)
                  (:copier nil))
  length
  type
  data)

(defstruct (mesh (:conc-name nil)
                 (:predicate nil)
                 (:copier nil))
  name
  primitives)

(defstruct (primitive (:constructor %make-primitive)
                      (:conc-name nil)
                      (:predicate nil)
                      (:copier nil))
  vao
  mode
  element-count
  component-type
  index-buffer
  draw-func)

(u:define-printer (chunk stream :type t)
  (format stream "~s" (get-chunk-type chunk)))

(defun get-property (gltf key &optional object)
  (let ((object (or object (json gltf))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json gltf)) key))))

(defun get-chunk-type (chunk)
  (case (chunk-type chunk)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun parse-header (gltf)
  (let* ((header (make-header))
         (buffer (fast-io:make-input-buffer
                  :vector (parse:parse-bytes (buffer gltf) 12)))
         (magic (parse:parse-string buffer :byte-count 4)))
    (if (string= magic "glTF")
        (setf (magic header) magic
              (version header) (parse:parse-uint/le buffer 4)
              (format-length header) (parse:parse-uint/le buffer 4))
        (error "Invalid glTF2 file."))
    header))

(defgeneric parse-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let ((buffer (fast-io:make-input-buffer
                   :vector (parse:parse-bytes (buffer gltf)
                                              (chunk-length chunk)))))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-chunk-data (gltf (chunk-type (eql :json-content)) chunk
                             &key buffer)
  (let ((data (parse:parse-string buffer :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk
                             &key buffer)
  (loop :with buffers = (get-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (parse:parse-bytes buffer size))
        :finally (setf (buffers gltf) data))
  nil)

(defmethod parse-chunk-data (gltf (chunk-type (eql :unknown)) chunk &key buffer)
  (declare (ignore buffer))
  (warn "Ignoring an unknown chunk type."))

(defun parse-chunk (gltf)
  (let* ((buffer (buffer gltf))
         (chunk (make-chunk :length (parse:parse-uint/le buffer 4)
                            :type (parse:parse-uint/le buffer 4))))
    (setf (chunk-data chunk) (parse-chunk-data
                              gltf (get-chunk-type chunk) chunk))))

(defun parse-chunks (gltf)
  (loop :with stream = (fast-io:input-buffer-stream (buffer gltf))
        :until (= (file-position stream) (file-length stream))
        :collect (parse-chunk gltf)))

(defun parse-datastream (gltf)
  (make-datastream :header (parse-header gltf) :chunks (parse-chunks gltf)))

(defun parse-meshes (gltf)
  (loop :for mesh :in (get-property gltf "meshes")
        :for index :from 0
        :for name = (or (get-property gltf "name" mesh)
                        (format nil "~a~d" (name gltf) index))
        :for primitives = (map
                           'vector
                           (lambda (x)
                             (make-primitive gltf x))
                           (get-property gltf "primitives" mesh))
        :do (setf (u:href (meshes gltf) name)
                  (make-mesh :name name :primitives primitives))))

(defun get-component-type (gltf accessor)
  (ecase (get-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-component-count (data-type)
  (ecase (a:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-attribute-location (name)
  (u:alist-get +attribute-locations+ name :test #'string=))

(defun get-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun get-primitive-mode (gltf primitive)
  (case (get-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun find-mesh (gltf index)
  (let ((meshes (get-property gltf "meshes")))
    (when (>= index (length meshes))
      (error "Mesh index ~d not found." index))
    (get-property gltf "primitives" (elt meshes index))))

(defun make-buffer (gltf target accessor)
  (let ((buffer-view-index (get-property gltf "bufferView" accessor)))
    (unless (find buffer-view-index (allocated-views gltf))
      (let* ((buffer-view (elt (get-property gltf "bufferViews")
                               buffer-view-index))
             (index (get-property gltf "buffer" buffer-view))
             (offset (+ (or (get-property gltf "byteOffset" accessor) 0)
                        (or (get-property gltf "byteOffset" buffer-view) 0)))
             (size (get-property gltf "byteLength" buffer-view))
             (buffer (aref (buffers gltf) index))
             (data (static-vectors:make-static-vector
                    size
                    :element-type 'u:octet
                    :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (static-vectors:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (static-vectors:free-static-vector data)
        (push buffer-view-index (allocated-views gltf))
        buffer-id))))

(defun configure-attribute (gltf attribute accessor)
  (let* ((buffer-view (elt (get-property gltf "bufferViews")
                           (get-property gltf "bufferView" accessor)))
         (type (get-component-type gltf accessor))
         (count (get-component-count (get-property gltf "type" accessor)))
         (stride (or (get-property gltf "byteStride" buffer-view) 0))
         (location (get-attribute-location attribute))
         (normalize (get-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-vertex-buffers (gltf primitive data)
  (jsown:do-json-keys (attr accessor-id)
                      (get-property gltf "attributes" data)
    (let* ((accessor (elt (get-property gltf "accessors") accessor-id))
           (count (get-property gltf "count" accessor)))
      (make-buffer gltf :array-buffer accessor)
      (configure-attribute gltf attr accessor)
      (when (string= attr "POSITION")
        (setf (element-count primitive) count)))))

(defun make-index-buffer (gltf primitive data)
  (a:when-let* ((indices (get-property gltf "indices" data))
                (accessor (elt (get-property gltf "accessors") indices)))
    (setf (element-count primitive) (get-property gltf "count" accessor)
          (component-type primitive) (get-component-type gltf accessor)
          (index-buffer primitive) (make-buffer
                                    gltf :element-array-buffer accessor))))

(defun draw-primitive/vertices (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (vao primitive))
  (gl:draw-arrays-instanced
   (mode primitive) 0 (element-count primitive) instance-count))

(defun draw-primitive/indexed (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (vao primitive))
  (gl:bind-buffer :element-array-buffer (index-buffer primitive))
  (%gl:draw-elements-instanced (mode primitive)
                               (element-count primitive)
                               (component-type primitive)
                               0
                               instance-count))

(defun make-draw-func (primitive)
  (setf (draw-func primitive)
        (if (index-buffer primitive)
            (lambda (x) (draw-primitive/indexed primitive x))
            (lambda (x) (draw-primitive/vertices primitive x)))))

(defun make-primitive (gltf data)
  (let ((primitive (%make-primitive :vao (gl:create-vertex-array)
                                    :mode (get-primitive-mode gltf data))))
    (gl:bind-vertex-array (vao primitive))
    (make-vertex-buffers gltf primitive data)
    (make-index-buffer gltf primitive data)
    (make-draw-func primitive)
    primitive))

(defun load (path)
  (u:with-binary-input (in path)
    (let* ((buffer (fast-io:make-input-buffer :stream in))
           (gltf (make-gltf :file-name (pathname-name path) :buffer buffer)))
      (setf (parse-tree gltf) (parse-datastream gltf))
      (parse-meshes gltf)
      (setf (buffers gltf) nil)
      gltf)))
