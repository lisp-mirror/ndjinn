(in-package #:pyx)

(defstruct (avl-tree (:constructor %make-avl-tree))
  sentinel
  root
  item-type
  (key #'identity :type function)
  (sorter #'< :type function)
  (hash-test #'eql :type function))

(u:define-printer (avl-tree stream :identity t :type nil)
  (format stream "AVL-TREE"))

(defstruct (avl-tree-node (:constructor %make-avl-tree-node)
                          (:predicate nil))
  tree
  key
  data
  parent
  left
  right
  (height 0 :type fixnum)
  (balance-factor 0 :type fixnum))

(u:define-printer (avl-tree-node stream :identity t :type nil)
  (format stream "AVL-TREE-NODE"))

(u:fn-> avl-tree-valid-p (avl-tree) boolean)
(defun avl-tree-valid-p (tree)
  (declare (optimize speed))
  (let ((previous nil))
    (labels ((%check (node sorter)
               (declare (function sorter))
               (when (avl-tree-node-p node)
                 (when (or (null (%check (avl-tree-node-left node) sorter))
                           (and previous
                                (funcall sorter
                                         (avl-tree-node-key node)
                                         (avl-tree-node-key previous))))
                   (return-from %check))
                 (setf previous node)
                 (return-from %check
                   (%check (avl-tree-node-right node) sorter)))
               t))
      (%check (avl-tree-root tree) (avl-tree-sorter tree)))))

(u:fn-> avl-tree-node-p ((or avl-tree-node null)) (or avl-tree-node null))
(declaim (inline avl-tree-node-p))
(defun avl-tree-node-p (node)
  (declare (optimize speed))
  (unless (and node (eq node (avl-tree-sentinel (avl-tree-node-tree node))))
    node))

(u:fn-> make-avl-tree (&key (:item-type symbol)
                            (:key function)
                            (:sort function)
                            (:hash-test function))
        avl-tree)
(defun make-avl-tree (&key item-type
                        (key #'identity) (sort #'<) (hash-test #'eql))
  (declare (optimize speed))
  (unless item-type
    (error "Must specify :ITEM-TYPE denoting the type of items stored in the ~
            tree."))
  (let* ((tree (%make-avl-tree
                :item-type item-type
                :key key
                :sorter sort
                :hash-test hash-test))
         (sentinel (make-avl-tree-node tree nil)))
    (setf (avl-tree-sentinel tree) sentinel
          (avl-tree-root tree) sentinel)
    tree))

(u:fn-> make-avl-tree-node (avl-tree t) avl-tree-node)
(defun make-avl-tree-node (tree item)
  (declare (optimize speed))
  (let ((sentinel (avl-tree-sentinel tree))
        (node (%make-avl-tree-node
               :tree tree
               :key (when item (funcall (avl-tree-key tree) item))
               :data (u:dict (avl-tree-hash-test tree) item item))))
    (setf (avl-tree-node-left node) sentinel
          (avl-tree-node-right node) sentinel)
    node))

(u:fn-> avl-tree/walk (avl-tree function) null)
(defun avl-tree/walk (tree func)
  (declare (optimize speed))
  (a:when-let ((node (avl-tree-node-p (avl-tree-root tree))))
    (loop :with current = node
          :with stack
          :do (cond
                ((avl-tree-node-p current)
                 (push current stack)
                 (setf current (avl-tree-node-left current)))
                (stack
                 (setf current (pop stack))
                 (u:do-hash-keys (k (avl-tree-node-data current))
                   (funcall func k))
                 (setf current (avl-tree-node-right current)))
                (t (loop-finish))))))

(u:fn-> avl-tree/transplant (avl-tree-node avl-tree-node) avl-tree-node)
(defun avl-tree/transplant (node1 node2)
  (declare (optimize speed))
  (let ((parent (avl-tree-node-parent node1)))
    (cond
      ((not (avl-tree-node-p parent))
       (setf (avl-tree-root (avl-tree-node-tree node1)) node2))
      ((eq node1 (avl-tree-node-left parent))
       (setf (avl-tree-node-left parent) node2))
      (t (setf (avl-tree-node-right parent) node2)))
    (setf (avl-tree-node-parent node2) (avl-tree-node-parent node1))))

(u:fn-> avl-tree/rotate/left (avl-tree-node) avl-tree-node)
(defun avl-tree/rotate/left (node)
  (declare (optimize speed))
  (let ((p (avl-tree-node-parent node))
        (b (avl-tree-node-right node)))
    (setf (avl-tree-node-right node) (avl-tree-node-left b))
    (when (avl-tree-node-p (avl-tree-node-left b))
      (setf (avl-tree-node-parent (avl-tree-node-left b)) node))
    (setf (avl-tree-node-left b) node
          (avl-tree-node-parent node) b
          (avl-tree-node-parent b) p)
    (when (avl-tree-node-p p)
      (if (eq (avl-tree-node-right p) node)
          (setf (avl-tree-node-right p) b)
          (setf (avl-tree-node-left p) b)))
    (if (zerop (avl-tree-node-balance-factor b))
        (setf (avl-tree-node-balance-factor b) -1
              (avl-tree-node-balance-factor node) 1)
        (setf (avl-tree-node-balance-factor b) 0
              (avl-tree-node-balance-factor node) 0))
    b))

(u:fn-> avl-tree/rotate/right (avl-tree-node) avl-tree-node)
(defun avl-tree/rotate/right (node)
  (declare (optimize speed))
  (let ((p (avl-tree-node-parent node))
        (b (avl-tree-node-left node)))
    (setf (avl-tree-node-left node) (avl-tree-node-right b))
    (when (avl-tree-node-p (avl-tree-node-right b))
      (setf (avl-tree-node-parent (avl-tree-node-right b)) node))
    (setf (avl-tree-node-right b) node
          (avl-tree-node-parent node) b
          (avl-tree-node-parent b) p)
    (when (avl-tree-node-p p)
      (if (eq (avl-tree-node-left p) node)
          (setf (avl-tree-node-left p) b)
          (setf (avl-tree-node-right p) b)))
    (if (zerop (avl-tree-node-balance-factor b))
        (setf (avl-tree-node-balance-factor b) 1
              (avl-tree-node-balance-factor node) -1)
        (setf (avl-tree-node-balance-factor b) 0
              (avl-tree-node-balance-factor node) 0))
    b))

(u:fn-> avl-tree/rotate/left-right (avl-tree-node) avl-tree-node)
(defun avl-tree/rotate/left-right (node)
  (declare (optimize speed))
  (let* ((z (avl-tree-node-left node))
         (new-root (avl-tree-node-right z))
         (new-root-balance (avl-tree-node-balance-factor new-root)))
    (avl-tree/rotate/left z)
    (avl-tree/rotate/right node)
    (case new-root-balance
      (-1 (setf (avl-tree-node-balance-factor node) 1
                (avl-tree-node-balance-factor z) 0))
      (0 (setf (avl-tree-node-balance-factor node) 0
               (avl-tree-node-balance-factor z) 0))
      (1 (setf (avl-tree-node-balance-factor node) 0
               (avl-tree-node-balance-factor z) -1)))
    (setf (avl-tree-node-balance-factor new-root) 0)
    new-root))

(u:fn-> avl-tree/rotate/right-left (avl-tree-node) avl-tree-node)
(defun avl-tree/rotate/right-left (node)
  (declare (optimize speed))
  (let* ((z (avl-tree-node-right node))
         (new-root (avl-tree-node-left z))
         (new-root-balance (avl-tree-node-balance-factor new-root)))
    (avl-tree/rotate/right z)
    (avl-tree/rotate/left node)
    (case new-root-balance
      (-1 (setf (avl-tree-node-balance-factor node) 0
                (avl-tree-node-balance-factor z) 1))
      (0 (setf (avl-tree-node-balance-factor node) 0
               (avl-tree-node-balance-factor z) 0))
      (1 (setf (avl-tree-node-balance-factor node) -1
               (avl-tree-node-balance-factor z) 0)))
    (setf (avl-tree-node-balance-factor new-root) 0)
    new-root))

(u:fn-> avl-tree/insert-rebalance (avl-tree-node) (or avl-tree-node null))
(defun avl-tree/insert-rebalance (new)
  (declare (optimize speed))
  (loop :with child = new
        :for node = (avl-tree-node-parent child)
        :while (avl-tree-node-p node)
        :do (if (eq child (avl-tree-node-left node))
                (ecase (decf (avl-tree-node-balance-factor node))
                  (0 (return))
                  (-1 (setf child node))
                  (-2 (let ((node-parent (avl-tree-node-parent node))
                            (new-root (if (= (avl-tree-node-balance-factor
                                              child)
                                             1)
                                          (avl-tree/rotate/left-right node)
                                          (avl-tree/rotate/right node))))
                        (if (avl-tree-node-p node-parent)
                            (return)
                            (return new-root)))))
                (ecase (incf (avl-tree-node-balance-factor node))
                  (0 (return))
                  (1 (setf child node))
                  (2 (let ((node-parent (avl-tree-node-parent node))
                           (new-root (if (= (avl-tree-node-balance-factor
                                             child)
                                            -1)
                                         (avl-tree/rotate/right-left node)
                                         (avl-tree/rotate/left node))))
                       (setf (avl-tree-node-parent new-root) node-parent)
                       (if (avl-tree-node-p node-parent)
                           (return)
                           (return new-root))))))))

(u:fn-> avl-tree/insert (avl-tree t) avl-tree-node)
(defun avl-tree/insert (tree item)
  (declare (optimize speed))
  (a:if-let ((node (avl-tree-node-p (nth-value 1 (avl-tree/find tree item)))))
    (progn
      (setf (u:href (avl-tree-node-data node) item) item)
      node)
    (let ((node (make-avl-tree-node tree item)))
      (loop :with sorter = (avl-tree-sorter tree)
            :with key = (avl-tree-node-key node)
            :with current = (avl-tree-root tree)
            :with parent = (avl-tree-sentinel tree)
            :while (avl-tree-node-p current)
            :do (setf parent current)
                (if (funcall sorter key (avl-tree-node-key current))
                    (setf current (avl-tree-node-left current))
                    (setf current (avl-tree-node-right current)))
            :finally (setf (avl-tree-node-parent node) parent)
                     (cond
                       ((not (avl-tree-node-p parent))
                        (setf (avl-tree-root tree) node))
                       ((funcall sorter key (avl-tree-node-key parent))
                        (setf (avl-tree-node-left parent) node))
                       (t (setf (avl-tree-node-right parent) node))))
      (a:when-let ((new-root (avl-tree/insert-rebalance node)))
        (setf (avl-tree-root tree) new-root))
      node)))

(u:fn-> avl-tree/delete-rebalance (avl-tree-node keyword)
        (or avl-tree-node null))
(defun avl-tree/delete-rebalance (new-root direction)
  (declare (optimize speed))
  (loop :for first-time = t :then nil
        :with child = new-root
        :for node = (avl-tree-node-parent child)
        :while (avl-tree-node-p node)
        :do (if (and (or (not first-time) (and first-time (eq direction :left)))
                     (eq child (avl-tree-node-left node)))
                (ecase (incf (avl-tree-node-balance-factor node))
                  (0 (setf child node))
                  (1 (return))
                  (2 (let ((node-parent (avl-tree-node-parent node))
                           (right-child (avl-tree-node-right node)))
                       (if (= (avl-tree-node-balance-factor right-child) -1)
                           (setf child (avl-tree/rotate/right-left node))
                           (setf child (avl-tree/rotate/left node)))
                       (setf (avl-tree-node-parent child) node-parent)
                       (cond
                         ((not (avl-tree-node-p node-parent))
                          (return child))
                         ((= (avl-tree-node-balance-factor right-child) -1)
                          (return))))))
                (ecase (decf (avl-tree-node-balance-factor node))
                  (0 (setf child node))
                  (-1 (return))
                  (-2 (let ((node-parent (avl-tree-node-parent node))
                            (left-child (avl-tree-node-left node)))
                        (if (= (avl-tree-node-balance-factor left-child) 1)
                            (setf child (avl-tree/rotate/left-right node))
                            (setf child (avl-tree/rotate/right node)))
                        (cond
                          ((not (avl-tree-node-p node-parent))
                           (return child))
                          ((= (avl-tree-node-balance-factor left-child) 1)
                           (return)))))))))

(u:fn-> avl-tree/delete (avl-tree t) (or avl-tree-node null))
(defun avl-tree/delete (tree item)
  (declare (optimize speed))
  (labels ((%delete (node)
             (if (and (avl-tree-node-p (avl-tree-node-left node))
                      (avl-tree-node-p (avl-tree-node-right node)))
                 (let ((replacement (avl-tree/min (avl-tree-node-right node))))
                   (setf (avl-tree-node-data node)
                         (avl-tree-node-data replacement))
                   (%delete replacement))
                 (let ((direction (if (eq node (avl-tree-node-left
                                                (avl-tree-node-parent node)))
                                      :left
                                      :right)))
                   (cond ((avl-tree-node-p (avl-tree-node-left node))
                          (avl-tree/transplant node (avl-tree-node-left node))
                          (avl-tree/delete-rebalance (avl-tree-node-left node)
                                                     direction))
                         ((avl-tree-node-p (avl-tree-node-right node))
                          (avl-tree/transplant node (avl-tree-node-right node))
                          (avl-tree/delete-rebalance (avl-tree-node-right node)
                                                     direction))
                         (t (avl-tree/transplant node (avl-tree-node-left node))
                            (avl-tree/delete-rebalance
                             (avl-tree-node-left node) direction)))))))
    (a:when-let ((node (avl-tree-node-p
                        (nth-value 1 (avl-tree/find tree item)))))
      (if (<= (hash-table-count (avl-tree-node-data node)) 1)
          (progn
            (a:when-let ((new-root (%delete node)))
              (setf (avl-tree-root tree) new-root))
            (setf (avl-tree-node-parent (avl-tree-sentinel tree))
                  (avl-tree-sentinel tree)))
          (remhash item (avl-tree-node-data node)))
      node)))

(u:fn-> avl-tree/find (avl-tree t) (values &optional t avl-tree-node))
(defun avl-tree/find (tree item)
  (declare (optimize speed))
  (labels ((%find (node key sorter)
             (declare (function sorter))
             (a:when-let ((result (and (avl-tree-node-p node)
                                       (avl-tree-node-key node))))
               (cond
                 ((funcall sorter key result)
                  (%find (avl-tree-node-left node) key sorter))
                 ((funcall sorter result key)
                  (%find (avl-tree-node-right node) key sorter))
                 (t node)))))
    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (when (typep item (avl-tree-item-type tree))
        (a:when-let ((node (%find (avl-tree-root tree)
                                  (funcall (avl-tree-key tree) item)
                                  (avl-tree-sorter tree))))
          (values (u:href (avl-tree-node-data node) item)
                  node))))))

(u:fn-> avl-tree/min (avl-tree-node) (or avl-tree-node null))
(defun avl-tree/min (node)
  (declare (optimize speed))
  (when (avl-tree-node-p node)
    (loop :with current = node
          :for left = (avl-tree-node-left current)
          :while (avl-tree-node-p left)
          :do (setf current left)
          :finally (return current))))

(u:fn-> avl-tree/max (avl-tree-node) (or avl-tree-node null))
(defun avl-tree/max (node)
  (declare (optimize speed))
  (when (avl-tree-node-p node)
    (loop :with current = node
          :for right = (avl-tree-node-right current)
          :while (avl-tree-node-p right)
          :do (setf current right)
          :finally (return current))))
