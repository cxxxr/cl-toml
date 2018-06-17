(in-package :cl-toml)

(defun make-adjustable-string ()
  (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character))

(defun make-adjustable-vector (&rest args)
  (make-array (length args) :initial-contents args :adjustable t :fill-pointer 0))

(defun list-to-vector (list)
  (make-array (length list) :initial-contents list))

(defun vector-last (vector)
  (aref vector (1- (length vector))))

(defun (setf vector-last) (value vector)
  (setf (aref vector (1- (length vector))) value))

(defun make-plist-iterate (plist)
  (lambda ()
    (if (null plist)
        nil
        (multiple-value-prog1 (values t (first plist) (second plist))
          (setf plist (cddr plist))))))

(defun make-alist-iterate (alist)
  (lambda ()
    (if (null alist)
        nil
        (let ((elt (pop alist)))
          (values t (car elt) (cdr elt))))))

(defun alist-put-end (table key value)
  (loop :for prev := nil :then rest
        :for rest :on table
        :for elt := (first rest)
        :do (when (equal (car elt) key)
              (setf (cdr elt) value)
              (return))
        :finally (cond ((null prev)
                        (setf table (acons key value nil)))
                       (t
                        (setf (cdr prev) (list (cons key value))))))
  table)

(defmacro do-sequence ((var sequence) &body body)
  `(map nil (lambda (,var) ,@body) ,sequence))
