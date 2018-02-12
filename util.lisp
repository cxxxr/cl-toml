(in-package :toml)

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

(defmacro do-sequence ((var sequence) &body body)
  `(map nil (lambda (,var) ,@body) ,sequence))
