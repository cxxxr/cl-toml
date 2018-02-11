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
