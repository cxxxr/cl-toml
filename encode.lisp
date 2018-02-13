(in-package :toml)

(defvar *inline-value-p* nil)
(defvar *table-name-stack* '())

(defun table-value-p (x)
  (or (hash-table-p x)
      (trivial-types:property-list-p x)
      (trivial-types:association-list-p x)))

(defun array-p (x)
  (or (and (listp x)
           (not (trivial-types:property-list-p x)))
      (vectorp x)))

(defun table-array-p (x)
  (and (array-p x)
       (or (zerop (length x))
           (table-value-p (elt x 0)))))

(defgeneric encode (value &optional stream))

(defmethod encode ((value ratio) &optional (stream *standard-output*))
  (encode (coerce value 'double-float) stream))

(defmethod encode ((value float) &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (format stream "~F" (coerce value 'double-float)))
  value)

(defmethod encode ((value integer) &optional (stream *standard-output*))
  (princ value stream))

(defmethod encode ((value local-time:timestamp) &optional (stream *standard-output*))
  (princ value stream))

(defparameter *char-replacement*
  '((#\backspace . "\\b")
    (#\tab . "\\t")
    (#\newline . "\\n")
    (#\return . "")
    (#\" "\\\"")
    (#\/ "\\/")
    (#\\ "\\\\")))

(defmethod encode ((value string) &optional (stream *standard-output*))
  (write-char #\" stream)
  (loop :for c :across value
        :do (let ((replacement (cdr (assoc c *char-replacement*))))
              (if replacement
                  (write-string replacement stream)
                  (write-char c stream))))
  (write-char #\" stream)
  value)

(defmethod encode ((value (eql t)) &optional (stream *standard-output*))
  (write-string "true" stream))

(defmethod encode ((value (eql 'true)) &optional (stream *standard-output*))
  (write-string "true" stream))

(defmethod encode ((value (eql 'false)) &optional (stream *standard-output*))
  (write-string "false" stream))

(defmacro with-aggregate ((stream opening-char closing-char) &body body)
  (alexandria:with-unique-names (first)
    `(let ((,first t))
       (write-char ,opening-char ,stream)
       (macrolet ((with-element (&body body)
                    `(progn
                       (if ,',first
                           (setf ,',first nil)
                           (write-string ", " stream))
                       ,@body)))
         ,@body)
       (write-char ,closing-char ,stream))))

(defun encode-sequence (sequence stream)
  (with-aggregate (stream #\[ #\])
    (let ((*inline-value-p* t))
      (do-sequence (e sequence)
        (with-element (encode e stream))))))

(defun encode-inline-table (iter stream)
  (with-aggregate (stream #\{ #\})
    (loop
     (multiple-value-bind (more key value) (funcall iter)
       (if (not more)
           (return)
           (with-element
            (format stream "~A = " key)
            (encode value stream)))))))

(defun word-string-p (string)
  (loop :for c :across string
        :always (or (alphanumericp c) (char= c #\_))))

(defun encode-key-value (key value stream)
  (flet ((table-header (arrayp)
           (write-string (if arrayp "[[" "[") stream)
           (loop :for rest :on (reverse *table-name-stack*)
                 :for name := (first rest)
                 :do (if (word-string-p name)
                         (write-string name stream)
                         (encode (format nil "~A" name) stream))
                     (when (rest rest) (write-char #\. stream)))
           (write-string (if arrayp "]]" "]") stream)
           (terpri stream)))
    (cond ((table-array-p value)
           (let ((*table-name-stack* (cons key *table-name-stack*)))
             (do-sequence (e value)
               (table-header t)
               (encode e stream)
               (terpri stream))))
          ((table-value-p value)
           (let ((*table-name-stack* (cons key *table-name-stack*)))
             (table-header nil)
             (encode value stream)))
          (t
           (encode key stream)
           (write-string " = " stream)
           (encode value stream)
           (terpri stream)))))

(defun encode-table (iter stream)
  (if *inline-value-p*
      (encode-inline-table iter stream)
      (let ((cont '()))
        (loop
         (multiple-value-bind (more key value) (funcall iter)
           (cond ((not more)
                  (return))
                 ((or (table-value-p value) (table-array-p value))
                  (push (cons key value) cont))
                 (t
                  (encode-key-value key value stream)))))
        (loop :for (key . value) :in (nreverse cont)
              :do (encode-key-value key value stream)))))

(defmethod encode ((value hash-table) &optional (stream *standard-output*))
  (with-hash-table-iterator (iter value)
    (encode-table (lambda () (iter)) stream)))

(defmethod encode ((value list) &optional (stream *standard-output*))
  (cond ((trivial-types:property-list-p value)
         (encode-table (make-plist-iterate value) stream))
        ((trivial-types:association-list-p value)
         (encode-table (make-alist-iterate value) stream))
        (t
         (encode-sequence value stream))))

(defmethod encode ((value vector) &optional (stream *standard-output*))
  (encode-sequence value stream))
