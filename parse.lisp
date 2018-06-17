(in-package :cl-toml)

(defparameter *table-as* :hash-table)
(defparameter *array-as* :vector)

(defun make-table ()
  (ecase *table-as*
    (:hash-table
     (make-hash-table :test #'equal))
    ((:alist :plist)
     (list))))

(defun alist-to-table (alist)
  (ecase *table-as*
    (:hash-table
     (alexandria:alist-hash-table alist :test 'equal))
    (:plist
     (alexandria:alist-plist alist))
    (:alist
     alist)))

(defun as-table-p (x)
  (ecase *table-as*
    (:hash-table
     (hash-table-p x))
    (:plist
     (trivial-types:property-list-p x))
    (:alist
     (trivial-types:association-list-p x))))

(defun table-get (table key)
  (ecase *table-as*
    (:hash-table
     (gethash key table))
    (:plist
     (getf table (intern key :keyword)))
    (:alist
     (alexandria:assoc-value table key :test #'equal))))

(defun table-put (table key value)
  (ecase *table-as*
    (:hash-table
     (setf (gethash key table) value)
     table)
    (:plist
     (setf (getf table (intern key :keyword)) value)
     table)
    (:alist
     (alist-put-end table key value))))

(defun make-toml-array ()
  (ecase *array-as*
    (:vector
     (make-adjustable-vector))
    (:list
     (list))))

(defun toml-array-push (array value)
  (ecase *array-as*
    (:vector
     (vector-push-extend value array)
     array)
    (:list
     (nconc array (list value)))))

(defun toml-array-last (array)
  (ecase *array-as*
    (:vector
     (vector-last array))
    (:list
     (car (last array)))))

(defun (setf toml-array-last) (value array)
  (ecase *array-as*
    (:vector
     (setf (vector-last array) value))
    (:list
     (setf (car (last array)) value))))

(defun list-to-toml-array (list)
  (ecase *array-as*
    (:vector
     (list-to-vector list))
    (:list
     list)))

(defrule space*
    (* #\space))

(defrule comment
    (and #\# (* (not #\newline))))

(defrule whitespace*
    (* (or #\space #\newline comment)))

(defrule end-of-line
    (and space* (? comment) (* #\newline) (or #\newline (string 0))))

(defrule bare-key
    (+ (bare-key-character-p character))
  (:lambda (list)
    (coerce list 'string)))

(defrule table-key
    (and space*
         (or string bare-key))
  (:destructure (spaces key)
   (declare (ignore spaces))
   key))

(defrule table-pair-delim
    (and space* #\= space*))

(defrule table-pair
    (and table-key table-pair-delim value space*)
  (:destructure (key delim value spaces)
   (declare (ignore delim spaces))
   (cons key value)))

(defrule table-pairs
    (* (and whitespace* table-pair end-of-line))
  (:lambda (result)
    (mapcar #'second result)))

(defrule table-header-names
    (and table-key (* (and space* #\. space* table-key)))
  (:destructure (first-key rest-keys)
   (cons first-key (mapcar #'alexandria:lastcar rest-keys))))

(defrule table-header
    (and (and whitespace* #\[) table-header-names (and space* #\]) end-of-line)
  (:destructure ([ names ] eol)
   (declare (ignore [ ] eol))
   (cons nil names)))

(defrule table-array-header
    (and (and whitespace* #\[ space* #\[) table-header-names (and space* #\] space* #\]) end-of-line)
  (:destructure ([[ names ]] eol)
   (declare (ignore [[ ]] eol))
   (cons t names)))

(defrule table
    (and (or table-header table-array-header)
         table-pairs))

(defrule inline-table
    (or (and #\{ space* #\})
        (and #\{ table-pair (* (and #\, table-pair)) #\}))
  (:lambda (s)
    (if (= 3 (length s))
        (make-table)
        (alist-to-table
         (cons (second s)
               (mapcar #'second (third s)))))))

(defun convert-escape-sequence (string multiline-p)
  (let ((i 0))
    (labels ((peek ()
               (if (< i (length string)) (char string i)))
             (next-char ()
               (prog1 (peek) (incf i)))
             (try (n)
               (ignore-errors
                 (let ((code (code-char (parse-integer string :start i :end (+ i n) :radix 16))))
                   (when code
                     (incf i n)
                     code)))))
      (let ((buffer (make-adjustable-string)))
        (loop :for c := (next-char)
              :while c
              :do (if (char= c #\\)
                      (let ((c (next-char)))
                        (cond ((and multiline-p (char= c #\newline))
                               (loop :for c1 := (peek)
                                     :while (member c1 '(#\space #\newline))
                                     :do (next-char)))
                              (t
                               (let ((escape-char
                                      (ecase c
                                        (#\b #\backspace)
                                        (#\t #\tab)
                                        (#\n #\newline)
                                        (#\f #\page)
                                        (#\r nil)
                                        ((#\" #\/ #\\) c)
                                        (#\u (or (try 8) (try 4))))))
                                 (cond (escape-char
                                        (vector-push-extend escape-char buffer))
                                       ((not (char= c #\r))
                                        (vector-push-extend #\\ buffer)
                                        (vector-push-extend c buffer)))))))
                      (vector-push-extend c buffer)))
        buffer))))

(defun transform-string-contents (list multiline-p)
  (let ((buffer (make-adjustable-string)))
    (loop :for e :in list
          :for c := (if (consp e) (char (second e) 0) e)
          :do (vector-push-extend c buffer))
    (convert-escape-sequence buffer multiline-p)))

(defun string-char-p (char)
  (not (or (eql char #\") (eql char #\newline))))

(defrule string-contents
    (* (or (and #\\ character) (string-char-p character)))
  (:lambda (list)
    (transform-string-contents
     (mapcan (lambda (x)
               (if (consp x)
                   (list (char (first x) 0)
                         (second x))
                   (list x)))
             list)
     nil)))

(defrule string
    (and #\" string-contents #\")
  (:destructure (q1 string q2)
   (declare (ignore q1 q2))
   string))

(defrule multiline-string-contents
    (* (or (and #\\ #\") (not multiline-string-quotes)))
  (:lambda (s)
    (transform-string-contents s t)))

(defrule multiline-string-quotes
    (and #\" #\" #\"))

(defrule multiline-string
    (and multiline-string-quotes
         (? #\newline)
         multiline-string-contents
         multiline-string-quotes)
  (:destructure (q1 first-newline string q2)
   (declare (ignore q1 first-newline q2))
   string))

(defrule literal-string-contents
    (* (literal-string-char-p character))
  (:lambda (list)
    list))

(defrule literal-string
    (and #\' literal-string-contents #\')
  (:destructure (q1 string q2)
   (declare (ignore q1 q2))
   (coerce string 'string)))

(defrule multiline-literal-string-contents
    (* (not multiline-literal-string-quotes))
  (:lambda (s)
    (coerce s 'string)))

(defrule multiline-literal-string-quotes
    (and #\' #\' #\'))

(defrule multiline-literal-string
    (and multiline-literal-string-quotes
         (? #\newline)
         multiline-literal-string-contents
         multiline-literal-string-quotes)
  (:destructure (q1 first-newline string q2)
   (declare (ignore q1 first-newline q2))
   string))

(defrule digit
    (or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
  (:lambda (s)
    (char s 0)))

(defrule digits
    (+ (or digit #\_))
  (:lambda (list)
    (let ((buffer (make-adjustable-string)))
      (loop :for c :in list
            :unless (equal c "_")
            :do (vector-push-extend c buffer))
      buffer)))

(defrule integer
    digits)

(defrule float
    (and digits #\. digits)
  (:destructure (x dot y)
   (declare (ignore dot))
   (format nil "~A.~A" x y)))

(defrule e-number
    (and (or float integer) (or #\e #\E) (? (or #\+ #\-)) integer)
  (:destructure (m e s exp)
   (declare (ignore e))
   (if s
       (format nil "~Ae~A~A" m s exp)
       (format nil "~Ae~A" m exp))))

(defrule number
    (and (? (or #\+ #\-))
         (or e-number
             float
             integer))
  (:destructure (sign string)
   (read-from-string
    (if sign
        (format nil "~A~A" sign string)
        string))))

(defrule boolean
    (or "true" "false")
  (:lambda (s)
    (if (string= s "true")
        'true
        'false)))

(defrule 4digit
    (and digit digit digit digit))

(defrule 2digit
    (and digit digit))

(defrule full-date
    (and 4digit #\- 2digit #\- 2digit))

(defrule time-secfrac
    (and "." digits))

(defrule partial-time
    (and 2digit #\: 2digit #\: 2digit (? time-secfrac)))

(defrule time-num-offset
    (and (or #\+ #\-) 2digit #\: 2digit))

(defrule time-offset
    (or #\Z time-num-offset))

(defrule full-time
    (and partial-time time-offset))

(defrule datetime
    (and full-date (or #\t #\T) full-time)
  (:lambda (result)
    (local-time:parse-rfc3339-timestring
     (format nil "~{~A~}" (alexandria:flatten result)))))

(defrule array-element
    (and whitespace* value whitespace*)
  (:lambda (x)
    (second x)))

(defrule array
    (or (and #\[ whitespace* #\])
        (and #\[ array-element (* (and #\, array-element)) (? (and #\, whitespace*)) #\]))
  (:lambda (x)
    (if (= 3 (length x))
        (vector)
        (list-to-toml-array
         (cons (second x)
               (mapcar #'second (third x)))))))

(defrule value
    (or datetime
        boolean
        number
        multiline-literal-string
        literal-string
        multiline-string
        string
        inline-table
        array))

(defun linked-table (names table last-value arrayp)
  (unless table
    (setf table
          (if (and (null names) arrayp)
              (make-toml-array)
              (make-table))))
  (cond (names
         (cond ((as-table-p table)
                (setf table
                      (table-put table
                                 (first names)
                                 (linked-table (rest names)
                                               (table-get table (first names))
                                               last-value
                                               arrayp))))
               (t
                (setf (toml-array-last table)
                      (table-put (toml-array-last table)
                                 (first names)
                                 (linked-table (rest names)
                                               (table-get (toml-array-last table) (first names))
                                               last-value
                                               arrayp)))))
         table)
        (arrayp
         (toml-array-push table (alist-to-table last-value)))
        (t
         (loop :for (k . v) :in last-value
               :do (setf table (table-put table k v)))
         table)))

(defrule toplevel
    (and whitespace* table-pairs (* table))
  (:destructure (spaces pairs header-pairs)
   (declare (ignore spaces))
   (let ((table (make-table)))
     (loop :for (k . v) :in pairs
           :do (setf table (table-put table k v)))
     (loop :for ((arrayp . names) pairs) :in header-pairs
           :for count :from 0
           :do (setf table (linked-table names table pairs arrayp)))
     table)))

(defun parse (string &key ((:array-as *array-as*) *array-as*) ((:table-as *table-as*) *table-as*))
  (esrap:parse 'toplevel string))

(defun parse-file (filename &rest args &key ((:array-as *array-as*) *array-as*) ((:table-as *table-as*) *table-as*))
  (apply #'parse (uiop:read-file-string filename) args))
