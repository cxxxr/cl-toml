(defpackage :cl-toml-test
  (:use :cl :cl-toml :prove))
(in-package :cl-toml-test)

(plan nil)

(defparameter *example-text*
  "# This is a TOML document. Boom.

title = \"TOML Example\"

[owner]
name = \"Lance Uppercut\"
dob = 1979-05-27T07:32:00-08:00 # First class dates? Why not?

[database]
server = \"192.168.1.1\"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  [servers.beta]
  ip = \"10.0.0.2\"
  dc = \"eqdc10\"

[clients]
data = [ [\"gamma\", \"delta\"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  \"alpha\",
  \"omega\"
]
")

(defun parse-test-example (x)
  (is (gethash "title" x) "TOML Example" :test #'equal)
  (let ((owner (gethash "owner" x)))
    (is (hash-table-count owner) 2)
    (is (gethash "name" owner) "Lance Uppercut" :test #'equal)
    (is (gethash "dob" owner)
        (local-time:parse-rfc3339-timestring
         "1979-05-27T15:32:00.000000Z")
        :test #'local-time:timestamp=))
  (let ((database (gethash "database" x)))
    (is (hash-table-count database) 4)
    (is (gethash "server" database) "192.168.1.1" :test #'equal)
    (is (gethash "ports" database) #(8001 8001 8002) :test #'equalp)
    (is (gethash "connection_max" database) 5000)
    (is (gethash "enabled" database) 'true))
  (let ((servers (gethash "servers" x)))
    (is (hash-table-count servers) 2)
    (let ((alpha (gethash "alpha" servers)))
      (is (gethash "ip" alpha) "10.0.0.1")
      (is (gethash "dc" alpha) "eqdc10"))
    (let ((beta (gethash "beta" servers)))
      (is (gethash "ip" beta) "10.0.0.2")
      (is (gethash "dc" beta) "eqdc10")))
  (let ((clients (gethash "clients" x)))
    (is (hash-table-count clients) 2)
    (is (gethash "data" clients)
        #(#("gamma" "delta") #(1 2))
        :test #'equalp)
    (is (gethash "hosts" clients)
        #("alpha" "omega")
        :test #'equalp)))

(defparameter *table-text-1* "[table]
key = \"value\"
bare_key = \"value\"
bare-key = \"value\"

\"127.0.0.1\" = \"value\"
\"character encoding\" = \"value\"
\"ʎǝʞ\" = \"value\"
")

(defun parse-test-table-1 (toplevel)
  (let ((table (gethash "table" toplevel)))
    (is (gethash "key" table) "value" :test #'equal)
    (is (gethash "bare_key" table) "value" :test #'equal)
    (is (gethash "127.0.0.1" table) "value" :test #'equal)
    (is (gethash "character encoding" table) "value" :test #'equal)
    (is (gethash "ʎǝʞ" table) "value" :test #'equal)))

(defparameter *table-text-2* "[dog.\"tater.man\"]
type = \"pug\"
")

(defun parse-test-table-2 (table)
  (is "pug" (gethash "type" (gethash "tater.man" (gethash "dog" table))) :test #'equal))

(defparameter *table-text-3* "[a.b]
c = 1

[a]
d = 2
")

(defun parse-test-table-3 (table)
  (is 1 (gethash "c" (gethash "b" (gethash "a" table))))
  (is 2 (gethash "d" (gethash "a" table))))

(defparameter *inline-table-text* "name = { first = \"Tom\", last = \"Preston-Werner\" }
point = { x = 1, y = 2 }
")

(defun parse-test-inline-table (table)
  (is "Tom" (gethash "first" (gethash "name" table)) :test #'equal)
  (is "Preston-Werner" (gethash "last" (gethash "name" table)) :test #'equal)
  (is 1 (gethash "x" (gethash "point" table)))
  (is 2 (gethash "y" (gethash "point" table))))

(defparameter *array-table-text-1* "[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]

[[products]]
name = \"Nail\"
sku = 284758393
color = \"gray\"
")

(defun parse-test-array-table-1 (table)
  (let ((products (gethash "products" table)))
    (is-type products 'vector)
    (is (length products) 3)
    (is (gethash "name" (aref products 0)) "Hammer" :test #'equal)
    (is (gethash "sku" (aref products 0)) 738594937)
    (is 0 (hash-table-count (aref products 1)))
    (is (gethash "name" (aref products 2)) "Nail" :test #'equal)
    (is (gethash "sku" (aref products 2)) 284758393)
    (is (gethash "color" (aref products 2)) "gray" :test #'equal)))

(defparameter *array-table-text-2* "
[[fruit]]
name = \"apple\"

[fruit.physical]
color = \"red\"
shape = \"round\"

[[fruit.variety]]
name = \"red delicious\"

[[fruit.variety]]
name = \"granny smith\"

[[fruit]]
name = \"banana\"

[[fruit.variety]]
name = \"plantain\"
")

(defun parse-test-array-table-2 (table)
  (let ((fruit (gethash "fruit" table)))
    (is-type fruit 'vector)
    (is 2 (length fruit))
    (let ((fruit-1 (aref fruit 0)))
      (is "apple" (gethash "name" fruit-1) :test #'equal)
      (let ((physical (gethash "physical" fruit-1)))
        (is (gethash "color" physical) "red" :test #'equal)
        (is (gethash "shape" physical) "round" :test #'equal))
      (let ((variety (gethash "variety" fruit-1)))
        (is-type variety 'vector)
        (is 2 (length variety))
        (is (gethash "name" (aref variety 0)) "red delicious" :test #'equal)
        (is (gethash "name" (aref variety 1)) "granny smith" :test #'equal)))
    (let ((fruit-2 (aref fruit 1)))
      (is "banana" (gethash "name" fruit-2) :test #'equal)
      (let ((variety (gethash "variety" fruit-2)))
        (is-type variety 'vector)
        (is 1 (length variety))
        (is (gethash "name" (aref variety 0)) "plantain" :test #'equal)))))

(defparameter *parse-test-table* '())

(defmacro def-parse-test (name text function)
  `(push (list ,name ,text ,function)
         *parse-test-table*))

(def-parse-test "example" *example-text* #'parse-test-example)
(def-parse-test "table-1" *table-text-1* #'parse-test-table-1)
(def-parse-test "table-2" *table-text-2* #'parse-test-table-2)
(def-parse-test "table-3" *table-text-3* #'parse-test-table-3)
(def-parse-test "inline-table" *inline-table-text* #'parse-test-inline-table)
(def-parse-test "array-table-1" *array-table-text-1* #'parse-test-array-table-1)
(def-parse-test "array-table-2" *array-table-text-2* #'parse-test-array-table-2)

(defun parse-test (name)
  (destructuring-bind (text function) (cdr (assoc name *parse-test-table* :test #'string=))
    (funcall function (parse text))))

(defun encode-test (name)
  (destructuring-bind (text function) (cdr (assoc name *parse-test-table* :test #'string=))
    (funcall function (parse (with-output-to-string (out) (encode (parse text) out))))))

(subtest "parse"
  (subtest "example"
    (parse-test "example"))

  (subtest "string"
    (is (coerce (esrap:parse 'cl-toml::value "\"\\b\\t\\n\\fabc\\r\\n\\\"\\/\\\\\\u1234\"") 'list)
        '(#\Backspace #\Tab #\Newline #\Page #\a #\b #\c #\Newline #\" #\/ #\\ #\ሴ)
        :test #'equal)

    (is (esrap:parse 'cl-toml::value "\"\"\"
Roses are red
Violets are blue\"\"\"")
        "Roses are red
Violets are blue"
        :test #'equal)

    (is (esrap:parse 'cl-toml::value "\"The quick brown fox jumps over the lazy dog.\"")
        "The quick brown fox jumps over the lazy dog."
        :test #'string=)
    (is (esrap:parse 'cl-toml::value "\"\"\"
The quick brown \\


fox jumps over \\
the lazy dog.\"\"\"")
        "The quick brown fox jumps over the lazy dog."
        :test #'string=)
    (is (esrap:parse 'cl-toml::value "\"\"\"\\
The quick brown \\
fox jumps over \\
the lazy dog.\\
\"\"\"")
        "The quick brown fox jumps over the lazy dog."
        :test #'string=)

    (dolist (s '("C:\\Users\\nodejs\\templates"
                 "\\\\ServerX\\admin$\\system32\\"
                 "Tom \"Dubs\" Preston-Werner"
                 "<\\i\\c*\\s*>"))
      (is (esrap:parse 'cl-toml::value (format nil "'~A'" s)) s :test #'string=))

    (is (esrap:parse 'cl-toml::value "'''I [dw]on't need \\d{2} apples'''") "I [dw]on't need \\d{2} apples" :test #'string=)
    (is (esrap:parse 'cl-toml::value "'''
The first newline is
trimmed in raw strings.
All other whitespace
is preserved.
'''")
        "The first newline is
trimmed in raw strings.
All other whitespace
is preserved.
"
        :test #'string=))

  (subtest "integer"
    (loop :for (s v) :in '(("+99" 99) ("42" 42) ("0" 0) ("-17" -17)
                           ("1_000" 1000) ("5_349_221" 5349221) ("1_2_3_4_5" 12345))
          :do (is (esrap:parse 'cl-toml::value s) v)))

  (subtest "float"
    (loop :for s :in '("+1.0" "3.1415" "-0.01" "5e+22" "1e6" "-2E-2" "6.626e-34"
                       "9_224_617.445_991_228_313" "1e1_0")
          :do (ok (esrap:parse 'cl-toml::value s))))

  (subtest "boolean"
    (is (esrap:parse 'cl-toml::value "true") 'true)
    (is (esrap:parse 'cl-toml::value "false") 'false))

  (subtest "datetime"
    (dolist (s '("1979-05-27T07:32:00Z"
                 "1979-05-27T00:32:00-07:00"
                 "1979-05-27T00:32:00.999999-07:00"))
      (is (esrap:parse 'cl-toml::value s)
          (local-time:parse-rfc3339-timestring s)
          :test #'local-time:timestamp=)))

  (subtest "array"
    (is (esrap:parse 'cl-toml::value "[ 1, 2, 3 ]") #(1 2 3) :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[ \"red\", \"yellow\", \"green\" ]") #("red" "yellow" "green") :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[ [ 1, 2 ], [3, 4, 5] ]") #(#(1 2) #(3 4 5)) :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''']")
        #("all" "strings" "are the same" "type") :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]")
        #(#(1 2) #("a" "b" "c")) :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[
1, 2, 3
]")
        #(1 2 3) :test #'equalp)
    (is (esrap:parse 'cl-toml::value "[
1,
2, # OK
]")
        #(1 2)
        :test #'equalp))

  (subtest "table"
    (parse-test "table-1")
    (parse-test "table-2")
    (parse-test "table-3"))

  (subtest "inline-table"
    (parse-test "inline-table"))

  (subtest "array-table"
    (parse-test "array-table-1")
    (parse-test "array-table-2")))

(subtest "encode"
  (subtest "example" (encode-test "example"))
  (subtest "table-1" (encode-test "table-1"))
  (subtest "table-2" (encode-test "table-2"))
  (subtest "table-3" (encode-test "table-3"))
  (subtest "inline-table" (encode-test "inline-table"))
  (subtest "array-table-1" (encode-test "array-table-1"))
  (subtest "array-table-2" (encode-test "array-table-2"))

  (is (cl-toml:parse (with-output-to-string (out)
                       (cl-toml:encode '(("foo" ("bar" . ""))) out))
                     :table-as :alist)
      '(("foo" ("bar" . "")))
      :test #'equal))

(finalize)
