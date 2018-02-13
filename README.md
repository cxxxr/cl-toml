# cl-toml
toml 0.4.0 parser and encoder

## Usage
```common-lisp
(defparameter *example-text*
  "# This is a TOML document. Boom.
title = \"TOML Example\"
[owner]
name = \"Lance Uppercut\"
dob = 1979-05-27T07:32:00-08:00 # First class dates? Why not?
")

(toml:parse *example-text*)
=> #<EQUAL Hash Table{5} 40205D45A3>


(toml:parse *example-text* :table-as :alist)
=> (("owner" ("dob" . @1979-05-27T15:32:00.000000Z) ("name" . "Lance Uppercut"))
    ("title" . "TOML Example"))

(toml:encode '(("owner" ("dob" . @1979-05-27T15:32:00.000000Z) ("name" . "Lance Uppercut"))
               ("title" . "TOML Example")))

"title" = "TOML Example"
[owner]
"dob" = 1979-05-27T15:32:00.000000Z
"name" = "Lance Uppercut"
```

## License
MIT
