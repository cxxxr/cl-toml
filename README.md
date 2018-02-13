# cl-toml
toml 0.4 parser and encoder

## Usage
```common-lisp
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
(toml:parse *example-text*)
=> #<EQUAL Hash Table{5} 40205D45A3>

(toml:parse *example-text* :table-as :alist)
=> (("clients" ("hosts" . #("alpha" "omega")) ("data" . #(#("gamma" "delta") #(1 2))))
    ("servers"
     ("beta" ("dc" . "eqdc10") ("ip" . "10.0.0.2"))
     ("alpha" ("dc" . "eqdc10") ("ip" . "10.0.0.1")))
    ("database"
     ("enabled" . TOML:TRUE)
     ("connection_max" . 5000)
     ("ports" . #(8001 8001 8002))
     ("server" . "192.168.1.1"))
    ("owner" ("dob" . @1979-05-27T15:32:00.000000Z) ("name" . "Lance Uppercut"))
    ("title" . "TOML Example"))

(toml:encode '(("clients" ("hosts" . #("alpha" "omega")) ("data" . #(#("gamma" "delta") #(1 2))))
               ("servers"
                ("beta" ("dc" . "eqdc10") ("ip" . "10.0.0.2"))
                ("alpha" ("dc" . "eqdc10") ("ip" . "10.0.0.1")))
               ("database"
                ("enabled" . TOML:TRUE)
                ("connection_max" . 5000)
                ("ports" . #(8001 8001 8002))
                ("server" . "192.168.1.1"))
               ("owner" ("dob" . @1979-05-27T15:32:00.000000Z) ("name" . "Lance Uppercut"))
               ("title" . "TOML Example")))

"title" = "TOML Example"
[clients]
"hosts" = ["alpha", "omega"]
"data" = [["gamma", "delta"], [1, 2]]
[servers]
[servers.beta]
"dc" = "eqdc10"
"ip" = "10.0.0.2"
[servers.alpha]
"dc" = "eqdc10"
"ip" = "10.0.0.1"
[database]
"enabled" = true
"connection_max" = 5000
"ports" = [8001, 8001, 8002]
"server" = "192.168.1.1"
[owner]
"dob" = 1979-05-27T15:32:00.000000Z
"name" = "Lance Uppercut"

## License
MIT

```
