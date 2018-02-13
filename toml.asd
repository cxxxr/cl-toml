(defsystem "toml"
  :depends-on ("alexandria"
               "trivial-types"
               "esrap"
               "local-time")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")
               (:file "encode")))

