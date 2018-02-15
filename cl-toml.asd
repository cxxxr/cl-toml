(defsystem "cl-toml"
  :license "MIT"
  :author "cxxxr"
  :description "TOML v0.4.0 parser and encoder"
  :depends-on ("alexandria"
               "trivial-types"
               "esrap"
               "local-time")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")
               (:file "encode"))
  :in-order-to ((test-op (test-op "toml-test"))))