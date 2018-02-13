(defsystem "toml"
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

(defsystem "toml-test"
  :depends-on ("toml" "prove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :prove '#:run :toml)))
