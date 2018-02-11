(defsystem "toml"
  :depends-on ("alexandria"
               "esrap"
               "local-time")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse")))

(defsystem "toml-test"
  :depends-on ("toml" "prove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :prove '#:run :toml-test)))
