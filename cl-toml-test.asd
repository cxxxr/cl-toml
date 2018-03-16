(defsystem "cl-toml-test"
  :depends-on ("cl-toml" "prove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :prove '#:run :cl-toml)))
