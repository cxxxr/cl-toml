(defsystem "toml-test"
  :depends-on ("toml" "prove")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :prove '#:run :toml)))
