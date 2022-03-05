(in-package :asdf-user)

(defsystem "AoC2021"
  :class :package-inferred-system
  :depends-on ("AoC2021/src/app")
  :description ""
  :in-order-to ((test-op (load-op "AoC2021/test/all")))
  :perform (test-op (o c) (symbol-call :test/all :test-suite)))

(defsystem "AoC2021/test"
  :depends-on ("AoC2021/test/all"))

(register-system-packages "AoC2021/src/app" '(:app))
(register-system-packages "AoC2021/test/all" '(:test/all))
