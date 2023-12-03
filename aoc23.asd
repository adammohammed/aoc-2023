(defsystem "aoc23"
  :version "0.1.0"
  :author "Adam Mohammed"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "aoc23/tests"))))

(defsystem "aoc23/tests"
  :author "Adam Mohammed"
  :license ""
  :depends-on ("aoc23"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for aoc23"
  :perform (test-op (op c) (symbol-call :rove :run c)))
