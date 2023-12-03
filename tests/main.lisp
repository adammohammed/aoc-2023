(defpackage aoc23/tests/main
  (:use :cl
        :aoc23
        :rove))
(in-package :aoc23/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :aoc23)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
