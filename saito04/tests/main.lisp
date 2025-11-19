(defpackage agent01/tests/main
  (:use :cl
        :agent01
        :rove))
(in-package :agent01/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :agent01)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
