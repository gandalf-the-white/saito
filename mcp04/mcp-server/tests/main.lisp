(defpackage server/tests/main
  (:use :cl
        :server
        :rove))
(in-package :server/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :server)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
