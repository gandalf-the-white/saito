(defpackage client/tests/main
  (:use :cl
        :client
        :rove))
(in-package :client/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :client)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
