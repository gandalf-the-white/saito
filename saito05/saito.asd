(in-package :asdf-user)

(defsystem "saito"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:drakma
               :jonathan
               :babel)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config")
                 (:file "logging")
                 (:file "utils")
                 (:file "http-utils")
                 (:file "tools-core")
                 (:file "tool-weather")
                 (:file "tool-time")
                 (:file "ollama-client")
                 (:file "agent"))))
  :description ""
  :in-order-to ((test-op (test-op "saito/tests"))))

(defsystem "saito/tests"
  :author ""
  :license ""
  :depends-on ("saito"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for saito"
  :perform (test-op (op c) (symbol-call :rove :run c)))
