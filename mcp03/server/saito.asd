(in-package :asdf-user)

(defsystem "saito"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:drakma
               :jonathan
               :babel
               :hunchentoot)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logging")
                 (:file "http-utils")
                 (:file "tools-registry")
                 (:file "tool-time")
                 (:file "tool-weather")
                 (:file "tool-ollama-chat")
                 (:file "mcp-protocol")
                 (:file "http-server")
                 (:file "main"))))
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
