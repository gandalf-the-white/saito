(in-package :asdf-user)

(defsystem "saito-client"
  :version "0.0.1"
  :author "spike"
  :license "MIT"
  :depends-on (:drakma
               :jonathan
               :babel)
  :components ((:module "src"
                :components
                ((:file "mcp-ollama-client"))))
  :description ""
  :in-order-to ((test-op (test-op "saito-client/tests"))))

(defsystem "saito-client/tests"
  :author ""
  :license ""
  :depends-on ("saito-client"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "mcp-ollama-client"))))
  :description "Test system for saito"
  :perform (test-op (op c) (symbol-call :rove :run c)))
