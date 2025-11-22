(in-package :asdf-user)

(defsystem "mcp-client"
  :version "0.0.1"
  :author "spike spiegel"
  :license ""
  :depends-on (:drakma
               :jonathan
               :babel)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logging")
                 (:file "http-utils")
                 (:file "mcp-http-client")
                 (:file "ollama-client")
                 (:file "agent")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mcp-client/tests"))))

(defsystem "mcp-client/tests"
  :author ""
  :license ""
  :depends-on ("mcp-client"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for client"
  :perform (test-op (op c) (symbol-call :rove :run c)))
