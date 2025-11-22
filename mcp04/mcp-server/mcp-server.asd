(in-package :asdf-user)

(defsystem "mcp-server"
  :version "0.0.1"
  :author "spike"
  :license "MIT"
  :depends-on (:jonathan
               :drakma
               :hunchentoot
               :easy-routes)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logging")
                 (:file "http-utils")
                 (:file "tools-registry")
                 (:file "mcp-protocol")
                 (:file "http-server")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mcp-server/tests"))))

(defsystem "mcp-server/tests"
  :author ""
  :license ""
  :depends-on ("mcp-server"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for server"
  :perform (test-op (op c) (symbol-call :rove :run c)))
