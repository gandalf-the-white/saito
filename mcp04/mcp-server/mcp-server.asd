(in-package :asdf-user)

(defsystem "mcp-server"
  :version "0.0.1"
  :author "spike"
  :license "MIT"
  :depends-on (:jonathan
               :drakma
               :hunchentoot
               :easy-routes
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logging")
                 (:file "http-utils")
                 (:file "tools-registry")
                 (:file "tool-time")
                 (:file "mcp-protocol")
                 (:file "http-server")
                 (:file "main"))))
  :description "")

