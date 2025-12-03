(in-package :asdf-user)

(defsystem "mcp-client"
  :version "0.0.1"
  :author "spike spiegel"
  :license ""
  :depends-on (:drakma
               :jonathan
               :babel
               :local-time)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "logging")
                 (:file "http-utils")
                 (:file "mcp-http-client")
                 (:file "ollama-client")
                 (:file "agent")
                 (:file "main"))))
  :description "")
