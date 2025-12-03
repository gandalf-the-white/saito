(in-package :asdf-user)

(defsystem "saito"
  :version "0.0.1"
  :author "spike spiegel"
  :license "MIT"
  :depends-on (:drakma
               :jonathan
               :babel)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "")
