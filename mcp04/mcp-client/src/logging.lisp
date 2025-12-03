(in-package :mcp-client)

(defparameter *debug* t)

;; (defmacro dbg (fmt &rest args)
;;   `(when *debug*
;;      (format *error-output* "[CLIENT]~% ~?" ,fmt (list ,@args))
;;      (finish-output *error-output*)))

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[CLIENT ~A] ~?"
             (local-time:format-timestring nil (local-time:now))
             ,fmt (list ,@args))
     (terpri *error-output*)
     (finish-output *error-output*)))
