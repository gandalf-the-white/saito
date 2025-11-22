(in-package :mcp-client)

(defparameter *debug* t)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[CLIENT]~% ~?" ,fmt (list ,@args))
     (finish-output *error-output*)))
