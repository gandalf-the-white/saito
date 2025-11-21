(in-package :saito)

(defparameter *debug* t)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[MCP]~% ~?" ,fmt (list ,@args))
     (finish-output *error-output*)))
