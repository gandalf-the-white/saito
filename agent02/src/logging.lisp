(in-package :saito)

(defparameter *agent-debug* nil)

(defmacro dbg (fmt &rest args)
  `(when *agent-debug*
     (format *trace-output* " [AGENT]~% ~?" ,fmt (list ,@args))))
