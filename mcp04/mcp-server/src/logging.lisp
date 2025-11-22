(in-package :mcp-server)

(defparameter *debug* nil)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[MCP]~% ~?", fmt (list ,@args))
     (finish-output *error-output*)))
