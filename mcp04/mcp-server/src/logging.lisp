(in-package :mcp-server)

(defparameter *debug* nil)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[MCP ~A] ~?"
             (local-time:format-timestring nil (local-time:now))
             ,fmt (list ,@args))
     (terpri *error-output*)
     (finish-output *error-output*)))
