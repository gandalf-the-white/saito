(in-package :mcp-server)

(defun main ()
  (handler-case
      (progn
        (defparameter *debug* nil)
        (start-mcp-http-server :port 8000) ;; our start-app, for example clack:clack-up
        ;; let the webserver run,
        ;; keep the server thread in the foreground:
        ;; sleep for ± a hundred billion years.
        (sleep 1000))

    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     () (progn
          (format *error-output* "Aborting.~&")
          (stop-mcp-http-server)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
