#!/usr/bin/env sbcl --script

;;; app.lisp
(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

(ql:quickload '(:drakma :jonathan :babel))
(load "./src/main.lisp")
(saito::run-mcp-server)
