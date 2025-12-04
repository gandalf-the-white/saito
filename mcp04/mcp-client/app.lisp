#!/usr/bin/env sbcl --script

;;; app.lisp

;;; 1. Charger Quicklisp
(eval-when (:execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (unless (probe-file quicklisp-init)
      (error "Quicklisp not found. Please install Quicklisp first."))
    (load quicklisp-init)))

;;; 2. Charger les dépendances
(ql:quickload '(:drakma :jonathan :hunchentoot :easy-routes :local-time))
(push (uiop:getcwd) asdf:*central-registry*)

;;; 3. Charger ton système
(ql:quickload "mcp-client")

;;; 4. Démarrer le serveur
(let ((*package* (find-package :mcp-client)))
  (defparameter mcp-client::*debug* t)
  (mcp-client::main)
  (dbg "Client started~%"))
