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
(ql:quickload "mcp-server")

;;; 4. Démarrer le serveur
(let ((*package* (find-package :mcp-server)))
  (defparameter mcp-server::*debug* t)
  (mcp-server::start-mcp-http-server)
  (format t "Server started~%")
  (loop (sleep 1000)))

;;; 5. Empêcher la fin du script (pour que le serveur reste actif)
;; (sb-ext:exit :code 0 :abort t))  ; Ne pas quitter, sauf si erreur
