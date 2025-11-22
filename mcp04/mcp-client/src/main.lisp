(in-package :mcp-client)

;; (defun run-mcp-ollama-repl (&key (prompt "mcp> "))
;;   "Boucle interactive simple pour discuter avec l'agent Ollama+MCP.
;; Commandes spéciales :
;;   :q, :quit, :exit  -> quitter la boucle."
;;   (format t "MCP/Ollama interactive session.~%")
;;   (format t "Type :q, :quit or :exit to leave.~%~%")
;;   (loop
;;     (format t "~A" prompt)
;;     (finish-output)
;;     (let ((line (read-line *standard-input* nil nil)))
;;       (when (or (null line)
;;                 (string= line ":q")
;;                 (string= line ":quit")
;;                 (string= line ":exit"))
;;         (format t "~%Bye.~%")
;;         (return))
;;       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
;;         (when (> (length trimmed) 0)
;;           (handler-case
;;               (let ((answer (run-mcp-ollama-session trimmed)))
;;                 (format t "~&[ANSWER] ~A~%~%" answer))
;;             (error (e)
;;               (format t "~&[ERROR] ~A~%~%" e))))))))

(defun run-mcp-ollama-repl (&key (prompt "mcp> "))
  "Boucle interactive simple pour discuter avec l'agent Ollama+MCP.
Commandes spéciales :
  :q, :quit, :exit   -> quitter la boucle.
  :tools             -> afficher la liste des tools MCP."
  (format t "MCP/Ollama interactive session.~%")
  (format t "Commands: :tools, :q, :quit, :exit~%~%")
  (loop
    (format t "~A" prompt)
    (finish-output)
    (let ((line (read-line *standard-input* nil nil)))
      (when (or (null line)
                (string= line ":q")
                (string= line ":quit")
                (string= line ":exit"))
        (format t "~%Bye.~%")
        (return))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (when (> (length trimmed) 0)
          (cond
            ;; nouvelle commande :tools
            ((string= trimmed ":tools")
             (handler-case
                 (mcp-print-tools)
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))
            ;; sinon, on envoie la requête au LLM
            (t
             (handler-case
                 (let ((answer (run-mcp-ollama-session trimmed)))
                   (format t "~&[ANSWER] ~A~%~%" answer))
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))))))))

(defun main ()
  "Point d'entrée pratique si tu veux lancer le client depuis la ligne de commande."
  (run-mcp-ollama-repl))
