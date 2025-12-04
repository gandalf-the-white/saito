(in-package :mcp-client)

(defparameter *current-session* nil)

(defun ensure-current-session ()
  "Retourne la session courante, en la créant si nécessaire."
  (or *current-session*
      (setf *current-session* (start-chat-session))))

(defun run-mcp-ollama-repl (&key (prompt "mcp> "))
  "Boucle interactive pour discuter avec l'agent Ollama+MCP (multi-turn).

Commandes spéciales :
  :tools             -> afficher la liste des tools MCP
  :reset             -> réinitialiser la session (oublier le contexte)
  :q, :quit, :exit   -> quitter la boucle."
  (format t "MCP/Ollama interactive session (multi-turn).~%")
  (format t "Commands: :tools, :reset, :q, :quit, :exit~%~%")
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
            ;; Afficher les tools MCP
            ((string= trimmed ":tools")
             (handler-case
                 (mcp-print-tools)
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))
            ;; Réinitialiser la session (on oublie le contexte)
            ((string= trimmed ":reset")
             (setf *current-session* nil)
             (format t "~&[INFO] Session reset. New context will be created on next question.~%~%"))
            ;; Sinon : question normale à l'agent (multi-turn)
            (t
             (handler-case
                 (let* ((session (ensure-current-session))
                        (answer  (chat-session-ask session trimmed)))
                   (format t "~&[ANSWER] ~A~%~%" answer))
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))))))))

(defun main ()
  "Point d'entrée pratique si tu veux lancer le client depuis la ligne de commande."
  (run-mcp-ollama-repl))
