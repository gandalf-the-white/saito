(in-package :mcp-client)

(defparameter *current-session* nil)

(defun ensure-current-session ()
  "Returns the current session, creating it if necessary."
  (or *current-session*
      (setf *current-session* (start-chat-session))))

(defun run-mcp-ollama-repl (&key (prompt "mcp> "))
  "Interactive loop for chatting with the Ollama+MCP agent (multi-turn).

Special orders :
  :tools             -> display the list of MCP tools
  :reset             -> reset the session (forget the context)
  :q, :quit, :exit   -> exit the loop."
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
            ;; Show tools MCP
            ((string= trimmed ":tools")
             (handler-case
                 (mcp-print-tools)
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))
            ;; Reset the session (we forget the context)

            ((string= trimmed ":reset")
             (setf *current-session* nil)
             (format t "~&[INFO] Session reset. New context will be created on next question.~%~%"))
            ;; Otherwise: normal question to the agent (multi-turn).
            (t
             (handler-case
                 (let* ((session (ensure-current-session))
                        (answer  (chat-session-ask session trimmed)))
                   (format t "~&[ANSWER] ~A~%~%" answer))
               (error (e)
                 (format t "~&[ERROR] ~A~%~%" e))))))))))

(defun main ()
  "Convenient entry point if you want to launch the client from the command line."
  (run-mcp-ollama-repl))
