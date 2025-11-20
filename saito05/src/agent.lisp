(in-package :saito)


(defun run-weather-agent (user-input)
  "Main agent entrypoint: send USER-INPUT to Ollama with tools enabled."
  (let ((messages (list `(("role" . "system")
                          ("content" . ,*agent-system-prompt*))
                        `(("role" . "user")
                          ("content" . ,user-input)))))
    (dbg "=== START AGENT ===")
    (loop
      (dbg "--- Ollama call ---")
      (dbg "Messages = ~A" messages)

      (let* ((response   (call-ollama messages))
             (message    (gethash "message" response))
             (content    (and message (gethash "content" message)))
             (tool-calls (and message (gethash "tool_calls" message))))
        (dbg "message = ~A" message)
        (dbg "content = ~A" content)
        (dbg "tool_calls (raw) = ~A" tool-calls)

        ;; Add assistant message to conversation history
        (when message
          (push `(("role" . "assistant")
                  ("content" . ,(or content "")))
                messages)
          (setf messages (nreverse messages)))

        (let ((calls (ensure-list tool-calls)))
          (dbg "tool_calls list = ~A" calls)

          (if (null calls)
              (progn
                ;; No native tool_calls: try manual JSON tool syntax first
                (when content
                  (let ((manual (try-manual-tool-json content)))
                    (when manual
                      (dbg "Tool result (manual JSON mode) = ~A" manual)
                      (return manual))))
                ;; Otherwise, just return the model's content
                (dbg "No tool calls, returning model answer.")
                (return content))
              (progn
                (dbg "Executing ~D native tool_call(s)..." (length calls))
                (dolist (tc calls)
                  (let ((tool-msg (handle-tool-call tc)))
                    (dbg "tool_msg = ~A" tool-msg)
                    (setf messages (append messages (list tool-msg))))))))))))
