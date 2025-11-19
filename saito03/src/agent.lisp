(in-package :saito)

(defun run-weather-agent (user-input)
  "Envoie USER-INPUT à llama3.2 via Ollama avec les tools enregistrés."
  (let ((messages (list `(("role" . "system")
                          ("content" . ,*agent-system-prompt*))
                        `(("role" . "user")
                          ("content" . ,user-input)))))
    (dbg "=== DÉBUT AGENT ===")
    (loop
      (dbg "--- Appel Ollama ---")
      (dbg "Messages = ~A" messages)

      (let* ((response   (call-ollama messages))
             (message    (gethash "message" response))
             (content    (and message (gethash "content" message)))
             (tool-calls (and message (gethash "tool_calls" message))))
        (dbg "message = ~A" message)
        (dbg "content = ~A" content)
        (dbg "tool_calls (brut) = ~A" tool-calls)

        ;; Ajout du message assistant dans l’historique
        (when message
          (push `(("role" . "assistant")
                  ("content" . ,(or content "")))
                messages)
          (setf messages (nreverse messages)))

        (let ((calls (ensure-list tool-calls)))
          (dbg "tool_calls liste = ~A" calls)

          (if (null calls)
              (progn
                ;; 1) Pas de tool_calls natifs -> tenter le JSON manuel
                (when content
                  (let ((manual (try-manual-tool-json content)))
                    (when manual
                      (dbg "Réponse tool (mode JSON manuel) = ~A" manual)
                      (return manual))))
                ;; 2) Sinon, on renvoie la réponse brute du modèle
                (dbg "Pas de tool_call -> réponse finale brute.")
                (return content))
              (progn
                (dbg "Exécution de ~D tool_call(s)..." (length calls))
                (dolist (tc calls)
                  (let ((tool-msg (handle-tool-call tc)))
                    (dbg "tool_msg = ~A" tool-msg)
                    (setf messages (append messages (list tool-msg))))))))))))
