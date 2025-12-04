(in-package :mcp-client)

(defparameter *agent-system-prompt*
  "You are a tool-using agent.

You have access to remote tools exposed by a Model Context Protocol (MCP) server.
Your available tools are provided in the 'tools' section of the chat API.

IMPORTANT RULES:
- Always answer in the same language as the user.
- When a question requires external information (weather, time, etc.), you MUST call a tool.
- To call a tool, use the provided tool_calls mechanism.
- Do not invent tool results. Always wait for the tool response if you call one.
- If no tool is appropriate, answer normally in natural language.
- When you have enough information, provide a clear and concise final answer.")

(defun mcp-print-tools ()
  "Affiche les tools MCP disponibles et renvoie la liste de leurs noms."
  (let ((names (mcp-tools-list-text nil)))  ; paramètre ignoré dans mcp-tools-list-text
    (format t "Outils MCP disponibles :~%")
    (dolist (n names)
      (format t " - ~A~%" n))
    (terpri)
    names))

;; (defun append-message (messages role content &key tool-name)
;;   "Ajoute un message (role, content, tool-name éventuel) à la fin de MESSAGES
;; et renvoie la nouvelle liste."
;;   (append messages
;;           (list (remove nil
;;                         `(("role" . ,role)
;;                           ("content" . ,content)
;;                           ,@(when tool-name
;;                               (list (cons "tool_name" tool-name))))))))

;; (defun run-mcp-ollama-session (user-input)
;;   "Session simple:
;; 1) récupère les tools via MCP (HTTP)
;; 2) les passe à Ollama
;; 3) boucle tool_calls -> MCP tools/call -> réponse finale."
;;   (let* ((mcp-tools    (mcp-tools-list))
;;          (ollama-tools (mcp-tools->ollama-tools mcp-tools))
;;          (messages
;;            (list `(("role" . "system")
;;                    ("content" . ,*agent-system-prompt*))
;;                  `(("role" . "user")
;;                    ("content" . ,user-input)))))
;;     (dbg "Available MCP tools: ~A"
;;          (mapcar (lambda (tool) (gethash "name" tool)) mcp-tools))
;;     (loop
;;       (dbg "--- Appel Ollama ---")
;;       (dbg "Messages = ~A" messages)

;;       (let* ((response   (call-ollama-with-tools messages ollama-tools))
;;              (message    (gethash "message" response))
;;              (content    (and message (gethash "content" message)))
;;              (tool-calls (and message (gethash "tool_calls" message))))

;;         (dbg "message = ~A" message)
;;         (dbg "content = ~A" content)
;;         (dbg "tool_calls (brut) = ~A" tool-calls)

;;         ;; ajout du message assistant dans l'historique
;;         (when message
;;           (push `(("role" . "assistant")
;;                   ("content" . ,(or content "")))
;;                 messages)
;;           (setf messages (nreverse messages)))

;;         (let ((calls (ensure-list tool-calls)))
;;           (dbg "tool_calls liste = ~A" calls)
;;           (if (null calls)
;;               (progn
;;                 (dbg "Pas de tool_call -> réponse finale.")
;;                 (return content))
;;               (progn
;;                 (dbg "Exécution de ~D tool_call(s)..." (length calls))
;;                 (dolist (tc calls)
;;                   (let* ((fn-obj    (gethash "function" tc))
;;                          (fn-name   (and fn-obj (gethash "name" fn-obj)))
;;                          (raw-args  (and fn-obj (gethash "arguments" fn-obj)))
;;                          (args-ht   (parse-arguments-maybe raw-args)))
;;                     (dbg "Tool_call: name=~A args=~A" fn-name args-ht)
;;                     (when fn-name
;;                       ;; conversion hash-table -> alist pour MCP
;;                       (let ((alist-args nil))
;;                         (maphash (lambda (k v)
;;                                    (push (cons k v) alist-args))
;;                                  args-ht)
;;                         (setf alist-args (nreverse alist-args))
;;                         (let ((tool-result (mcp-call-tool-text fn-name alist-args)))
;;                           (dbg "Résultat tool ~A = ~A" fn-name tool-result)
;;                           (when tool-result
;;                             (push `(("role" . "tool")
;;                                     ("tool_name" . ,fn-name)
;;                                     ("content" . ,tool-result))
;;                                   messages)
;;                             (setf messages (nreverse messages)))))))))))))))

;; (defun run-mcp-ollama-session (user-input)
;;   "Session simple:
;; 1) récupère les tools via MCP (HTTP)
;; 2) les passe à Ollama
;; 3) boucle tool_calls -> MCP tools/call -> réponse finale."
;;   (let* ((mcp-tools    (mcp-tools-list))
;;          (ollama-tools (mcp-tools->ollama-tools mcp-tools))
;;          (messages
;;            (list `(("role" . "system")
;;                    ("content" . ,*agent-system-prompt*))
;;                  `(("role" . "user")
;;                    ("content" . ,user-input)))))
;;     (dbg "Available MCP tools: ~A"
;;          (mapcar (lambda (tool) (gethash "name" tool)) mcp-tools))
;;     (loop
;;       (dbg "--- Appel Ollama ---")
;;       (dbg "Messages = ~A" messages)

;;       (let* ((response   (call-ollama-with-tools messages ollama-tools))
;;              (message    (gethash "message" response))
;;              (content    (and message (gethash "content" message)))
;;              (tool-calls (and message (gethash "tool_calls" message))))

;;         (dbg "message = ~A" message)
;;         (dbg "content = ~A" content)
;;         (dbg "tool_calls (brut) = ~A" tool-calls)

;;         ;; ajout du message assistant dans l'historique
;;         (when message
;;           (setf messages
;;                 (append-message messages "assistant" (or content ""))))

;;         (let ((calls (ensure-list tool-calls)))
;;           (dbg "tool_calls liste = ~A" calls)
;;           (if (null calls)
;;               (progn
;;                 (dbg "Pas de tool_call -> réponse finale.")
;;                 (return content))
;;               (progn
;;                 (dbg "Exécution de ~D tool_call(s)..." (length calls))
;;                 (dolist (tc calls)
;;                   (let* ((fn-obj    (gethash "function" tc))
;;                          (fn-name   (and fn-obj (gethash "name" fn-obj)))
;;                          (raw-args  (and fn-obj (gethash "arguments" fn-obj)))
;;                          (args-ht   (parse-arguments-maybe raw-args)))
;;                     (dbg "Tool_call: name=~A args=~A" fn-name args-ht)
;;                     (when fn-name
;;                       (let ((alist-args nil))
;;                         (maphash (lambda (k v)
;;                                    (push (cons k v) alist-args))
;;                                  args-ht)
;;                         (setf alist-args (nreverse alist-args))
;;                         (let ((tool-result (mcp-call-tool-text fn-name alist-args)))
;;                           (dbg "Résultat tool ~A = ~A" fn-name tool-result)
;;                           (when tool-result
;;                             (setf messages
;;                                   (append-message messages "tool" tool-result
;;                                                   :tool-name fn-name)))))))))))))))


(defun chat-session-ask (session user-input)
  "Envoie USER-INPUT dans une CHAT-SESSION existante, avec contexte conservé.
Retourne la réponse finale (string), met à jour la session in-place."
  (let* ((messages (chat-session-messages session)))
    ;; On ajoute le message user
    (setf messages
          (append messages
                  (list `(("role" . "user")
                          ("content" . ,user-input)))))
    (multiple-value-bind (answer new-messages)
        (run-mcp-ollama-turn messages (chat-session-tools-schemas session))
      (setf (chat-session-messages session) new-messages)
      answer)))

(defun run-mcp-ollama-session (user-input)
  "Appel one-shot : crée une nouvelle session, envoie une question, renvoie la réponse."
  (let* ((session (start-chat-session))
         (answer  (chat-session-ask session user-input)))
    answer))

(defun run-mcp-ollama-turn (messages tools-schemas)
  "Effectue un tour de boucle (user + éventuels tool_calls) avec Ollama.
Prend une liste MESSAGES et TOOLS-SCHEMAS, renvoie deux valeurs :
- la réponse finale (string)
- les messages mis à jour (avec assistant + tools)."
  (loop
    (dbg "--- Appel Ollama ---")
    (dbg "Messages = ~A" messages)

    (let* ((response   (call-ollama-with-tools messages tools-schemas))
           (message    (gethash "message" response))
           (content    (and message (gethash "content" message)))
           (tool-calls (and message (gethash "tool_calls" message))))

      (dbg "message = ~A" message)
      (dbg "content = ~A" content)
      (dbg "tool_calls (brut) = ~A" tool-calls)

      ;; Ajoute le message assistant à l'historique
      (when message
        (setf messages
              (append messages
                      (list `(("role" . "assistant")
                              ("content" . ,(or content "")))))))

      (let ((calls (ensure-list tool-calls)))
        (dbg "tool_calls liste = ~A" calls)
        (if (null calls)
            (progn
              (dbg "Pas de tool_call -> réponse finale.")
              (return (values content messages)))
            (progn
              (dbg "Exécution de ~D tool_call(s)..." (length calls))
              (dolist (tc calls)
                (let* ((fn-obj    (gethash "function" tc))
                       (fn-name   (and fn-obj (gethash "name" fn-obj)))
                       (raw-args  (and fn-obj (gethash "arguments" fn-obj)))
                       (args-ht   (parse-arguments-maybe raw-args)))
                  (dbg "Tool_call: name=~A args=~A" fn-name args-ht)
                  (when fn-name
                    ;; conversion hash-table -> alist pour MCP
                    (let ((alist-args nil))
                      (maphash (lambda (k v)
                                 (push (cons k v) alist-args))
                               args-ht)
                      (setf alist-args (nreverse alist-args))
                      (let ((tool-result (mcp-call-tool-text fn-name alist-args)))
                        (dbg "Résultat tool ~A = ~A" fn-name tool-result)
                        (when tool-result
                          (setf messages
                                (append messages
                                        (list `(("role" . "tool")
                                                ("tool_name" . ,fn-name)
                                                ("content" . ,tool-result)))))))))))))))))

(defstruct chat-session
  messages        ;; liste de messages pour Ollama
  tools-schemas)  ;; schémas des tools pour /api/chat

(defun start-chat-session (&optional (system-prompt *agent-system-prompt*))
  "Crée une nouvelle session de chat avec system prompt et tools MCP chargés."
  (let* ((mcp-tools    (mcp-tools-list))
         (ollama-tools (mcp-tools->ollama-tools mcp-tools))
         (messages
           (list `(("role" . "system")
                   ("content" . ,system-prompt)))))
    (dbg "New chat session with tools: ~A"
         (mapcar (lambda (tool) (gethash "name" tool)) mcp-tools))
    (make-chat-session
     :messages messages
     :tools-schemas ollama-tools)))
