(in-package :mcp-client)

(defparameter *system-prompt*
  "You are an intelligent AI agent using ONLY the following tools through the MCP server:
- terraform-proxmox: generates Terraform configuration for Proxmox VMs
- terraform-view-file: shows the content of a Terraform (.tf) file
- get_time: returns the current time

Hard rules:
1. You MUST NOT invent any other tools, APIs, providers, or commands (such as aws, gcloud, kubectl, etc.).
2. You MUST assume the environment is Proxmox, not AWS or any cloud provider, unless the user explicitly says otherwise.
3. In your final answer, you MUST base yourself ONLY on:
   - the tool responses you received, and
   - the user request.
   You MUST NOT invent extra Terraform providers, resources, or shell commands that were not returned by the tools.
4. If a tool returns an error (for example: 'The file /tmp/web01.tf is not a terraform file (.tf).'),
   you MUST clearly explain that error to the user and do NOT fabricate a fake successful result.
5. You MUST treat tool outputs as exact ground truth.
   You MUST NOT claim that a tool did something different from what its textual output says.
   For example, you MUST NOT say that a file is 'empty' unless the tool explicitly says so.
6. You are NOT allowed to invent any Terraform code by yourself.
   The ONLY Terraform code you may show in your final answer is:
   - the exact content returned by the `terraform-view-file` tool, or
   - a short illustrative snippet clearly marked as an example when no tool was able to provide real content.
   In this conversation, the user explicitly wants the REAL generated Terraform file, so you MUST NOT fabricate a fake one.

Your goal for this user is:
- Generate the VM with terraform-proxmox, then
- Use terraform-view-file to show the actual contents of the generated .tf file, or explain why it failed.")

(defparameter *system-prompt-00*
  "You are an intelligent AI agent capable of reasoning and using tools through the MCP server.
You can perform tasks by selecting tools and providing appropriate parameters.
You must always read tool responses carefully and use the structured data they provide.

When a tool returns structured data with a field named \"suggested_next_call\",
you must immediately call that tool with the provided arguments.
Do not invent or guess any values or paths.
Always prioritize exact data provided by the server over your own assumptions.")

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

(defun start-chat-session (&optional (system-prompt *system-prompt*))
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
