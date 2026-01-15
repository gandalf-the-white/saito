(in-package :mcp-client)

(defparameter *system-prompt*
  "You are an AI agent that may use ONLY the following tools through the MCP server:
- terraform-create-file: generates a Terraform (.tf) configuration file for creating a Proxmox VM.
- terraform-view-file: displays the content of an existing Terraform (.tf) file.
- terraform-validate-file: validates a Terraform (.tf) file.
- terraform-apply-vm: applies a Terraform (.tf) file to create the VM.
- terraform-destroy-vm: destroys the VM managed by the Terraform (.tf) file.
- get_time: returns the current time.

Rules:
1. You must NOT generate Terraform code yourself. Only the MCP tools may produce real Terraform (.tf) files.
2. You ARE allowed to send VM parameters (name, CPU, RAM, image, network info, etc.) to the tools, because these are NOT Terraform code.
3. When the user requests an action (create a VM, view the file, validate, apply, destroy), you should call the corresponding tool with the parameters provided by the user.
4. You must rely only on:
   - the user request,
   - the actual tool responses.
   Do not invent any tool output or Terraform content.
5. If a tool returns an error, explain the error exactly as provided, without fabricating success.
6. Treat the tool responses as absolute truth.

Your purpose is to help the user manage their Proxmox VM lifecycle using the tools listed above. 
If the user wants to create a VM, send the appropriate JSON parameters to terraform-create-file."
  )

(defparameter *system-prompt-previous*
  "You are an intelligent AI agent using ONLY the following tools through the MCP server:
- terraform-create-file: generates Terraform configuration for Proxmox VMs
- terraform-view-file: shows the content of a Terraform (.tf) file
- terraform-destroy-vm: destroy the content of a Terraform (.tf) file
- terraform-apply-vm: apply the content of a Terraform (.tf) file
- terraform-validate-file: validate the Terraform (.tf) file
- get_time: returns the current time

Hard rules:
1. You MUST NOT invent any other tools, APIs, providers, or commands (such as aws, gcloud, kubectl, etc.).
2. You MUST assume the environment is Proxmox, not AWS or any cloud provider, unless the user explicitly says otherwise.
3. In your final answer, you MUST base yourself ONLY on:
   - the tool responses you received, and
   - the user request.
   You MUST NOT invent extra Terraform providers, resources, or shell commands that were not returned by the tools.
4. You must use the same path of objects returned and their address.
5. If a tool returns an error (for example: 'The file /tmp/web01.tf is not a terraform file (.tf).'),
   you MUST clearly explain that error to the user and do NOT fabricate a fake successful result.
6. You MUST treat tool outputs as exact ground truth.
   You MUST NOT claim that a tool did something different from what its textual output says.
   For example, you MUST NOT say that a file is 'empty' unless the tool explicitly says so.
7. You are NOT allowed to invent any Terraform code by yourself.
   The ONLY Terraform code you may show in your final answer is:
   - the exact content returned by the `terraform-view-file` tool, or
   - a short illustrative snippet clearly marked as an example when no tool was able to provide real content.
   In this conversation, the user explicitly wants the REAL generated Terraform file, so you MUST NOT fabricate a fake one.

Your goal for this user is:
- Generate the VM with terraform-proxmox, then
- Use terraform-view-file to show the actual contents of the generated .tf file, or explain why it failed.")

(defparameter *system-prompt-old*
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
- When a question requires external information (weather, time, Virtual machine, terraform, proxmox, etc.), you MUST call a tool.
- To call a tool, use the provided tool_calls mechanism.
- Do not invent tool results. Always wait for the tool response if you call one.
- If no tool is appropriate, answer normally in natural language.
- When you have enough information, provide a clear and concise final answer.")

(defun mcp-print-tools ()
  "Displays the available MCP tools and returns a list of their names."
  (let ((names (mcp-tools-list-text nil)))  ; paramètre ignoré dans mcp-tools-list-text
    (format t "Outils MCP disponibles :~%")
    (dolist (n names)
      (format t " - ~A~%" n))
    (terpri)
    names))

(defun chat-session-ask (session user-input)
  "Sends USER-INPUT to an existing CHAT-SESSION, with context preserved.
Returns the final response (string), updates the session in-place."
  (let* ((messages (chat-session-messages session)))
    ;; Add the user message
    (setf messages
          (append messages
                  (list `(("role" . "user")
                          ("content" . ,user-input)))))
    (multiple-value-bind (answer new-messages)
        (run-mcp-ollama-turn messages (chat-session-tools-schemas session))
      (setf (chat-session-messages session) new-messages)
      answer)))

(defun run-mcp-ollama-session (user-input)
  "One-shot call: creates a new session, sends a question, returns the answer."
  (let* ((session (start-chat-session))
         (answer  (chat-session-ask session user-input)))
    answer))

(defun run-mcp-ollama-turn (messages tools-schemas)
  "Performs a loop iteration (user + any tool_calls) with Ollama.
Takes a list of MESSAGES and TOOLS-SCHEMAS, returns two values :
- the final answer (string)
- updated messages (with assistant + tools)."
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

      ;; Adds the assistant message to the history
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
                    ;; hash-table -> alist conversion for MCP
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
  messages        ;; list of messages for Ollama
  tools-schemas)  ;; diagrams of tools for /api/chat

(defun start-chat-session (&optional (system-prompt *system-prompt*))
  "Creates a new chat session with system prompt and MCP tools loaded."
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
