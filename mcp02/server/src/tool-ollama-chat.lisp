(in-package :saito)

(defun call-ollama-chat (prompt)
  "Call Ollama /api/chat with model *ollama-model* and a single user prompt."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ((("role" . "user")
                             ("content" . ,prompt))))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload))
         (ht       (parse body-str :as :hash-table))
         (msg      (gethash "message" ht))
         (content  (and msg (gethash "content" msg))))
    (or content body-str)))

(defun tool-ollama-chat (args-ht)
  "MCP tool handler for ollama_chat."
  (let ((prompt (or (and args-ht (gethash "prompt" args-ht))
                    "Say hello in English.")))
    (call-ollama-chat prompt)))

(register-mcp-tool
 (make-mcp-tool
  :name "ollama_chat"
  :title "Ollama Chat"
  :description "Call the local Ollama server with llama3.2 using a text prompt."
  :input-schema
  '(("type" . "object")
    ("properties" .
     (("prompt" .
       (("type" . "string")
        ("description" . "User prompt to send to Ollama.")))))
    ("required" . ("prompt")))
  :handler #'tool-ollama-chat))
