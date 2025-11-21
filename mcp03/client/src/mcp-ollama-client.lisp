(defpackage :saito-client
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request :url-encode)
  (:import-from :babel :octets-to-string))

(in-package :saito-client)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

;; URL de l'API chat d'Ollama (local)
(defparameter *ollama-url* "http://localhost:11434/api/chat")

;; Nom du modèle Ollama
(defparameter *ollama-model* "llama3.2")

;; URL HTTP du serveur MCP distant (à adapter à ton infra)
;; Par exemple : "http://localhost:8000/mcp"
(defparameter *mcp-url* "http://localhost:8000/mcp")

(defparameter *debug* t)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[CLIENT]~% ~?" ,fmt (list ,@args))
     (finish-output *error-output*)))

;;; ---------------------------------------------------------------------------
;;; Utilitaires HTTP
;;; ---------------------------------------------------------------------------

(defun body->string (body)
  "Convertit le BODY retourné par drakma:http-request en string UTF-8."
  (cond
    ((stringp body)
     body)
    ((and (arrayp body)
          (= (array-rank body) 1)
          (subtypep (array-element-type body) '(unsigned-byte 8)))
     (octets-to-string body :encoding :utf-8))
    ((vectorp body)
     (map 'string #'code-char body))
    (t
     (error "Type de body non géré: ~S (~A)" body (type-of body)))))

(defun http-post-json (url payload-alist)
  "POST JSON vers URL et renvoie la réponse en string."
  (let ((json (to-json payload-alist :from :alist)))
    (dbg "HTTP POST JSON to ~A: ~A" url json)
    (multiple-value-bind (body status headers uri stream)
        (http-request url
                      :method :post
                      :content json
                      :content-type "application/json"
                      :accept "application/json"
                      :external-format-out :utf-8
                      :external-format-in  :utf-8)
      (declare (ignore headers uri stream))
      (dbg "HTTP POST => status ~A" status)
      (when (>= status 400)
        (error "POST ~A -> HTTP ~A (~A)" url status (body->string body)))
      (body->string body))))

;;; ---------------------------------------------------------------------------
;;; Client MCP via HTTP (JSON-RPC 2.0)
;;; ---------------------------------------------------------------------------

(defparameter *mcp-next-id* 0)

(defun mcp-next-id ()
  (incf *mcp-next-id*))

(defun mcp-http-request (method params)
  "Envoie une requête JSON-RPC 2.0 au serveur MCP via HTTP et renvoie un hash-table."
  (let* ((id  (mcp-next-id))
         (req `(("jsonrpc" . "2.0")
                ("id"      . ,id)
                ("method"  . ,method)
                ("params"  . ,params)))
         (body-str (http-post-json *mcp-url* req)))
    (dbg "MCP response JSON = ~A" body-str)
    (parse body-str :as :hash-table)))

(defun mcp-tools-list ()
  "Récupère la liste des tools MCP (liste de hash-tables)."
  (let* ((resp   (mcp-http-request "tools/list" '()))
         (result (and resp (gethash "result" resp)))
         (tools  (and result (gethash "tools" result))))
    (cond
      ((null tools) nil)
      ((listp tools) tools)
      ((vectorp tools) (coerce tools 'list))
      (t nil))))

(defun mcp-call-tool-text (name args-alist)
  "Appelle tools/call pour le tool NAME avec ARGS-ALIST et renvoie le texte (premier bloc)."
  (let* ((params `(("name" . ,name)
                   ("arguments" . ,args-alist)))
         (resp   (mcp-http-request "tools/call" params))
         (result (and resp (gethash "result" resp)))
         (content (and result (gethash "content" result))))
    (when content
      (let* ((lst (cond
                    ((listp content) content)
                    ((vectorp content) (coerce content 'list))
                    (t nil))))
        (when (and lst (hash-table-p (first lst)))
          (gethash "text" (first lst)))))))

;;; ---------------------------------------------------------------------------
;;; Conversion MCP tools -> Ollama tools (schema tools)
;;; ---------------------------------------------------------------------------

(defun mcp-tools->ollama-tools (mcp-tools)
  "Transforme les tools MCP en 'tools' pour Ollama /api/chat."
  (mapcar
   (lambda (tool)
     (let* ((name        (gethash "name" tool))
            (desc        (or (gethash "description" tool) ""))
            (input-schema (or (gethash "inputSchema" tool)
                              '(("type" . "object")))))
       `(("type" . "function")
         ("function" .
                     (("name" . ,name)
                      ("description" . ,desc)
                      ("parameters" . ,input-schema))))))
   mcp-tools))

;;; ---------------------------------------------------------------------------
;;; Client Ollama avec tools (fonction calling)
;;; ---------------------------------------------------------------------------

(defun ensure-list (x)
  (cond
    ((null x) nil)
    ((listp x) x)
    ((vectorp x) (coerce x 'list))
    (t (list x))))

(defun parse-arguments-maybe (args)
  "Ollama peut renvoyer les arguments soit comme string JSON, soit comme hash-table."
  (cond
    ((hash-table-p args)
     args)
    ((stringp args)
     (handler-case
         (parse args :as :hash-table)
       (error (e)
         (dbg "Erreur de parse des arguments JSON: ~A" e)
         (make-hash-table :test 'equal))))
    (t
     (make-hash-table :test 'equal))))

(defun call-ollama-with-tools (messages tools-schemas)
  "Appelle /api/chat d'Ollama avec historique MESSAGES et 'tools' = TOOLS-SCHEMAS.
Renvoie la réponse parse en hash-table."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ,messages)
             ("tools"    . ,tools-schemas)
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload)))
    (dbg "Ollama raw response = ~A" body-str)
    (parse body-str :as :hash-table)))

;;; ---------------------------------------------------------------------------
;;; System prompt pour guider Llama3.2
;;; ---------------------------------------------------------------------------

(defparameter *agent-system-prompt*
  "You are a tool-using agent.

You have access to remote tools exposed by a Model Context Protocol (MCP) server.
Your available tools are provided in the 'tools' section of the chat API.

IMPORTANT RULES:
- When a question requires external information (weather, time, etc.), you MUST call a tool.
- To call a tool, use the tool calling mechanism you have been given (tool_calls).
- Do not invent tool results. Always wait for the tool response if you call one.
- If no tool is appropriate, answer normally in natural language.")

;;; ---------------------------------------------------------------------------
;;; Boucle d'agent : Ollama + MCP HTTP
;;; ---------------------------------------------------------------------------

(defun run-mcp-ollama-session (user-input)
  "Démarre une session simple :
1) récupère les tools via MCP (HTTP)
2) donne ces tools à Ollama
3) boucle tool_calls -> MCP tools/call -> réponse finale

Renvoie la réponse finale de Llama3.2 (string)."
  (let* ((mcp-tools    (mcp-tools-list))
         (ollama-tools (mcp-tools->ollama-tools mcp-tools))
         (messages
           (list `(("role" . "system")
                   ("content" . ,*agent-system-prompt*))
                 `(("role" . "user")
                   ("content" . ,user-input)))))
    (dbg "Available MCP tools: ~A"
         (mapcar (lambda (tool) (gethash "name" tool)) mcp-tools))
    (loop
      (dbg "--- Appel Ollama ---")
      (dbg "Messages = ~A" messages)

      (let* ((response   (call-ollama-with-tools messages ollama-tools))
             (message    (gethash "message" response))
             (content    (and message (gethash "content" message)))
             (tool-calls (and message (gethash "tool_calls" message))))

        (dbg "message = ~A" message)
        (dbg "content = ~A" content)
        (dbg "tool_calls (brut) = ~A" tool-calls)

        
        ;; on ajoute le message assistant dans l'historique
        (when message
          (push `(("role" . "assistant")
                  ("content" . ,(or content "")))
                messages)
          (setf messages (nreverse messages)))

        (let ((calls (ensure-list tool-calls)))
          (dbg "tool_calls liste = ~A" calls)
          (if (null calls)
              (progn
                (dbg "Pas de tool_call -> réponse finale.")
                (return content))
              (progn
                (dbg "Exécution de ~D tool_call(s)..." (length calls))
                (dolist (tc calls)
                  (let* ((fn-obj    (gethash "function" tc))
                         (fn-name   (and fn-obj (gethash "name" fn-obj)))
                         (raw-args  (and fn-obj (gethash "arguments" fn-obj)))
                         (args-ht   (parse-arguments-maybe raw-args)))
                    (dbg "Tool_call: name=~A args=~A" fn-name args-ht)
                    (when fn-name
                      (let ((tool-result (mcp-call-tool-text fn-name
                                                             ;; convertir hash-table -> alist pour MCP
                                                             (let (alist)
                                                               (maphash (lambda (k v)
                                                                          (push (cons k v) alist))
                                                                        args-ht)
                                                               (nreverse alist)))))
                        (dbg "Résultat tool ~A = ~A" fn-name tool-result)
                        ;; on ajoute un message 'tool' dans l'historique
                        (when tool-result
                          (push `(("role" . "tool")
                                  ("tool_name" . ,fn-name)
                                  ("content" . ,tool-result))
                                messages)
                          (setf messages (nreverse messages))))))))))))))
