(in-package :mcp-client)

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

(defun mcp-tools->ollama-tools (mcp-tools)
  "Transforme les tools MCP en 'tools' pour Ollama /api/chat."
  (mapcar
   (lambda (tool)
     (let* ((name         (gethash "name" tool))
            (desc         (or (gethash "description" tool) ""))
            (input-schema (or (gethash "inputSchema" tool)
                              '(("type" . "object")))))
       `(("type" . "function")
         ("function" .
                     (("name" . ,name)
                      ("description" . ,desc)
                      ("parameters" . ,input-schema))))))
   mcp-tools))

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
