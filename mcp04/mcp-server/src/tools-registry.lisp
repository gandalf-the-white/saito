(in-package :mcp-server)

;; (defstruct mcp-tool
;; name title description input-schema handler)

(defclass mcp-tool ()
  ((name :initarg :name
         :accessor mcp-tool-name)
   (title :initarg :title
          :accessor mcp-tool-title)
   (description :initarg :description
                :accessor mcp-tool-description)
   (input-schema :initarg :input-schema
                 :accessor mcp-tool-input-schema)
   (handler :initarg :handler
            :accessor mcp-tool-handler)))

(defparameter *mcp-tools* nil
  "Registered MCP tools (instances of MCP-TOOL).")

(defun register-mcp-tool (tool)
  (push tool *mcp-tools*)
  tool)

(defun find-mcp-tool (name)
  (find name *mcp-tools* :key #'mcp-tool-name :test #'string=))

(defun mcp-tool->json (tool)
  "Convert MCP-TOOL struct to MCP Tool JSON (alist)."
  `(("name" . ,(mcp-tool-name tool))
    ("title" . ,(mcp-tool-title tool))
    ("description" . ,(mcp-tool-description tool))
    ("inputSchema" . ,(mcp-tool-input-schema tool))))
