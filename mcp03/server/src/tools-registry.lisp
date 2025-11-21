(in-package :saito)

(defstruct mcp-tool
  name title description input-schema handler)

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
