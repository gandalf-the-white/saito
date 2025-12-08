(in-package :mcp-client)

(defparameter *mcp-next-id* 0)

(defun mcp-next-id ()
  (incf *mcp-next-id*))

(defun mcp-http-request (method params)
  "Sends a JSON-RPC 2.0 request to the MCP server via HTTP and returns a hash table."
  (let* ((id  (mcp-next-id))
         (req `(("jsonrpc" . "2.0")
                ("id"      . ,id)
                ("method"  . ,method)
                ("params"  . ,params)))
         (body-str (http-post-json *mcp-url* req)))
    (dbg "MCP response JSON = ~A" body-str)
    (parse body-str :as :hash-table)))

(defun mcp-tools-list ()
  "Returns the list of MCP tools (list of hash tables)."
  (let* ((resp   (mcp-http-request "tools/list" '()))
         (result (and resp (gethash "result" resp)))
         (tools  (and result (gethash "tools" result))))
    (cond
      ((null tools) nil)
      ((listp tools) tools)
      ((vectorp tools) (coerce tools 'list))
      (t nil))))

(defun mcp-tools-list-text (conn-ignored)
  "Returns the list of MCP tool names as a list of strings.
conn-ignored is there to be compatible with the old signature, but ignored."
  (declare (ignore conn-ignored))
  (let ((tools (mcp-tools-list)))
    (when tools
      (loop for item in tools
            for name = (and (hash-table-p item)
                            (gethash "name" item))
            when name
              collect name))))

(defun mcp-call-tool-text (name args-alist)
  "Call tools/call for the tool NAME with ARGS-ALIST.
Returns the text of the first block (result.content[0].text)."
  (let* ((params `(("name" . ,name)
                   ("arguments" . ,args-alist)))
         (resp   (mcp-http-request "tools/call" params))
         (result (and resp (gethash "result" resp)))
         (content (and result (gethash "content" result))))
    (when content
      (let* ((lst (cond
                    ((listp content) content)
                    ((vectorp content) (coerce content 'list))
                    (t nil)))
             (item (and lst (first lst))))
        (when (and item (hash-table-p item))
          (gethash "text" item))))))
