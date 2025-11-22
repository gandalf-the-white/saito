(in-package :mcp-server)

(defun write-json-response (alist)
  "Serialize ALIST as JSON and write it on stdout (one line)."
  (let ((json (to-json alist :from :alist)))
    (format t "~A~%" json)
    (finish-output)))

(defun make-error-response (id code message)
  `(("jsonrpc" . "2.0")
    ("id"      . ,id)
    ("error"   . (("code" . ,code)
                  ("message" . ,message)))))

(defun handle-initialize (id params)
  (declare (ignore params))
  (let ((result
          '(("protocolVersion" . "2025-06-18")
            ("capabilities" .
             (("tools" . (("listChanged" . :false)))))
            ("serverInfo" .
             (("name" . "lisp-saito")
              ("version" . "0.1.0"))))))
    `(("jsonrpc" . "2.0")
      ("id"      . ,id)
      ("result"  . ,result))))

(defun handle-tools-list (id params)
  (declare (ignore params))
  (let* ((tools-json (mapcar #'mcp-tool->json *mcp-tools*))
         (result `(("tools" . ,tools-json))))
    `(("jsonrpc" . "2.0")
      ("id"      . ,id)
      ("result"  . ,result))))

(defun handle-tools-call (id params)
  (let* ((name      (and params (gethash "name" params)))
         (arguments (and params (gethash "arguments" params))))
    (let ((tool (and name (find-mcp-tool name))))
      (if (null tool)
          (make-error-response id -32602 (format nil "Unknown tool: ~A" name))
          (handler-case
              (let* ((text-result (funcall (mcp-tool-handler tool) arguments))
                     (result `(("content" .
                                ((( "type" . "text")
                                  ("text" . ,text-result))))
                               ("isError" . :false))))
                `(("jsonrpc" . "2.0")
                  ("id"      . ,id)
                  ("result"  . ,result)))
            (error (e)
              (let ((msg (format nil "Tool ~A error: ~A" name e)))
                `(("jsonrpc" . "2.0")
                  ("id"      . ,id)
                  ("result"  . (("content" .
                                           ((( "type" . "text")
                                             ("text" . ,msg))))
                                ("isError" . :true)))))))))))

(defun handle-json-rpc-request (json-string)
  "Take a JSON-RPC request (string) and send back a alist"
  (handler-case
      (let* ((req    (parse json-string :as :hash-table))
             (id     (gethash "id" req))
             (method (gethash "method" req))
             (params (gethash "params" req)))
        (dbg "Received request: ~A" json-string)
        (cond
          ((string= method "initialize")
           (handle-initialize id params))
          ((string= method "tools/list")
           (handle-tools-list id params))
          ((string= method "tools/call")
           (handle-tools-call id params))
          (t
           (make-error-response id -32601
                                (format nil "Unknown method: ~A" method)))))
    (error (e)
      (dbg "Error while handling JSON-RPC req ~A : ~A" json-string e)
      `(("jsonrpc" . "2.0")
        ("id"      . nil)
        ("error"   . (("code" . -32700)
                      ("message" . ,(format nil "Parse error: ~A" e))))))))

(defun handle-json-rpc-line (line)
  "Version STDIN/STDOUT (old), compatibility."
  (let ((resp (handle-json-rpc-request line)))
    (write-json-response resp)))
