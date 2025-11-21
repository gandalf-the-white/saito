(in-package :saito)

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

(defun handle-json-rpc-line (line)
  (handler-case
      (let* ((req    (parse line :as :hash-table))
             (id     (gethash "id" req))
             (method (gethash "method" req))
             (params (gethash "params" req)))
        (dbg "Received request: ~A" line)
        (cond
          ((string= method "initialize")
           (write-json-response (handle-initialize id params)))
          ((string= method "tools/list")
           (write-json-response (handle-tools-list id params)))
          ((string= method "tools/call")
           (write-json-response (handle-tools-call id params)))
          (t
           (write-json-response
            (make-error-response id -32601
                                 (format nil "Unknown method: ~A" method))))))
    (error (e)
      (dbg "Error while handling line ~A : ~A" line e)
      (write-json-response
       `(("jsonrpc" . "2.0")
         ("id"      . nil)
         ("error"   . (("code" . -32700)
                       ("message" . ,(format nil "Parse error: ~A" e)))))))))
