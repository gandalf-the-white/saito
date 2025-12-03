(in-package :mcp-server)

(defparameter *mcp-http-server* nil)

(defun mcp-http-handler ()
  "HTTP handler for the endpoint.
Read the body JSON, call handle-json-rpc-request, send back JSON"
  (let* ((body (tbnl:raw-post-data :force-text t))
         (resp-alist (handle-json-rpc-request body))
         (resp-json (to-json  resp-alist :from :alist)))
    (setf (tbnl:content-type*) "application/json")
    resp-json))

(defun start-mcp-http-server(&key (port *mcp-port*)(host *mcp-host*))
  "Start the HTTP MCP server."
  (setf tbnl:*dispatch-table*
        (list (tbnl:create-prefix-dispatcher "/mcp" #'mcp-http-handler)))
  (setf *mcp-http-server*
        (tbnl:start
         (make-instance 'easy-routes:easy-routes-acceptor
                        :port port
                        :address host)))
  (infor "MCP HTTP server started on port ~A" port)
  (dbg "MCP HTTP server started on port ~A" port)
  *mcp-http-server*)

(defun stop-mcp-http-server ()
  "Stop the HTTP MCP server."
  (when *mcp-http-server*
    (tbnl:stop *mcp-http-server*)
    (setf *mcp-http-server* nil)
    (infor "MCP HTTP server stopped")
    (dbg "MCP HTTP server stopped")))
