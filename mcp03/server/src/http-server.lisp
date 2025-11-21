(in-package :saito)

(defparameter *mcp-http-server* nil)

(defun mcp-http-handler ()
  "Handler HTTP pour le endpoint /mcp.
Lit le body JSON, appelle handle-json-rpc-request, renvoie JSON."
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (resp-alist (handle-json-rpc-request body))
         (resp-json  (to-json resp-alist :from :alist)))
    (setf (hunchentoot:content-type*) "application/json")
    resp-json))

(defun start-mcp-http-server (&key (port 8000))
  "Démarre un serveur HTTP MCP sur le port donné. Endpoint: POST /mcp"
  (setf hunchentoot:*dispatch-table*
        (list (hunchentoot:create-prefix-dispatcher "/mcp" #'mcp-http-handler)))
  (setf *mcp-http-server*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor
                        :port port
                        :document-root nil)))
  (dbg "MCP HTTP server started on port ~A" port)
  *mcp-http-server*)

(defun stop-mcp-http-server ()
  "Arrête le serveur HTTP MCP."
  (when *mcp-http-server*
    (hunchentoot:stop *mcp-http-server*)
    (setf *mcp-http-server* nil)
    (dbg "MCP HTTP server stopped")))
