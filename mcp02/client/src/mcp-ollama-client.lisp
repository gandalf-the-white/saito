(defpackage :saito-client
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:export
   :start-mcp-server
   :stop-mcp-server
   :mcp-init
   :mcp-tools-list
   :mcp-call-tool))

(in-package :saito-client)

;;;; ------------------------------------------------------------------------
;;;; Connection structure
;;;; ------------------------------------------------------------------------

(defstruct mcp-connection
  process
  in-stream
  out-stream)

(defparameter *mcp-next-id* 0)

(defun next-id ()
  (incf *mcp-next-id*))

;;;; ------------------------------------------------------------------------
;;;; Start / stop server (SBCL-centric)
;;;; ------------------------------------------------------------------------

(defun start-mcp-server (&key (program "/opt/homebrew/bin/sbcl")
                           (system-name "saito"))
  "Start the MCP server as a child process using SBCL.
Return an MCP-CONNECTION object.

Assumes you have ASDF + Quicklisp, and the system SYSTEM-NAME is loadable."
  (let* ((args (list
                "--noinform"
                "--disable-debugger"
                "--eval" "(push #P\"\/Users\/laurent\/github\/saito\/mcp02\/server\/\" asdf:*central-registry*)"
                "--eval" "(ql:quickload \"saito\")"
                "--eval" "(in-package :saito)"
                "--eval" "(run-mcp-server)"))
         (proc (sb-ext:run-program program args
                                   :input :stream
                                   :output :stream
                                   :error *error-output*
                                   :wait nil)))
    (make-mcp-connection
     :process proc
     :in-stream (sb-ext:process-output proc)
     :out-stream (sb-ext:process-input proc))))

(defun stop-mcp-server (conn)
  "Stop the MCP server process associated with CONN."
  (when (and conn (mcp-connection-process conn))
    (ignore-errors
     (sb-ext:process-kill (mcp-connection-process conn) 15))
    (ignore-errors
     (sb-ext:process-close (mcp-connection-process conn)))))

;;;; ------------------------------------------------------------------------
;;;; Low-level JSON-RPC send/receive
;;;; ------------------------------------------------------------------------

(defun mcp-send-request (conn method &optional (params nil))
  "Envoie une requête JSON-RPC au serveur MCP et attend une véritable réponse JSON.
Ignore toutes les lignes qui ne sont PAS du JSON valide (ex: messages Quicklisp)."
  (let* ((id (next-id))
         (req `(("jsonrpc" . "2.0")
                ("id"      . ,id)
                ("method"  . ,method)
                ("params"  . ,params)))
         (json (to-json req :from :alist)))
    
    ;; Envoi de la requête JSON-RPC
    (format (mcp-connection-out-stream conn) "~A~%" json)
    (finish-output (mcp-connection-out-stream conn))

    ;; Boucle de lecture des réponses
    (loop
      (let ((line (read-line (mcp-connection-in-stream conn) nil nil)))
        (when (null line)
          (error "Le serveur MCP a fermé la connexion."))

        (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
          (when (> (length trimmed) 0)
            (handler-case
                ;; Tentative de parse JSON
                (return (parse trimmed :as :hash-table))

              ;; Si ce n'est pas du JSON → on ignore
              (error (e)
                (format *error-output*
                        "[MCP-CLIENT] Ligne ignorée (pas du JSON): ~A (~A)~%"
                        trimmed e)
                (finish-output *error-output*)))))))))

;;;; ------------------------------------------------------------------------
;;;; High-level helpers: initialize / tools.list / tools.call
;;;; ------------------------------------------------------------------------

(defun mcp-init (conn)
  "Send MCP 'initialize' to the server and return the decoded result hash-table."
  (let* ((params `(("protocolVersion" . "2025-06-18")
                   ("capabilities" .
                                   (("tools" . (("listChanged" . :false)))))
                   ("clientInfo" .
                                 (("name" . "lisp-mcp-client")
                                  ("version" . "0.1.0")))))
         (resp (mcp-send-request conn "initialize" params)))
    resp))

(defun mcp-tools-list (conn)
  "Call MCP method 'tools/list'. Returns the full JSON-RPC response hash-table.
Example : (#<hash-table get_time> #<hash-table get_weather> ...)."
  (let* ((resp   (mcp-send-request conn "tools/list" nil))
         (result (and resp (gethash "result" resp)))
         (tools  (and result (gethash "tools" result))))
    (cond
      ((null tools) nil)
      ((listp tools) tools)
      ((vectorp tools) (coerce tools 'list))
      (t nil))))

(defun mcp-call-tool (conn name &optional (arguments nil))
  "Call MCP method 'tools/call' for tool NAME.
ARGUMENTS is an alist that will be encoded as a JSON object."
  (let* ((params `(("name" . ,name)
                   ("arguments" . ,arguments)))
         (resp   (mcp-send-request conn "tools/call" params)))
    resp))

;;;; ------------------------------------------------------------------------
;;;; Wrappers
;;;; ------------------------------------------------------------------------

(defun mcp-call-text (conn tool-name &optional (arguments nil))
  "Calls an MCP tool and directly returns the text (string)
present in result.content[0].text.
Returns NIL if the tool returns nothing or in case of error."
  (let* ((resp (mcp-call-tool conn tool-name arguments))
         (result (and resp (gethash "result" resp)))
         (content (and result (gethash "content" result))))
    (when (and content
               (arrayp content)
               (> (length content) 0))
      (let ((item (aref content 0)))
        (when (and (hash-table-p item)
                   (gethash "text" item))
          (gethash "text" item))))))

(defun mcp-tools-list-text (conn)
  "Returns the list of MCP tools as a list of strings.
Example : (\"get_time\" \"get_weather\" \"ollama_chat\")"
  (let ((tools (mcp-tools-list conn)))
    (when tools
      (loop for item in tools
            for name = (and (hash-table-p item)
                            (gethash "name" item))
            when name
              collect name))))

(defun mcp-print-tools (conn)
  "Displays the available tools and returns a list of their names.
We probably need to disable *debug*
(setf saito::*debug* nil)"
  (let ((names (mcp-tools-list-text conn)))
    (format t "Available tools :~%")
    (dolist (n names)
      (format t " - ~A~%" n))
    (terpri)
    names))
