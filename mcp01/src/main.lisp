(defpackage saito
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request :url-encode)
  (:import-from :babel :octets-to-string))

(in-package :saito)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(defparameter *ollama-url* "http://localhost:11434/api/chat")
(defparameter *ollama-model* "llama3.2")

(defparameter *server-name* "lisp-mcp-ollama-server")
(defparameter *server-version* "0.1.0")

;;; ---------------------------------------------------------------------------
;;; Small logging macro
;;; ---------------------------------------------------------------------------

(defparameter *debug* t)

(defmacro dbg (fmt &rest args)
  `(when *debug*
     (format *error-output* "[MCP]~% ~?" ,fmt (list ,@args))
     (finish-output *error-output*)))

;;; ---------------------------------------------------------------------------
;;; HTTP utilities
;;; ---------------------------------------------------------------------------

(defun body->string (body)
  "Convert HTTP BODY (drakma) to UTF-8 string."
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
     (error "Unsupported HTTP body type: ~S (~A)" body (type-of body)))))

(defun http-get (url &key parameters)
  (multiple-value-bind (body status headers uri stream)
      (http-request url
                    :method :get
                    :parameters parameters
                    :external-format-in :utf-8)
    (declare (ignore headers uri stream))
    (dbg "HTTP GET ~A => ~A" url status)
    (when (>= status 400)
      (error "GET ~A -> HTTP ~A (~A)" url status (body->string body)))
    (body->string body)))

(defun http-post-json (url payload-alist)
  "POST JSON and return body as string."
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
      (dbg "HTTP POST ~A => ~A" url status)
      (when (>= status 400)
        (error "POST ~A -> HTTP ~A (~A)" url status (body->string body)))
      (body->string body))))

;;; ---------------------------------------------------------------------------
;;; Tool implementations
;;; ---------------------------------------------------------------------------

;;; ---- get_time -------------------------------------------------------------

(defun current-time-string (&optional (timezone "UTC"))
  "Return current time as 'YYYY-MM-DD HH:MM:SS'. TIMEZONE is informational only."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun tool-get-time (args-ht)
  "MCP tool handler for get_time."
  (let ((tz (or (and args-ht (gethash "timezone" args-ht))
                (and args-ht (gethash "tz" args-ht))
                "UTC")))
    (format nil "Current time (~A): ~A" tz (current-time-string tz))))


;;; ---- get_weather (Open-Meteo) --------------------------------------------

(defun geocode-city (city &key (count 1) (language "en"))
  "Resolve CITY name into (lat lon name country admin1 timezone) using Open-Meteo."
  (let* ((base "https://geocoding-api.open-meteo.com/v1/search")
         (params `(("name"     . ,city)
                   ("count"    . ,(princ-to-string count))
                   ("language" . ,language)
                   ("format"   . "json")))
         (body-str (http-get base :parameters params))
         (data     (parse body-str :as :hash-table))
         (results  (gethash "results" data)))
    (dbg "Geocoding response: ~A" body-str)
    (if (and results (plusp (length results)))
        (let* ((first (cond
                        ((vectorp results) (aref results 0))
                        ((listp results)   (first results))
                        (t                 results)))
               (lat      (gethash "latitude"  first))
               (lon      (gethash "longitude" first))
               (name     (gethash "name"      first))
               (country  (gethash "country"   first))
               (admin1   (gethash "admin1"    first))
               (timezone (gethash "timezone"  first)))
          (values lat lon name country admin1 timezone))
        (values nil nil nil nil nil nil))))

(defun get-weather-from-api (city)
  "Return a short English weather description for CITY using Open-Meteo."
  (multiple-value-bind (lat lon name country admin1 timezone)
      (geocode-city city)
    (unless lat
      (return-from get-weather-from-api
        (format nil "I could not find the city \"~A\" in the Open-Meteo database." city)))
    (let* ((url (format nil
                        "https://api.open-meteo.com/v1/forecast?latitude=~,5F&longitude=~,5F&current_weather=true&timezone=auto"
                        lat lon))
           (body-str (http-get url))
           (data     (parse body-str :as :hash-table))
           (current  (gethash "current_weather" data)))
      (dbg "Forecast response: ~A" body-str)
      (unless current
        (return-from get-weather-from-api
          (format nil "Could not retrieve current weather for ~A." (or name city))))
      (let* ((temp        (or (gethash "temperature"   current) 0.0))
             (windspeed   (or (gethash "windspeed"     current) 0.0))
             (winddir     (or (gethash "winddirection" current) 0.0))
             (weathercode (gethash "weathercode"      current)))
        (format nil
                "Weather in ~A (~@[~A, ~]~A, timezone ~A): ~,1F C, wind ~,1F km/h (direction ~,0F degrees), weather code ~:[unknown~;~:*~A~]."
                (or name city)
                admin1
                country
                timezone
                temp
                windspeed
                winddir
                weathercode)))))

(defun tool-get-weather (args-ht)
  "MCP tool handler for get_weather."
  (let ((city (or (and args-ht (gethash "city" args-ht))
                  (and args-ht (gethash "location" args-ht))
                  "Paris")))
    (get-weather-from-api city)))

;;; ---- ollama_chat ----------------------------------------------------------

(defun call-ollama-chat (prompt)
  "Call Ollama /api/chat with model *ollama-model* and a single user prompt."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ((("role" . "user")
                             ("content" . ,prompt))))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload))
         (ht       (parse body-str :as :hash-table))
         (msg      (gethash "message" ht))
         (content  (and msg (gethash "content" msg))))
    (or content body-str)))

(defun tool-ollama-chat (args-ht)
  "MCP tool handler for ollama_chat."
  (let ((prompt (or (and args-ht (gethash "prompt" args-ht))
                    "Say hello in English.")))
    (call-ollama-chat prompt)))

;;; ---------------------------------------------------------------------------
;;; MCP tool registry
;;; ---------------------------------------------------------------------------

(defstruct mcp-tool
  name title description input-schema handler)

(defparameter *mcp-tools* nil)

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

;;; Register tools (called at load time)
(register-mcp-tool
 (make-mcp-tool
  :name "get_time"
  :title "Current Time"
  :description "Get the current time (UTC or a simple timezone label)."
  :input-schema
  '(("type" . "object")
    ("properties" .
     (("timezone" .
       (("type" . "string")
        ("description" . "Timezone label, for example \"UTC\" or \"local\".")))))
    ;; no required fields
    )
  :handler #'tool-get-time))

(register-mcp-tool
 (make-mcp-tool
  :name "get_weather"
  :title "Weather Information"
  :description "Get the current weather from Open-Meteo for a given city."
  :input-schema
  '(("type" . "object")
    ("properties" .
     (("city" .
       (("type" . "string")
        ("description" . "City name, for example \"Paris\".")))))
    ("required" . ("city")))
  :handler #'tool-get-weather))

(register-mcp-tool
 (make-mcp-tool
  :name "ollama_chat"
  :title "Ollama Chat"
  :description "Call the local Ollama server with llama3.2 using a text prompt."
  :input-schema
  '(("type" . "object")
    ("properties" .
     (("prompt" .
       (("type" . "string")
        ("description" . "User prompt to send to Ollama.")))))
    ("required" . ("prompt")))
  :handler #'tool-ollama-chat))

;;; ---------------------------------------------------------------------------
;;; JSON-RPC helpers
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; MCP method handlers
;;; ---------------------------------------------------------------------------

(defun handle-initialize (id params)
  (declare (ignore params))
  ;; Minimal initialize result (see spec)
  (let ((result
          '(("protocolVersion" . "2025-06-18")
            ("capabilities" .
             (("tools" . (("listChanged" . :false)))))
            ("serverInfo" .
             (("name" . "lisp-mcp-ollama-server")
              ("version" . "0.1.0"))))))
    `(("jsonrpc" . "2.0")
      ("id"      . ,id)
      ("result"  . ,result)))

  )

(defun handle-tools-list (id params)
  (declare (ignore params))
  (let* ((tools-json (mapcar #'mcp-tool->json *mcp-tools*))
         (result `(("tools" . ,tools-json)
                   ;; we do not paginate, so no nextCursor
                   )))
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

;;; ---------------------------------------------------------------------------
;;; Main JSON-RPC dispatcher
;;; ---------------------------------------------------------------------------

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
      ;; If we cannot parse the request, we may not have an id.
      (write-json-response
       `(("jsonrpc" . "2.0")
         ("id"      . nil)
         ("error"   . (("code" . -32700)
                       ("message" . ,(format nil "Parse error: ~A" e)))))))))

;;; ---------------------------------------------------------------------------
;;; Server loop (STDIN / STDOUT)
;;; ---------------------------------------------------------------------------

(defun run-mcp-server ()
  "Run a simple MCP tools server over STDIN/STDOUT.
Each incoming line is expected to be one JSON-RPC 2.0 request."
  (dbg "Starting MCP Ollama server (~A ~A)..." *server-name* *server-version*)
  (loop
    (let ((line (read-line *standard-input* nil nil)))
      (if (null line)
          (progn
            (dbg "End of input, stopping MCP server.")
            (return))
          (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
            (when (> (length trimmed) 0)
              (handle-json-rpc-line trimmed)))))))
