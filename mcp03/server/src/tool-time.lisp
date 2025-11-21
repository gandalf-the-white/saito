(in-package :saito)

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
    )
  :handler #'tool-get-time))
