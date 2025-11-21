(in-package :saito)

(defun current-time-string (&optional (timezone "UTC"))
  "Return a simple string representing the current time. Timezone is informational only."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore))
    (format nil "~2,'0D-~2,'0D-~4,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defparameter *time-tool-schema*
  '(("type" . "function")
    ("function" .
     (("name" . "get_time")
      ("description" . "Return the current time, optionally for a given timezone label.")
      ("parameters" .
       (("type" . "object")
        ("properties" .
         (("timezone" .
                      (("type" . "string")
                       ("description" . "Timezone label, for example \"UTC\" or \"local\".")))))))))))

(defun make-time-tool ()
  (make-instance 'tool
                 :name "get_time"
                 :schema *time-tool-schema*
                 :handler
                 (lambda (args-ht)
                   (let* ((tz (or (and args-ht (gethash "timezone" args-ht))
                                  (and args-ht (gethash "tz" args-ht))
                                  "UTC"))
                          (now (current-time-string tz))
                          (text (format nil "Current time (~A): ~A" tz now)))
                     `(("role" . "tool")
                       ("tool_name" . "get_time")
                       ("content" . ,text))))))

(register-tool (make-time-tool))
