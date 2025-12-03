(in-package :mcp-client)

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

(defun http-post-json (url payload-alist)
  "POST JSON to URL and return response body as string."
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
      (dbg "HTTP POST => status ~A" status)
      (when (>= status 400)
        (error "POST ~A -> HTTP ~A. Body: ~A" url status (body->string body)))
      (body->string body))))
