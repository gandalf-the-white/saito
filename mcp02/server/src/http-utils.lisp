(in-package :saito)

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
