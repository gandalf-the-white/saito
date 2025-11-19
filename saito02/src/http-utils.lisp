(in-package :saito)

(defun body->string (body)
  "Convertit le BODY retourné par drakma:http-request en string UTF-8.
   Gère string, vecteur d'octets, simple-vector de fixnums."
  (cond
    ((stringp body)
     body)
    ;; Vecteur d'octets (unsigned-byte 8)
    ((and (arrayp body)
          (= (array-rank body) 1)
          (subtypep (array-element-type body) '(unsigned-byte 8)))
     (octets-to-string body :encoding :utf-8))
    ;; Simple-vector de fixnums 0-255
    ((vectorp body)
     (map 'string #'code-char body))
    (t
     (error "Type de body non géré: ~S (~A)" body (type-of body)))))

(defun http-get (url &key parameters)
  "GET simple, renvoie le body en string."
  (multiple-value-bind (body status headers uri stream)
      (http-request url :method :get :parameters parameters)
    (declare (ignore headers uri stream))
    (dbg "HTTP GET ~A => ~A" url status)
    (when (>= status 400)
      (error "GET ~A -> HTTP ~A (~A)" url status (body->string body)))
    (body->string body)))

(defun http-post-json (url payload-alist)
  "POST JSON vers URL, renvoie le body en string."
  (let* ((json (to-json payload-alist :from :alist)))
    (dbg "HTTP POST JSON => ~A" json)
    (multiple-value-bind (body status headers uri stream)
        (http-request url
                      :method :post
                      :content json
                      :content-type "application/json"
                      :accept "application/json")
      (declare (ignore headers uri stream))
      (dbg "HTTP POST ~A => ~A" url status)
      (when (>= status 400)
        (error "POST ~A -> HTTP ~A (~A)" url status (body->string body)))
      (body->string body))))
