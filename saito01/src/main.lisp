(defpackage saito
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request :url-encode)
  (:import-from :babel :octets-to-string))

(in-package :saito)

(defparameter *ollama-url* "http://localhost:11434/api/chat")
(defparameter *ollama-model* "llama3.2")

(defparameter *agent-debug* nil)

(defmacro dbg (fmt &rest args)
  `(when *agent-debug*
     (format *trace-output* "[AGENT] ~?" ,fmt (list ,@args))))

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

(defun geocode-city (city &key (count 1) (language "fr"))
  "Résout le nom de ville CITY en (lat lon name country admin1 timezone)
   via https://geocoding-api.open-meteo.com."
  (let* ((base "https://geocoding-api.open-meteo.com/v1/search")
         (query-params `(("name"     . ,city)
                         ("count"    . ,(princ-to-string count))
                         ("language" . ,language)
                         ("format"   . "json")))
         (raw-body (multiple-value-bind (body status headers uri stream)
                       (http-request base
                                     :method :get
                                     :parameters query-params)
                     (declare (ignore status headers uri stream))
                     body))
         (body-str (body->string raw-body))
         (data     (parse body-str :as :hash-table))
         (results  (gethash "results" data)))
    (dbg "Open-Meteo geocode body = ~A" body-str)
    (dbg "Type de results = ~A" (type-of results))

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
  "Retourne une phrase en français décrivant la météo actuelle de CITY
   en utilisant Open-Meteo (géocodage + forecast)."
  (multiple-value-bind (lat lon name country admin1 timezone)
      (geocode-city city)
    (unless lat
      (return-from get-weather-from-api
        (format nil "Je n'ai pas trouvé la ville \"~A\" dans la base Open-Meteo." city)))
    (let* ((url (format nil
                        "https://api.open-meteo.com/v1/forecast?latitude=~,5F&longitude=~,5F&current_weather=true&timezone=auto"
                        lat lon))
           (raw-body (multiple-value-bind (body status headers uri stream)
                         (http-request url :method :get)
                       (declare (ignore status headers uri stream))
                       body))
           (body-str (body->string raw-body))
           (data     (parse body-str :as :hash-table))
           (current  (gethash "current_weather" data)))
      (dbg "Open-Meteo forecast body = ~A" body-str)
      (unless current
        (return-from get-weather-from-api
          (format nil "Impossible de récupérer la météo actuelle pour ~A." (or name city))))
      (let* ((temp        (or (gethash "temperature"   current) 0.0))
             (windspeed   (or (gethash "windspeed"     current) 0.0))
             (winddir     (or (gethash "winddirection" current) 0.0))
             (weathercode (gethash "weathercode"      current)))
        (format nil
                "Météo pour ~A (~@[~A, ~]~A, fuseau ~A) : ~,1F°C, vent ~,1F km/h (direction ~,0F°), code météo ~:[inconnu~;~:*~A~]."
                (or name city)
                admin1
                country
                timezone
                temp
                windspeed
                winddir
                weathercode)))))


(defparameter *weather-tool-schema*
  '(("type" . "function")
    ("function" .
     (("name" . "get_weather")
      ("description" . "Obtenir la météo actuelle d'une ville avec Open-Meteo.")
      ("parameters" .
       (("type" . "object")
        ("required" "city")
        ("properties" .
         (("city" .
                  (("type" . "string")
                   ("description" . "Nom de la ville, par exemple \"Paris\".")))))))))))


(defun ensure-list (x)
  (cond
    ((null x) nil)
    ((listp x) x)
    ((vectorp x) (coerce x 'list))
    (t (list x))))

(defun call-ollama (messages)
  "Appelle Ollama avec les MESSAGES + tool get_weather, renvoie la réponse (hash-table)."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ,messages)
             ("tools"    . ,(list *weather-tool-schema*))
             ;; IMPORTANT : booléen JSON false
             ("stream"   . :false)))
         (json (to-json payload :from :alist)))
    (dbg "Payload JSON = ~A" json)
    (multiple-value-bind (body status headers uri stream)
        (http-request *ollama-url*
                      :method :post
                      :content json
                      :content-type "application/json"
                      :accept "application/json")
      (declare (ignore headers uri stream))
      (let* ((body-str (body->string body)))
        (dbg "Status        = ~A" status)
        (dbg "Body (string) = ~A" body-str)
        (when (/= status 200)
          (error "Ollama HTTP ~A. Corps : ~A" status body-str))
        (parse body-str :as :hash-table)))))

(defun handle-tool-call (tool-call)
  "Exécute l'appel de tool retourné par Ollama et renvoie un message role: tool."
  (let* ((fn-obj    (gethash "function" tool-call))
         (fn-name   (gethash "name" fn-obj))
         (arguments (gethash "arguments" fn-obj)) ; hash-table
         (city      (and arguments (gethash "city" arguments))))
    (cond
      ((string= fn-name "get_weather")
       (let ((result (get-weather-from-api city)))
         `(("role" . "tool")
           ("tool_name" . "get_weather")
           ("content" . ,result))))
      (t
       `(("role" . "tool")
         ("tool_name" . ,fn-name)
         ("content" . ,(format nil "Tool inconnu: ~A" fn-name)))))))

(defun run-weather-agent (user-input)
  "Envoie USER-INPUT à llama3.2 via Ollama avec le tool météo Open-Meteo."
  (let ((messages (list `(("role" . "user")
                          ("content" . ,user-input)))))
    (dbg "=== DÉBUT AGENT ===")
    (loop
      (dbg "--- Appel Ollama ---")
      (dbg "Messages = ~S" messages)

      (let* ((response   (call-ollama messages))
             (message    (gethash "message" response))
             (content    (and message (gethash "content" message)))
             (tool-calls (and message (gethash "tool_calls" message))))

        (dbg "Réponse = ~S" response)
        (dbg "message = ~S" message)
        (dbg "content = ~S" content)
        (dbg "tool_calls (brut) = ~S" tool-calls)

        ;; on ajoute ce que l'assistant vient de "dire"
        (when message
          (push `(("role" . "assistant")
                  ("content" . ,(or content "")))
                messages)
          (setf messages (nreverse messages)))

        (let ((calls (ensure-list tool-calls)))
          (dbg "tool_calls liste = ~S" calls)
          (if (null calls)
              (progn
                (dbg "Pas de tool_call -> réponse finale.")
                (return content))
              (progn
                (dbg "Exécution de ~D tool_call(s)..." (length calls))
                (dolist (tc calls)
                  (let ((tool-msg (handle-tool-call tc)))
                    (dbg "tool_msg = ~S" tool-msg)
                    (setf messages (append messages (list tool-msg))))))))))))

(defun test-ollama-simple ()
  "Test simple de /api/chat sans tools, avec debug JSON."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ((("role" . "user")
                             ("content" . "Dis-moi bonjour en français."))))
             ;; booléen JSON false (pour jonathan -> :false)
             ("stream"   . :false)))
         (json (to-json payload :from :alist)))
    (format t "~&JSON envoyé à Ollama = ~A~%~%" json)
    (multiple-value-bind (body status headers uri stream)
        (http-request *ollama-url*
                      :method :post
                      :content json
                      :content-type "application/json"
                      :accept "application/json")
      (declare (ignore headers uri stream))
      (let* ((body-str (body->string body)))
        (format t "Status        = ~A~%" status)
        (format t "Body (string) = ~A~%~%" body-str)
        (if (/= status 200)
            (progn
              (format t "~&*** Erreur HTTP côté Ollama ***~%")
              (format t "Corps erreur : ~A~%" body-str)
              nil)
            ;; Status 200 : on tente de parser, mais avec handler-case
            (handler-case
                (let* ((ht      (parse body-str :as :hash-table))
                       (msg     (gethash "message" ht))
                       (content (and msg (gethash "content" msg))))
                  (format t "Clés top-level = ~S~%"
                          (loop for k being the hash-keys of ht collect k))
                  (format t "message = ~A~%" msg)
                  (format t "content = ~A~%" content)
                  content)
              (error (e)
                (format t "~&*** Erreur Jonathan lors du parse JSON ***~%")
                (format t "Erreur = ~A~%Body (string) = ~A~%" e body-str)
                nil)))))))

(defun test ()
  (run-weather-agent
   "Peux-tu me donner la météo actuelle à Paris ?"))
