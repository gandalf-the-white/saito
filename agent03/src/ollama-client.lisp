(in-package :saito)

(defun test-ollama-simple ()
  "Test simple de /api/chat sans tools."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ((("role" . "user")
                             ("content" . "Dis-moi bonjour en français."))))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload)))
    (format t "~&Réponse brute Ollama = ~A~%~%" body-str)
    (let* ((ht      (parse body-str :as :hash-table))
           (msg     (gethash "message" ht))
           (content (and msg (gethash "content" msg))))
      (format t "Réponse modèle = ~A~%" content)
      content)))

(defun call-ollama (messages)
  "Appelle Ollama avec MESSAGES + tous les tools enregistrés, renvoie un hash-table."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ,messages)
             ("tools"    . ,(tools-schemas))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload)))
    (dbg "Ollama response JSON = ~A" body-str)
    (parse body-str :as :hash-table)))
