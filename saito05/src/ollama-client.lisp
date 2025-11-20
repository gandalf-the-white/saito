(in-package :saito)

(defun test-ollama-simple ()
  "Simple /api/chat test without tools."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ((("role" . "user")
                             ("content" . "Say hello in English."))))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload)))
    (format t "~&Raw Ollama response = ~A~%~%" body-str)
    (let* ((ht      (parse body-str :as :hash-table))
           (msg     (gethash "message" ht))
           (content (and msg (gethash "content" msg))))
      (format t "Model reply = ~A~%" content)
      content)))

(defun call-ollama (messages)
  "Call Ollama /api/chat with the registered tools.
Returns the decoded JSON as a hash-table."
  (let* ((payload
           `(("model"    . ,*ollama-model*)
             ("messages" . ,messages)
             ("tools"    . ,(tools-schemas))
             ("stream"   . :false)))
         (body-str (http-post-json *ollama-url* payload)))
    (dbg "Ollama response JSON = ~A" body-str)
    (parse body-str :as :hash-table)))
