(in-package :saito)

;; (defun extract-json-substring (s)
;;   "Extrait la sous-chaîne JSON depuis la première accolade '{'
;;    jusqu'à la dernière '}' dans S. Retourne NIL si rien de plausible."
;;   (when s
;;     (let ((start (position #\{ s))
;;           (end   (position #\} s :from-end t)))
;;       (when (and start end (<= start end))
;;         (subseq s start (1+ end))))))

;; (defun try-manual-tool-json (content)
;;   "Si CONTENT contient un JSON du type {\"name\": ..., \"parameters\"/\"arguments\": {...}},
;;    on exécute le tool correspondant et on renvoie son 'content'. Sinon, retourne NIL."
;;   (let ((json-part (extract-json-substring content)))
;;     (when json-part
;;       (dbg "Tentative de parse JSON manuel depuis: ~A" json-part)
;;       (handler-case
;;           (let* ((ht (parse json-part :as :hash-table))
;;                  (name   (gethash "name" ht))
;;                  ;; le modèle peut utiliser 'parameters' ou 'arguments'
;;                  (params (or (gethash "parameters" ht)
;;                              (gethash "arguments"  ht))))
;;             (when (and name (hash-table-p params))
;;               (let ((tool (find-tool-by-name name)))
;;                 (when tool
;;                   (dbg "Manual JSON tool call détecté: name=~A, params=~A" name params)
;;                   (let* ((msg (funcall (tool-handler tool) params))
;;                          (tool-content (cdr (assoc "content" msg :test #'string=))))
;;                     tool-content)))))
;;         (error (e)
;;           (dbg "Échec parse JSON manuel pour content=~A, erreur=~A" content e)
;;           nil)))))

(defun extract-json-substring (s)
  "Extract a JSON-looking substring from S between the first '{' and the last '}'."
  (when s
    (let ((start (position #\{ s))
          (end   (position #\} s :from-end t)))
      (when (and start end (<= start end))
        (subseq s start (1+ end))))))

(defun try-manual-tool-json (content)
  "If CONTENT contains JSON like {\"name\": ..., \"parameters\"/\"arguments\": {...}},
execute the corresponding tool and return its content. Otherwise return NIL."
  (let ((json-part (extract-json-substring content)))
    (when json-part
      (dbg "Trying manual tool JSON from: ~A" json-part)
      (handler-case
          (let* ((ht (parse json-part :as :hash-table))
                 (name   (gethash "name" ht))
                 (params (or (gethash "parameters" ht)
                             (gethash "arguments"  ht))))
            (when (and name (hash-table-p params))
              (let ((tool (find-tool-by-name name)))
                (when tool
                  (dbg "Manual JSON tool call detected: name=~A params=~A" name params)
                  (let* ((msg (funcall (tool-handler tool) params))
                         (tool-content (cdr (assoc "content" msg :test #'string=))))
                    tool-content)))))
        (error (e)
          (dbg "Failed to parse manual JSON for content=~A, error=~A" content e)
          nil)))))
