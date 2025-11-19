(in-package :saito)

;; ;; Petit helper de formatage
;; (defun current-time-string (&optional (timezone "UTC"))
;;   "Retourne une string représentant l'heure actuelle.
;;    Pour l'instant, TIMEZONE est purement informatif : on utilise l'heure UTC."
;;   (multiple-value-bind (sec min hour day month year)
;;       (decode-universal-time (get-universal-time))
;;     (declare (ignore)) ; on ignore le 7e MV (weekday), etc.
;;     (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D:~2,'0D"
;;             day month year hour min sec)))

;; ;; Schéma JSON du tool pour Ollama
;; (defparameter *time-tool-schema*
;;   '(("type" . "function")
;;     ("function" .
;;      (("name" . "get_time")
;;       ("description" . "Donner l'heure actuelle, éventuellement pour un fuseau demandé.")
;;       ("parameters" .
;;        (("type" . "object")
;;         ("properties" .
;;          (("timezone" .
;;                       (("type" . "string")
;;                        ("description" . "Nom du fuseau horaire (indicatif), ex: \"UTC\" ou \"local\".")))))))))))

;; (defun make-time-tool ()
;;   "Construit l'objet TOOL pour get_time."
;;   (make-instance 'tool
;;                  :name "get_time"
;;                  :schema *time-tool-schema*
;;                  :handler
;;                  (lambda (args-ht)
;;                    (let* ((tz (or (and args-ht (gethash "timezone" args-ht))
;;                                   "UTC"))
;;                           (now (current-time-string tz))
;;                           (text (format nil "Heure actuelle (~A) : ~A" tz now)))
;;                      `(("role" . "tool")
;;                        ("tool_name" . "get_time")
;;                        ("content" . ,text))))))

;; (defun make-time-tool ()
;;   "Construit l'objet TOOL pour get_time."
;;   (make-instance 'tool
;;                  :name "get_time"
;;                  :schema *time-tool-schema*
;;                  :handler
;;                  (lambda (args-ht)
;;                    ;; le modèle peut envoyer "timezone" ou "tz"
;;                    (let* ((tz (or (and args-ht (gethash "timezone" args-ht))
;;                                   (and args-ht (gethash "tz" args-ht))
;;                                   "UTC"))
;;                           (now (current-time-string tz))
;;                           (text (format nil "Heure actuelle (~A) : ~A" tz now)))
;;                      `(("role" . "tool")
;;                        ("tool_name" . "get_time")
;;                        ("content" . ,text))))))

;; ;; Enregistrement au chargement de ce fichier
;; (register-tool (make-time-tool))

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
