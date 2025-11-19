(in-package :saito)

(defclass tool ()
  ((name
    :initarg :name
    :reader tool-name)
   (schema
    :initarg :schema
    :reader tool-schema
    :documentation "Schéma JSON (ALIST) pour la clé tools d’Ollama.")
   (handler
    :initarg :handler
    :reader tool-handler
    :documentation "Fonction (lambda (arguments-hash-table)) => message tool.")))

(defparameter *tools* '()
  "Liste des tools enregistrés (instances de TOOL).")

(defun register-tool (tool)
  (push tool *tools*)
  tool)

(defun find-tool-by-name (name)
  (find name *tools* :key #'tool-name :test #'string=))

(defun tools-schemas ()
  "Liste des schémas JSON de tous les tools pour la requête /api/chat."
  (mapcar #'tool-schema *tools*))

(defun ensure-list (x)
  (cond
    ((null x) nil)
    ((listp x) x)
    ((vectorp x) (coerce x 'list))
    (t (list x))))

(defun handle-tool-call (tool-call)
  "tool-call est un hash-table issu de la réponse d’Ollama.
   Cherche le bon tool et appelle son handler."
  (let* ((fn-obj    (gethash "function" tool-call))
         (fn-name   (gethash "name" fn-obj))
         (arguments (gethash "arguments" fn-obj))  ; hash-table
         (tool      (find-tool-by-name fn-name)))
    (if tool
        (funcall (tool-handler tool) arguments)
        ;; fallback: tool inconnu côté Lisp
        `(("role" . "tool")
          ("tool_name" . ,fn-name)
          ("content" . ,(format nil "Tool inconnu: ~A" fn-name))))))
