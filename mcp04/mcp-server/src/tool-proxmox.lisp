(in-package :mcp-server)

;;------------------------------------------------------------------
;; T O O L   S T A T U S 
;;------------------------------------------------------------------

(defun proxmox-get-json (url token-id token-secret)
  "Performs a GET request to the Proxmox API via drakma and returns a JSON hash table.."
  (multiple-value-bind (body status headers)
      (drakma:http-request
       url
       :method :get
       :additional-headers
       `(("Authorization" . ,(format nil "PVEAPIToken=~A=~A"
                                     token-id token-secret)))
       :want-stream nil)
    (declare (ignore headers))
    (cond
      ((not (= status 200))
       (error "Proxmox API returned status ~A" status))
      (t
       ;; Convert the byte vector to a UTF-8 string
       (let* ((json-string
                (etypecase body
                  (string body)
                  ((vector (unsigned-byte 8))
                   (flex:octets-to-string body :external-format :utf-8))))
              (json (cl-json:decode-json-from-string json-string)))
         json)))))

(defun tool-terraform-check-vm (params)
  "Check via Proxmox that the VM exists and retrieve its current status.."
  (handler-case
      (let* ((node        (gethash "node" params "proxmox"))
             (vmid        (gethash "vmid" params))
             (api         (gethash "api_url" params))
             (token-id    (gethash "token_id" params *token-id*))
             (token-secret (gethash "token_secret" params *token-secret*)))

        ;; Basic control
        (when (null node) (return-from tool-terraform-check-vm "Missing ‘node’ parameter"))
        (when (null vmid) (return-from tool-terraform-check-vm "Missing 'vmid' parameter"))
        (when (null api)  (return-from tool-terraform-check-vm "Missing 'api_url' parameter"))

        ;; Build URL
        (let* ((url (format nil "~A/nodes/~A/qemu/~A/status/current"
                            api node vmid))
               (json (proxmox-get-json url token-id token-secret))
               ;; JSON sent as alist
               (data (cdr (assoc :data json :test #'eq))))

          (with-output-to-string (s)
            (format s "=== Proxmox VM Check ===~%")
            (format s "Node: ~A~%" node)
            (format s "VMID: ~A~%" vmid)
            (format s "Status: ~A~%" (cdr (assoc :status data :test #'eq)))
            (format s "Name: ~A~%"   (cdr (assoc :name data :test #'eq)))
            (format s "Uptime: ~A~%" (cdr (assoc :uptime data :test #'eq)))
            (format s "CPU: ~A~%"    (cdr (assoc :cpu data :test #'eq)))
            (format s "Memory: ~A~%" (cdr (assoc :mem data :test #'eq)))
            (format s "Disk Read: ~A~%" (cdr (assoc :diskread data :test #'eq)))
            (format s "Disk Write: ~A~%" (cdr (assoc :diskwrite data :test #'eq))))))

    (error (e)
      (format nil "Error during the VM check : ~A" e))))

;;------------------------------------------------------------------
;; T O O L   D E L E T E
;;------------------------------------------------------------------

(defun proxmox-delete (api node vmid token-id token-secret)
  "Supprime une VM Proxmox via l’API (DELETE)."
  (let* ((url (format nil "~A/nodes/~A/qemu/~A"
                      api node vmid))
         (auth-header (format nil "PVEAPIToken=~A=~A"
                              token-id token-secret)))
    (multiple-value-bind (body status headers)
        (drakma:http-request
         url
         :method :delete
         :additional-headers `(("Authorization" . ,auth-header))
         :want-stream nil)
      (declare (ignore headers))

      ;; Vérifications de statut HTTP
      (when (not (= status 200))
        (error "Proxmox returned status ~A" status))

      ;; Conversion vecteur-octets → string JSON
      (let* ((json-string
               (etypecase body
                 (string body)
                 ((vector (unsigned-byte 8))
                  (flex:octets-to-string body :external-format :utf-8))))
             (json (cl-json:decode-json-from-string json-string)))

        json))))

(defun tool-proxmox-delete-vm (params)
  "Outil MCP : supprime une VM Proxmox via API REST."
  (handler-case
      (let* ((node         (gethash "node" params))
             (vmid         (gethash "vmid" params))
             (api-url      (gethash "api_url" params))
             (token-id     (gethash "token_id" params))
             (token-secret (gethash "token_secret" params)))

        ;; Vérification des paramètres
        (when (null node)        (return-from tool-proxmox-delete-vm "❌ Paramètre 'node' manquant"))
        (when (null vmid)        (return-from tool-proxmox-delete-vm "❌ Paramètre 'vmid' manquant"))
        (when (null api-url)     (return-from tool-proxmox-delete-vm "❌ Paramètre 'api_url' manquant"))
        (when (null token-id)    (return-from tool-proxmox-delete-vm "❌ Paramètre 'token_id' manquant"))
        (when (null token-secret)(return-from tool-proxmox-delete-vm "❌ Paramètre 'token_secret' manquant"))

        ;; Appel réel à l’API Proxmox
        (let* ((response (proxmox-delete api-url node vmid token-id token-secret))
               (data (cdr (assoc :data response :test #'eq))))

          (with-output-to-string (s)
            (format s "=== Suppression de la VM ~A sur node ~A ===~%" vmid node)
            (format s "UPID: ~A~%" data)
            (format s "La tâche de suppression a été lancée avec succès."))))

    (error (e)
      (format nil "❌ Erreur lors de la suppression de la VM : ~A" e))))

;;------------------------------------------------------------------
;; T O O L   R E G I S T E R
;;------------------------------------------------------------------

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-check-vm"
                :title "Check VM State"
                :description "Check the status of a Proxmox VM via the API (drakma)."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("node" . (("type" . "string")))
                    ("vmid" . (("type" . "number")))
                    ("api_url" . (("type" . "string")))
                    ("token_id" . (("type" . "string")))
                    ("token_secret" . (("type" . "string"))))))
                :handler #'tool-terraform-check-vm))

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "proxmox-delete-vm"
                :title "Delete VM"
                :description "Supprime une VM Proxmox via l’API REST."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("node" . (("type" . "string")))
                    ("vmid" . (("type" . "number")))
                    ("api_url" . (("type" . "string")))
                    ("token_id" . (("type" . "string")))
                    ("token_secret" . (("type" . "string"))))))
                :handler #'tool-proxmox-delete-vm))
