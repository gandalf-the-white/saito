(in-package :mcp-server)

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
