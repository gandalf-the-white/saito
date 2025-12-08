(in-package :mcp-server)

;;------------------------------------------------------------------
;; T O O L   S C R I P T 
;;------------------------------------------------------------------

(defun load-template (path)
  (with-open-file (in path :direction :input)
    (let ((content (make-string (file-length in))))
      (read-sequence content in)
      content)))

(defun tool-get-script-tf (params)
  "MCP tool handler for terraform-proxmox."
  (let* ((endpoint (gethash "endpoint" params *end-point*))
         (api_token (gethash "api_token" params *api-token*))
         (publkeyctn (gethash "publkeyctn" params *ctn-public-key*))
         (target_node (gethash "target_node" params "proxmox"))
         (cores (gethash "cores" params 2))
         (memory (gethash "memory" params 512))
         (storage (gethash "storage" params "local-lvm"))
         (image (gethash "image" params "local:vztmpl/debian-12-standard_12.12-1_amd64.tar.zst"))
         (bridge (gethash "bridge" params "vmbr3"))
         (vlan (gethash "vlan" params 200))
         (prefix (gethash "prefix" params "192.188.200"))
         (octet (gethash "octet" params "109"))
         (domain (gethash "domain" params "bebop.pmox"))
         (nameserver (gethash "nameserver" params "192.168.68.1"))
         (name (gethash "name" params))
         (key (gethash "key" params *private-key*))
         (path (format nil "platform/~A.tf" name))
         (template (load-template "templates/template_vm.tf"))
         (content (format nil template
                          endpoint api_token publkeyctn target_node cores memory storage
                          image bridge vlan prefix octet domain nameserver name key))
         (content (string-right-trim '(#\Space #\Newline #\Return #\Tab #\~) content))
         (content (coerce content 'string))
         (content (remove #\Nul content)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (dbg "Fichier ecrit: ~A (~A octets)" path (length content))
    (format nil "Terraform script generated at: ~A" path)))

;;------------------------------------------------------------------
;; T O O L   V I E W 
;;------------------------------------------------------------------

(defun tool-view-script-tf (params)
  "Send back the terraform file (.tf)"
  (handler-case
      (let* ((path (gethash "path" params))
             (max-size (* 100 1024))) ;; 100 ko max
        (cond
          ((null path)
           (format nil "No path included. We need it: \"path\"."))
          ((not (probe-file path))
           (format nil "File unknown: ~A" path))
          (t
           (let* ((pathname (pathname path))
                  (ext (pathname-type pathname)))
             (unless (and ext (string-equal ext "tf"))
               (return-from tool-view-script-tf
                 (format nil "The file ~A is not a terraform file (.tf)." path)))
             (with-open-file (in path :direction :input :external-format :utf-8)
               (let* ((size (file-length in))
                      (to-read (min size max-size))
                      (content (make-string to-read)))
                 (read-sequence content in :end to-read)
                 (setf content (string-right-trim '(#\Space #\Newline #\Return #\Tab #\~) content))
                 (setf content (remove #\Nul content))
                 (when (> size max-size)
                   (setf content (concatenate 'string content
                                              (format nil "~%~%… [Truncated : file > 100 Ko] …"))))
                 (setf content (pretty-print-tf content))
                 (format nil "**Terraform file**: `~A` (~A bytes)~%~%```~%~A~%```~%"
                         path size content)))))))
    (error (e)
      (format nil "Error reading file: ~A" e))))

;;------------------------------------------------------------------
;; T O O L   V A L I D A T E
;;------------------------------------------------------------------

(defun run-terraform-validate (path &key (terraform-bin "terraform"))
  "Validate script file (.tf)"
  (let* ((path-name (pathname path))
         (dir (make-pathname :directory (pathname-directory path-name))))
    (dbg "path: ~A~% path-name: ~A~% dir: ~A ~% bin: ~A" path path-name dir terraform-bin)
    (multiple-value-bind (stdout stderr code)
        (uiop:run-program
         (list terraform-bin "validate" "-no-color")
         :directory dir
         :output :string
         :error-output :string
         :ignore-error-status t)
      (values code stdout stderr))))

(defun tool-validate-terraform-tf (params)
  "MCP tool handler to validate a terraform file (.tf)."
  (handler-case
      (let* ((path (gethash "path" params))
             (run-tf (gethash "run-terraform" params t))
             (terraform-bin (or (gethash "terraform-bin" params)
                                "terraform")))
        (cond
          ((null path)
           (dbg "No path include. We need it: \"path\".")
           (format nil "No path include. We need it: \"path\"."))
          ((not (probe-file path))
           (dbg "File unknown: ~A" path)
           (format nil "File unknown: ~A" path))
          (t
           (let* ((exit-code nil)
                  (stdout "")
                  (stderr ""))
             (when run-tf (multiple-value-setq (exit-code stdout stderr)
                            (run-terraform-validate path :terraform-bin terraform-bin)))
             (with-output-to-string (out)
               (format out "Terraform Validation : ~A~%~%" path)
               (cond
                 ((not run-tf)
                  (dbg "Ignored (run_terraform=false).~%")
                  (format out "Ignored (run_terraform=false).~%"))
                 ((or (null exit-code)(minusp exit-code))
                  (dbg "Not done right.~%")
                  (format out "Not done right.~%"))
                 ((zerop exit-code)
                  (dbg "OK (exit-code ~A).~%" exit-code)
                  (format out "OK (exit-code ~A).~%" exit-code))
                 (t
                  (dbg "Echec (exit-code ~A).~%" exit-code)
                  (format out "Echec (exit-code ~A).~%" exit-code)))
               (when (> (length stdout) 0)
                 (format out "~%--- stdout ---~%~A~%" stdout))
               (when (> (length stderr) 0)
                 (format out "~%--- stderr ---~%~A~%" stderr)))))))
    (error (e)
      (format nil "Error reading file: ~A" e))))

;;------------------------------------------------------------------
;; T O O L   A P P L Y
;;------------------------------------------------------------------

(defun run-terraform-command (cmd-list &key directory)
  "Apply script file (.tf)"
  (dbg "cmd-list: ~A~% dir: ~A ~%" cmd-list directory)
  (multiple-value-bind (out err code)
      (uiop:run-program
       cmd-list
       :directory directory
       :output :string
       :error-output
       :ignore-error-status t)
    (values code out err)))

(defun tool-apply-terraform-tf (params)
  "MCP tool handler to apply a terraform file (.tf)."
  (handler-case
      (let* ((path (gethash "path" params))
             (terraform-bin (or (gethash "terraform-bin" params)
                                "terraform")))
        (cond
          ((null path)
           (format nil "No path include. We need it: \"path\"."))
          ((not (probe-file path))
           (format nil "File unknown: ~A" path))
          (t
           (let* ((tru-path (truename path))
                  (dir (uiop:pathname-directory-pathname tru-path)))
             ;; terraform init
             (multiple-value-bind (init-code init-out init-err)
                 (run-terraform-command
                  (list terraform-bin "init" "-no-color")
                  :directory dir)
               ;; init failed
               (when (not (zerop init-code))
                 (return-from tool-apply-terraform-tf
                   (format nil "terraform init failed (exit ~A)\n--- stdout ---\n~A\n--- stderr ---\n~A"
                           init-code init-out init-err)))
               ;; init apply
               (multiple-value-bind (init-code apply-out apply-err)
                   (run-terraform-command
                    (list terraform-bin "apply" "-auto-approve" "-no-color")
                    :directory dir)

                 (with-output-to-string (out)
                   (format out "=== terraform apply ===~%")
                   (format out "exit code: ~A~%~%" init-code)
                   (format out "--- stdout ---~%~A~%" apply-out)
                   (format out "--- stderr ---~%~A~%" apply-err))))))))
    (error (e)
      (format nil "Error apply file: ~A" e))))


;;------------------------------------------------------------------
;; T O O L   D E S T R O Y
;;------------------------------------------------------------------

(defun run-terraform-command (cmd-list &key directory)
  "Exécute une commande terraform et retourne (exit-code stdout stderr)."
  (multiple-value-bind (out err code)
      (uiop:run-program
       cmd-list
       :directory directory
       :output :string
       :error-output :string
       :ignore-error-status t)
    (values code out err)))

(defun tool-terraform-destroy-vm (params)
  "Outil MCP : détruit la VM via 'terraform destroy -auto-approve'."
  (handler-case
      (let* ((path (gethash "path" params))
             (terraform (or (gethash "terraform_bin" params) "terraform")))

        ;; Validations
        (when (null path)
          (return-from tool-terraform-destroy-vm "Missing ‘path’ parameter"))

        (when (not (probe-file path))
          (return-from tool-terraform-destroy-vm
            (format nil "The file does not exist. : ~A" path)))

        (let* ((tru (truename path))
               (dir (uiop:pathname-directory-pathname tru)))

          ;; terraform init
          (multiple-value-bind (init-code init-out init-err)
              (run-terraform-command
               (list terraform "init" "-no-color")
               :directory dir)

            (when (not (zerop init-code))
              (return-from tool-terraform-destroy-vm
                (format nil
                        "terraform init failed (exit ~A)\n--- stdout ---\n~A\n--- stderr ---\n~A"
                        init-code init-out init-err)))

            ;; terraform destroy
            (multiple-value-bind (code out err)
                (run-terraform-command
                 (list terraform "destroy" "-auto-approve" "-no-color")
                 :directory dir)

              (with-output-to-string (s)
                (format s "=== terraform destroy ===~%")
                (format s "exit code: ~A~%~%" code)
                (format s "--- stdout ---~%~A~%" out)
                (format s "--- stderr ---~%~A~%" err))))))

    (error (e)
      (format nil "Error during terraform destroy : ~A" e))))



;;------------------------------------------------------------------
;; T O O L   R E G I S T E R
;;------------------------------------------------------------------

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-proxmox"
                :title "Terraform Script"
                :description "Generate a Terraform configuration file for Proxmox VMs."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("name" .
                     (("type" . "string")
                      ("description" . "Name of the VM.")))
                    ("cores" .
                     (("type" . "integer")
                      ("description" . "CPU count.")))
                    ("memory" .
                     (("type" . "integer")
                      ("description" . "RAM in MB.")))
                    ("octet" .
                     (("type" . "string")
                      ("description" . "Lat digit of a IP address, e.g. \"100\"")))
                    ("image" .
                     (("type" . "string")
                      ("description" . "System name, e.g. \"local:vztmpl\/debian-12-standard_12.12-1_amd64.tar.zst\"")))
                    ("prefix" .
                     (("type" . "string")
                      ("description" . "Prefix of the IP address, e.g. \"192.168.28\".")))
                    ("nameserver" .
                     (("type" . "string")
                      ("description" . "Nameserver IP address, e.g. \"192.168.28.1\".")))
                    ("vlan" .
                     (("type" . "integer")
                      ("description" . "Vlan of the IP address, e.g. \"200\".")))
                    ("bridge" .
                     (("type" . "string")
                      ("description" . "Network bridge, e.g. vmbr0.")))
                    ("key" .
                     (("type" . "string")
                      ("description" . "ssh private key"))))))
                :handler #'tool-get-script-tf))

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-view-file"
                :title "Terraform View"
                :description "View the content of a generated Terraform (.tf) file."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("path" .
                     (("type" . "string")
                      ("description" . "Absolute path of the Terraform file to display."))))))
                :handler #'tool-view-script-tf))

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-validate-file"
                :title "Terraform Validate"
                :description "Validates a Terraform file (.tf) for creating Proxmox VMs."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("path" .
                     (("type" . "string")
                      ("description" . "Absolute path to the Terraform (.tf) file to be validated..")))
                    ("run-terraform" .
                     (("type" . "boolean")
                      ("description" . "If true, run `terraform validate` in the file directory.")))
                    ("terraform-bin" .
                     (("type" . "string")
                      ("description" . "Terraform binary to use (default is \"terraform\")."))))))
                :handler #'tool-validate-terraform-tf))

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-apply-vm"
                :title "Terraform Apply"
                :description "Create the VM with the command \"terraform apply --auto-approve\"."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("path" .
                     (("type" . "string")
                      ("description" . "Absolute path to the Terraform (.tf) file to apply.")))
                    ("run-terraform" .
                     (("type" . "boolean")
                      ("description" . "If true, run `terraform apply` in the file directory.")))
                    ("terraform-bin" .
                     (("type" . "string")
                      ("description" . "Terraform binary to use (default is \"terraform\")."))))))
                :handler #'tool-apply-terraform-tf))

(register-mcp-tool
 (make-instance 'mcp-tool
                :name "terraform-destroy-vm"
                :title "Terraform Destroy"
                :description "Deletes a Proxmox VM via 'terraform destroy -auto-approve'."
                :input-schema
                '(("type" . "object")
                  ("properties" .
                   (("path" . (("type" . "string")))
                    ("terraform_bin" . (("type" . "string"))))))
                :handler #'tool-terraform-destroy-vm))
