(in-package :mcp-server)

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
         (key (hethash "key" params) *private-key*)
         (path (format nil "platform/~A.tf" name))
         (template (load-template "templates/template_vm.tf"))
         (content (format nil template
                          endpoint api_token publkeyctn target_node cores memory storage
                          image bridge vlan prefix octet domain nameserver name))
         (content (string-right-trim '(#\Space #\Newline #\Return #\Tab #\~) content))
         (content (coerce content 'string))
         (content (remove #\Nul content)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (dbg "Fichier ecrit: ~A (~A octets)" path (length content))
    (format nil "Terraform script generated at: ~A" path)))


(defun tool-view-script-tf (params)
  "Send back the terraform file (.tf)"
  (handler-case
      (let* ((path (gethash "path" params))
             (max-size (* 100 1024))) ;; 100 ko max
        (cond
          ((null path)
           (format nil "No path include. We need it: \"path\"."))
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
                 (read-sequence content in)
                 (when (> size max-size)
                   (setf content (concatenate 'string content
                                              "\n\n… [Truncated : fichier > 100 Ko] …")))
                 (format nil "**Terraform file**: `~A` ~%  `~A` ~%" path content)))))))
    (error (e)
      (format nil "Error reading file: ~A" e))))

(defun run-terraform-validate (path &key (terraform-bin "terraform"))
  "Validate script file (.tf)"
  (let* ((path-name (pathname path))
         (dir (make-pathname :directory (pathname-directory path-name))))
    ;; (format t "path: ~A pathname: ~A dir: ~A ~%" path path-name dir)
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
