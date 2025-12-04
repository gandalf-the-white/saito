(in-package :mcp-server)

(defun tool-get-script-tf (params)
  "MCP tool handler for terraform-proxmox."
  (let* ((vm-name (gethash "vm_name" params "test-vm"))
         (cores (gethash "cores" params 8))
         (memory (gethash "memory" params "2048"))
         (disk-size (gethash "disk_size" params "30G"))
         (image (gethash "image" params "local:vztmpl/debian-12-standard_12.0-1_amd64.tar.zst"))
         (bridge (gethash "bridge" params "vmbr3"))
         (target-node (gethash "target_node" params "proxmox"))
         (path (format nil "./~A.tf" vm-name))
         (content (format nil
                          "terraform {
  required_providers {
    proxmox = {
      source  = \"Telmate/proxmox\"
      version = \"3.0.1-rc4\"
    }
  }
}

provider \"proxmox\" {
  pm_api_url      = \"https://proxmox.local:8006/api2/json\"
  pm_user         = \"root@pam\"
  pm_password     = \"changeme\"
  pm_tls_insecure = true
}

resource \"proxmox_vm_qemu\" \"~A\" {
  name         = \"~A\"
  target_node  = \"~A\"
  clone        = \"~A\"
  cores        = ~A
  memory       = ~A
  disk {
    size = \"~A\"
  }
  network {
    bridge = \"~A\"
  }
  os_type = \"cloud-init\"
}"
                          vm-name vm-name target-node image cores memory disk-size bridge)))
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out "~A" content))
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
          ((not (uiop::string-suffix-p ".tf" path))
           (format nil "The file ~A is not a terraform file (.tf)." path))
          (t
           (with-open-file (in path :direction :input :external-format :utf-8)
             (let* ((size (file-length in))
                    (to-read (min size max-size))
                    (content (make-string to-read)))
               (read-sequence content in)
               (when (> size max-size)
                 (setf content (concatenate 'string content
                                            "\n\n… [Tronqué : fichier > 100 Ko] …")))
               (format nil "📄 **Terraform file**: `~A`\n```hcl\n~A\n```" path content))))))
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
                   (("vm_name" .
                     (("type" . "string")
                      ("description" . "Name of the VM.")))
                    ("cores" .
                     (("type" . "integer")
                      ("description" . "CPU count.")))
                    ("memory" .
                     (("type" . "integer")
                      ("description" . "RAM in MB.")))
                    ("disk_size" .
                     (("type" . "string")
                      ("description" . "Disk size, e.g., 20G.")))
                    ("image" .
                     (("type" . "string")
                      ("description" . "Proxmox template or ISO name.")))
                    ("target_node" .
                     (("type" . "string")
                      ("description" . "Proxmox node name (default pve).")))
                    ("bridge" .
                     (("type" . "string")
                      ("description" . "Network bridge, e.g. vmbr0.")))))
                  )
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
