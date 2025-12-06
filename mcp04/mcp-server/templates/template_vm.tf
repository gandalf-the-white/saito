locals {
  endpoint    = "~A"
  api_token   = "~A"
  publkeyctn  = "~A"
  target_node = "~A"
  cores       = "~A"
  memory      = "~A"
  storage     = "~A"
  image       = "~A"
  bridge      = "~A"
  vlan        = "~A"
  prefix      = "~A"
  octet       = "~A"
  domain      = "~A"
  nameserver  = "~A"
  name        = "~A"
  privkey     = "~A"
}


terraform {
  required_providers {
    proxmox = {
      source  = "bpg/proxmox"
      version = "0.88.0"
    }
  }
}

provider "proxmox" {
  endpoint  = local.endpoint
  api_token = local.api_token
  insecure  = true

  ssh {
    username = "root"
    # agent       = true
    private_key = file(local.privkey)
  }
}

data "local_file" "ssh_public_key" {
  filename = local.publkeyctn
}

resource "proxmox_virtual_environment_container" "oracle" {
  node_name = local.target_node
  # vm_id        = 2204 # optionnel, sinon ID auto
  unprivileged = true

  cpu {
    cores = local.cores
  }

  memory {
    dedicated = local.memory
    swap      = 512
  }

  # Disque root du CT
  disk {
    datastore_id = local.storage
    size         = 8
  }

  # Image template (stockée dans local:vztmpl/)
  operating_system {
    template_file_id = local.image
  }

  # Réseau + VLAN 200 (IP via DHCP)
  network_interface {
    name     = "eth0"
    bridge   = local.bridge
    vlan_id  = local.vlan
    firewall = false
  }

  initialization {
    hostname = local.name

    user_account {
      password = "laurent"
      keys = [
        "${trimspace(data.local_file.ssh_public_key.content)}"
      ]
    }

    ip_config {
      ipv4 {
        address = "${local.prefix}.${local.octet}/24"
        gateway = "${local.prefix}.1"
      }
    }

    dns {
      domain  = local.domain
      servers = [local.nameserver]
    }
  }

  features {
    nesting = true
  }

  start_on_boot = true
}
