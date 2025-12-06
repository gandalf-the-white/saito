(defpackage mcp-server
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request)
  (:import-from :babel :octets-to-string)
  (:export
   :run-mcp-server
   :*ollama-url*
   :*ollama-model*))

(in-package :mcp-server)

(defun getenv-or (name default)
  (or (uiop:getenv name)
      default))

(defparameter *ollama-url* (getenv-or "OLLAMA_URL" "http://localhost:11434/api/chat"))

(defparameter *ollama-model* (getenv-or "OLLAMA_MODEL" "llama3.2"))

(defparameter *mcp-port* (getenv-or "MCP_PORT" 8000))

(defparameter *mcp-host* (getenv-or "MCP_HOST" "0.0.0.0"))

(defparameter *end-point* (uiop:getenv "TF_VAR_endpoint"))

(defparameter *api-token* (uiop:getenv "TF_VAR_api_token"))

(defparameter *token-id* (uiop:getenv "TF_VAR_token_id"))

(defparameter *token-secret* (uiop:getenv "TF_VAR_token"))

(defparameter *private-key* (uiop:getenv "TF_VAR_private_key"))

(defparameter *ctn-public-key* (uiop:getenv "TF_VAR_ctn_public_key"))
