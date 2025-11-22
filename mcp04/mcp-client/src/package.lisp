(defpackage :mcp-client
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request)
  (:import-from :babel :octets-to-string)
  (:export
   ;; config
   :*ollama-url*
   :*ollama-model*
   :*mcp-url*
   :*agent-system-prompt*
   :*debug*

   ;; MCP HTTP
   :mcp-tools-list
   :mcp-tools-list-text
   :mcp-call-tool-text

   ;; Ollama + MCP agent
   :run-mcp-ollama-session
   :run-mcp-ollama-repl
   :main))

(in-package :mcp-client)

(defparameter *ollama-url* "http://localhost:11434/api/chat")
(defparameter *ollama-model* "llama3.2")

;; URL de ton serveur MCP HTTP (exposé avec Hunchentoot)
(defparameter *mcp-url* "http://localhost:8000/mcp")
