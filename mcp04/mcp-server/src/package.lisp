(defpackage mcp-server
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request)
  (:import-from :babel :octets-to-string)
  (:export :run-mcp-server :*ollama-url*
           :*ollama-model*))

(in-package :mcp-server)

(defparameter *ollama-url* "http://localhost:11434/api/chat")
(defparameter *ollama-model* "llama3.2")
