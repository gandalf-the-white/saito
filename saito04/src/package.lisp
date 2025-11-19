(defpackage saito
  (:use :cl)
  (:import-from :jonathan :parse :to-json)
  (:import-from :drakma :http-request :url-encode)
  (:import-from :babel :octets-to-string))

(in-package :saito)

(defparameter *ollama-url* "http://localhost:11434/api/chat")
(defparameter *ollama-model* "llama3.2")
