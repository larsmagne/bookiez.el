;;; openai.el --- Querying the Openai API -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; openai.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; Usage: Get an API key from openai.ai and:

;; (setq openai-key "API_KEY")
;;
;; Then query away:
;;
;; (openai-query "What is one plus two? ")
;; => "One plus two equals three."

;;; Code:

(require 'url)

(defvar openai-key nil
  "API key for Openai.")

(defun openai-query (query)
  "Send QUERY to Openai.ai.
`openai-key' has to be set to the API key first."
  (unless openai-key
    (error "`openai-key' is not set"))
  (let* ((message (openai--query query))
	 (error (cdr (assq 'error message))))
    (setq m message)
    (if (not (eq error :null))
	(error "Error: %s" (cdr (assq 'message error)))
      (cdr (assq 'text (elt (cdr (assq 'content
				       (elt (cdr (assq 'output message)) 0)))
			    0))))))

(defun openai--hash (&rest values)
  (cl-loop with table = (make-hash-table :test #'equal)
	   for (key value) in values
	   do (setf (gethash key table) value)
	   finally (return table)))

(defun openai--query (query)
  (let ((url-request-method "POST")
        (url-request-data
	 (encode-coding-string
	  (json-serialize
	   (openai--hash
	    (list "model" "gpt-4o")
	    (list "input" query)))
	  'utf-8))
        (url-request-extra-headers
         `(("Connection" . "close")
           ("Content-Type" ."application/json")
	   ("Accept" . "application/json")
	   ("Authorization" . ,(format "Bearer %s" openai-key)))))
    (with-current-buffer (url-retrieve-synchronously
			  "https://api.openai.com/v1/responses" t)
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (json-parse-buffer :object-type 'alist))
	(kill-buffer (current-buffer))))))

(provide 'openai)

;;; openai.el ends here.
