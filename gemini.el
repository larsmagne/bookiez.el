;;; gemini.el --- Querying the Gemini API -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; gemini.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; Usage: Get an API key from gemini.ai and:

;; (setq gemini-key "API_KEY")
;;
;; Then query away:
;;
;; (gemini-query "What is one plus two? ")
;; => "One plus two equals three."

;;; Code:

(require 'url)

(defvar gemini-key nil
  "API key for Gemini.")

(defun gemini-query (query)
  "Send QUERY to Gemini.ai.
`gemini-key' has to be set to the API key first."
  (unless gemini-key
    (error "`gemini-key' is not set"))
  (let* ((message (gemini--query query))
	 (error (cdr (assq 'error message))))
    (if (and error
	     (not (eq error :null)))
	(error "Error: %s" (cdr (assq 'message error)))
      (cdr
       (assq
	'text
	(elt (cdr (assq 'parts
			(cdr (assq 'content
				   (elt (cdr (assq 'candidates message)) 0)))))
	     0))))))

(defun gemini--hash (&rest values)
  (cl-loop with table = (make-hash-table :test #'equal)
	   for (key value) in values
	   do (setf (gethash key table) value)
	   finally (return table)))

(defun gemini--query (query)
  (let ((url-request-method "POST")
        (url-request-data
	 (encode-coding-string
	  (json-serialize
	   (gemini--hash
	    (list "contents"
		  (vector
		   (gemini--hash
		    (list "parts"
			  (vector
			   (gemini--hash
			    (list "text" query)))))))))
	  'utf-8))
        (url-request-extra-headers
         `(("Connection" . "close")
           ("Content-Type" ."application/json")
	   ("Accept" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously
			  (concat "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=" gemini-key)
			  t)
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (json-parse-buffer :object-type 'alist))
	(kill-buffer (current-buffer))))))

(provide 'gemini)

;;; gemini.el ends here.
