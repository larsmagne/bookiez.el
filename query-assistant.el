;;; query-assistant.el --- Querying the LLM APIs -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; query-assistant.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; Usage: Get an API key from query-assistant.ai and:

;; (setq query-assistant-gemini-key "API_KEY")
;; and so on.
;;
;; Then query away:
;;
;; (query-assistant 'gemini "What is one plus two? ")
;; => "One plus two equals three."

;;; Code:

(require 'url)

(defvar query-assistant-gemini-key nil
  "The key to the Gemini assistant.")

(defvar query-assistant-openai-key nil
  "The key to the OpenAI assistant.")

(defvar query-assistant-perplexity-key nil
  "The key to the Perplexity assistant.")

(defun query-assistant (assistant query)
  (cl-destructuring-bind (url headers data parser)
      (funcall (intern (format "query-assistant--backend-%s" assistant))
	       query)
    (let* ((url-request-method "POST")
           (url-request-data
	    (encode-coding-string (json-serialize data) 'utf-8))
           (url-request-extra-headers
            `(("Connection" . "close")
              ("Content-Type" ."application/json")
	      ("Accept" . "application/json")
	      ,@headers)))
      (funcall
       parser
       (with-current-buffer (url-retrieve-synchronously url t)
	 (goto-char (point-min))
	 (unwind-protect
	     (and (search-forward "\n\n" nil t)
		  (json-parse-buffer))
	   (kill-buffer (current-buffer))))))))

(defun query-assistant--hash (&rest values)
  (cl-loop with table = (make-hash-table :test #'equal)
	   for (key value) in values
	   do (setf (gethash key table) value)
	   finally (return table)))

(defun query-assistant--backend-gemini (query)
  (list
   (concat "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key="
	   query-assistant-gemini-key)
   nil
   (query-assistant--hash
    (list "contents"
	  (vector (query-assistant--hash
		   (list "parts" (if (stringp query)
				     (vector (query-assistant--hash
					      (list "text" query)))
				   query))))))
   (lambda (message)
     (let ((error (gethash "error" message)))
       (if (and error
		(not (eq error :null)))
	   (error "Error: %s" (gethash "message" error))
	 (gethash "text"
		  (elt (gethash "parts"
				(gethash "content"
					 (elt (gethash "candidates" message) 0)))
		       0)))))))

(defun query-assistant--backend-perplexity (query)
  (list
   "https://api.perplexity.ai/chat/completions"
   `(("Authorization" . ,(format "Bearer %s" query-assistant-perplexity-key)))
   (query-assistant--hash
    (list "model" "sonar-pro")
    (list "messages"
	  (vector
	   (query-assistant--hash
	    (list "role" "system")
	    (list "web_search_options"
		  (query-assistant--hash
		   (list "search_context_size" "high")))
	    (list "content" "Be precise and concise.  Do not include any emphasis, references or comments."))
	   (query-assistant--hash
	    (list "role" "user")
	    (list "content" query)))))
   (lambda (message)
     (let ((data (elt (gethash "choices" message) 0)))
       (unless data
	 (error "Error: %s" (gethash "message" (gethash "error" message))))
       (gethash "content" (gethash "message" data))))))

(defun query-assistant--backend-openai (query)
  (list
   "https://api.openai.com/v1/responses"
   `(("Authorization" . ,(format "Bearer %s" query-assistant-openai-key)))
   (query-assistant--hash
    (list "model" "gpt-4.1-mini")
    (list "input" query))
   (lambda (message)
     (let ((error (gethash "error" message)))
       (if (not (eq error :null))
	   (error "Error: %s" (gethash "message" error))
	 (cl-loop for elem across (gethash "output" message)
		  for content = (elt (gethash "content" elem) 0)
		  when content
		  return (gethash "text" content)))))))

(provide 'query-assistant)

;;; query-assistant.el ends here.
