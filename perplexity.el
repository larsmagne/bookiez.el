;;; perplexity.el --- Querying the Perplexity API -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; perplexity.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; Usage: Get an API key from perplexity.ai and:

;; (setq perplexity-key "API_KEY")
;;
;; Then query away:
;;
;; (perplexity-query "What is one plus two? ")
;; => "One plus two equals three."

;;; Code:

(require 'url)

(defvar perplexity-key nil
  "API key for Perplexity.")

(defun perplexity-query (query)
  "Send QUERY to Perplexity.ai.
`perplexity-key' has to be set to the API key first."
  (unless perplexity-key
    (error "`perplexity-key' is not set"))
  (gethash "content"
	   (gethash "message"
		    (elt (gethash "choices" (perplexity--query query)) 0))))

(defun perplexity--hash (&rest values)
  (cl-loop with table = (make-hash-table :test #'equal)
	   for (key value) in values
	   do (setf (gethash key table) value)
	   finally (return table)))

(defun perplexity--query (query)
  (let ((url-request-method "POST")
        (url-request-data
	 (encode-coding-string
	  (json-serialize
	   (perplexity--hash
	    (list "model" "sonar-pro")
	    (list "messages"
		  (vector
		   (perplexity--hash
		    (list "role" "system")
		    (list "web_search_options"
			  (perplexity--hash
			   (list "search_context_size" "high")))
		    (list "content" "Be precise and concise.  Do not include any emphasis, references or comments."))
		   (perplexity--hash
		    (list "role" "user")
		    (list "content" query))))))
	  'utf-8))
        (url-request-extra-headers
         `(("Connection" . "close")
           ("Content-Type" ."application/json")
	   ("Accept" . "application/json")
	   ("Authorization" . ,(format "Bearer %s" perplexity-key)))))
    (with-current-buffer (url-retrieve-synchronously
			  "https://api.perplexity.ai/chat/completions" t)
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (json-parse-buffer))
	(kill-buffer (current-buffer))))))

(provide 'perplexity)

;;; perplexity.el ends here.
