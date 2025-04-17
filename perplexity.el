;;; perplexity.el --- Querying the Perplexity API -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; perplexity.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;;; Code:

(require 'url)

(defvar perplexity-key nil
  "API key for Perplexity.")

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
	    (list "model" "sonar")
	    (list "messages"
		  (vector
		   (perplexity--hash
		    (list "role" "system")
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
			  "https://api.perplexity.ai/chat/completions")
      (goto-char (point-min))
      (prog1
	  (and (search-forward "\n\n" nil t)
	       (json-parse-buffer))
	(kill-buffer (current-buffer))))))

(defun perplexity-query (query)
  (let ((result (perplexity--query query)))
    (gethash "content" (gethash "message"
				(elt (gethash "choices" result) 0)))))

(provide 'perplexity)

;;; perplexity.el ends here.
