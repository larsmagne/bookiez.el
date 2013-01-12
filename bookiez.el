;;; bookiez.el --- Managing Books
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: books

;; This file is not part of GNU Emacs.

;; dae.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; bookiez is a library for looking up ISBNs via the Google Book API,
;; displaying the result, and maintaining a database of books.

;;; Code:

;; (bookiez-display-isbn "1931520003")

(require 'json)

(defun bookiez-lookup-isbn (isbn)
  (let ((buffer (url-retrieve-synchronously
		 (format "https://www.googleapis.com/books/v1/volumes?q=ISBN%s"
			 isbn)))
	title author thumbnail date)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let* ((data (aref (cdr (assq 'items (json-read))) 0))
		 (volume (assq 'volumeInfo data)))
	    (setq title (cdr (assq 'title volume)))
	    (when (assq 'subtitle volume)
	      (setq title (concat title " ("
				  (cdr (assq 'subtitle volume)) ")")))
	    (setq author (mapconcat 'identity (cdr (assq 'authors volume))
				    ", "))
	    (setq date (cdr (assq 'publishedDate volume))
		  thumbnail (cdr (assq 'thumbnail
				       (cdr (assq 'imageLinks volume)))))))))
    (list title author date thumbnail)))

(defun bookiez-display-isbn (isbn)
  (destructuring-bind (title author date thumbnail) (bookiez-lookup-isbn isbn)
    (if (not title)
	(message "No match for %s" isbn)
      (pop-to-buffer "*isbn*")
      (erase-buffer)
      (insert author "\n" title "\n" date "\nISBN" isbn "\n\n")
      (when thumbnail
	(url-retrieve thumbnail 'bookiez-image-fetched
		      (list (current-buffer))
		      t t)))))

(defun bookiez-image-fetched (status buffer)
  (goto-char (point-min))
  (when (or (search-forward "\n\n" nil t)
	    (search-forward "\r\n\r\n" nil t))
    (let ((image (buffer-substring (point) (point-max))))
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert-image (create-image image nil t) "*")
	(goto-char (point-min))))))
