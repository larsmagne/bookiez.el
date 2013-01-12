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
(require 'cl)

(defvar bookiez-file "~/.emacs.d/bookiez.data")

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

(defun bookiez-display-isbn (isbn &optional save)
  (destructuring-bind (title author date thumbnail) (bookiez-lookup-isbn isbn)
    (if (not title)
	(message "No match for %s" isbn)
      (pop-to-buffer "*isbn*")
      (erase-buffer)
      (insert author "\n" title "\n" date "\nISBN" isbn "\n\n")
      (when thumbnail
	(url-retrieve thumbnail 'bookiez-image-fetched
		      (list (current-buffer))
		      t t))
      (bookiez-add-book author title isbn date thumbnail))))

(defun bookiez-image-fetched (status buffer)
  (goto-char (point-min))
  (when (or (search-forward "\n\n" nil t)
	    (search-forward "\r\n\r\n" nil t))
    (let ((image (buffer-substring (point) (point-max))))
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert-image (create-image image nil t) "*")
	(goto-char (point-min))))))

(defun bookiez-start-server ()
  (setq server-use-tcp t
	server-host (system-name)
	server-name "bookiez")
  (server-start))

(defvar bookiez-books nil)

(defun bookiez-add-book (author title isbn date thumbnail)
  (unless bookiez-books
    (bookiez-read-database))
  (let ((do-insert t))
    (loop for book in bookiez-books
	  when (or (equal isbn (nth 2 book))
		   (and (equal author (car book))
			(equal title (cadr book))))
	  do (message "%s/%s (%s) already exists in the database"
		      author title isbn)
	  (setq do-insert nil))
    (when do-insert
      (push (list author title isbn date
		  (format-time-string "%Y-%m-%d")
		  thumbnail)
	    bookiez-books)
      (bookiez-write-database))))

(defun bookiez-read-database ()
  (setq bookiez-books nil)
  (with-temp-buffer
    (insert-file-contents bookiez-file)
    (while (not (eobp))
      (push (split-string (buffer-substring (point) (line-end-position))
			  "\t")
	    bookiez-books)
      (forward-line 1))
    (setq bookiez-books (nreverse bookiez-books))))

(defun bookiez-write-database ()
  (with-temp-file bookiez-file
    (dolist (book bookiez-books)
      (insert (mapconcat (lambda (elem)
			   (subst-char-in-string ?\t ?  elem))
			 book
			 "\t")
	      "\n"))))

(provide 'bookiez)
