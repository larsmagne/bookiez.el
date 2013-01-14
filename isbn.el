;;; isbn.el --- Looking up ISBN numbers from various sources
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: books

;; isbn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; isbn.el is a library for looking up ISBNs via various sources.
;; Currently supported are Google Books, LibraryThing, isbndb and
;; OpenLibrary.

;; Usage:
;; (isbn-lookup "1931520003")
;; => ("Stranger Things Happen (Stories)" "Kelly Link" "2001-07-01" "http://bks7.books.google.com/books?id=55lkEsblJ1gC&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api")

;;; Code:

(require 'json)
(require 'cl)

(defvar isbn-isbndb-key nil
  "To use the isbndb lookup, get a developer key.")

(defvar isbn-librarything-key nil
  "To use the LibraryThing lookup, get a developer key.")

(defun isbn-lookup-google (isbn)
  (let ((buffer (url-retrieve-synchronously
		 (format "https://www.googleapis.com/books/v1/volumes?q=ISBN%s"
			 isbn)))
	title author thumbnail date)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let* ((main-data (cdr (assq 'items (json-read))))
		 (data (and main-data (aref main-data 0)))
		 (volume (assq 'volumeInfo data)))
	    (setq title (cdr (assq 'title volume)))
	    (when (assq 'subtitle volume)
	      (setq title (concat title " ("
				  (cdr (assq 'subtitle volume)) ")")))
	    (setq author (mapconcat 'identity (cdr (assq 'authors volume))
				    ", "))
	    (setq date (cdr (assq 'publishedDate volume))
		  thumbnail (cdr (assq 'thumbnail
				       (cdr (assq 'imageLinks volume)))))))
	(kill-buffer (current-buffer))))
    (and title
	 (list title author date thumbnail))))

(defun isbn-lookup-isbndb (isbn)
  (let ((buffer (url-retrieve-synchronously
		 (format "http://isbndb.com/api/books.xml?access_key=%s&results=details&index1=isbn&value1=%s"
			 isbn-isbndb-key
			 isbn)))
	title author thumbnail date)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let* ((data (libxml-parse-xml-region (point) (point-max)))
		 (entry (assq 'BookData (assq 'BookList (cdr data)))))
	    (and (nth 2 (assq 'Title entry))
		 (list (nth 2 (assq 'Title entry))
		       (isbn-isbndb-author (nth 2 (assq 'AuthorsText entry)))
		       (isbn-isbndb-date
			(cdr (assq 'edition_info (cadr (assq 'Details entry)))))
		       nil))))))))

(defun isbn-isbndb-date (string)
  ;; The edition info looks like "Paperback; 1986-11-01".
  (when (and string
	     (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" string))
    (match-string 0 string)))

(defun isbn-isbndb-author (string)
  (when string
    (setq string (replace-regexp-in-string ", $" "" string)))
  string)

(defun isbn-lookup-openlibrary (isbn)
  (let ((buffer (url-retrieve-synchronously
		 (format "http://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data"
			 isbn)))
	title author thumbnail date)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let ((data (cdar (json-read))))
	    (when data
	      (setq title (cdr (assq 'title data)))
	      (setq author (mapconcat
			    (lambda (elem)
			      (cdr (assq 'name elem)))
			    (cdr (assq 'authors data))
			    ", "))
	      (setq date (format-time-string
			  "%Y-%m-%d"
			  (apply 'encode-time
				 (mapcar
				  (lambda (elem)
				    (or elem 0))
				  (parse-time-string
				   (cdr (assq 'publish_date data))))))
		    thumbnail (cdr (assq 'large
					 (cdr (assq 'cover data))))))))
	(kill-buffer (current-buffer))))
    (and title
	 (list title author date thumbnail))))

(defun isbn-lookup-librarything (isbn)
  (let ((buffer (url-retrieve-synchronously
		 (format "http://www.librarything.com/services/rest/1.1/?method=librarything.ck.getwork&isbn=%s&apikey=%s"
			 isbn
			 isbn-librarything-key)))
	title author thumbnail date)
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let* ((data (libxml-parse-xml-region (point) (point-max)))
		 (entry (assq 'item (assq 'ltml (cdr data)))))
	    (and (nth 2 (assq 'title entry))
		 (list (nth 2 (assq 'title entry))
		       (nth 2 (assq 'author entry))
		       "1970-01-01"
		       nil))))))))

(defun isbn-lookup (isbn)
  (or (isbn-lookup-google isbn)
      (isbn-lookup-openlibrary isbn)
      (and isbn-isbndb-key
	   (isbn-lookup-isbndb isbn))
      (and isbn-librarything-key
	   (isbn-lookup-librarything isbn))
      (list nil nil nil nil)))

(provide 'isbn)
