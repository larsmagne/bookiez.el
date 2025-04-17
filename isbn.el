;;; isbn.el --- Looking up ISBN numbers from various sources -*- lexical-binding: t; -*-
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
;; OpenLibrary.  Some of the methods requires getting a developer key.

;; Usage:
;; (isbn-lookup "1931520003")
;; => ("Stranger Things Happen (Stories)" "Kelly Link" "2001-07-01" "http://bks7.books.google.com/books?id=55lkEsblJ1gC&printsec=frontcover&img=1&zoom=1&edge=curl&source=gbs_api")

;;; Code:

(require 'json)
(require 'dom)
(require 'shr)
(require 'browse-url)
(require 'time-date)

(defvar isbn-librarything-key nil
  "To use the LibraryThing lookup, get a developer key.")

(defvar isbn-google-key nil
  "If you do a lot of requests, put your key here to avoid rate limiting.")

(defvar isbn-lookup-types
  `(goodreads
    google
    openlibrary
    ,@(if isbn-librarything-key '(librarything)))
  "List of lookup engines to use, and the order to look up ISBNs in.
The data sources to be preferred is listed towards the front of
the list.")

;; General interface.

(defun isbn-lookup (isbn &optional all-results)
  "Return a list of author/title/year/thumbnail for ISBN.
If ALL-RESULTS, return the results from all providors."
  (let ((result (make-vector (length isbn-lookup-types) nil))
	(index 0))
    ;; The idea here is that we ask all the different APIs in
    ;; parallel.
    (dolist (type isbn-lookup-types)
      (aset result index
	    (cons (funcall (intern (format "isbn-lookup-%s" type))
			   isbn result index)
		  nil))
      (cl-incf index))
    ;; Then we exit when we've got all the results (but don't wait
    ;; more than 20 seconds).
    (cl-loop repeat 200
	     while (isbn-first-living-buffer result)
	     do (accept-process-output nil nil 100))
    (if all-results
	(mapcar #'cdr result)
      (isbn-first-result result))))

(defun isbn-covers (isbn)
  "Return cover URLs for ISBN."
  (cl-loop for result in (isbn-lookup isbn t)
	   for cover = (nth 3 result)
	   when cover
	   collect cover))

(defun isbn-first-result (result)
  (cl-loop for elem across result
	   when (cdr elem)
	   return (cdr elem)))

(defun isbn-first-living-buffer (result)
  (cl-loop for elem across result
	   when (buffer-live-p (car elem))
	   return (car elem)))

(defun isbn-compute (string)
  (let ((checksum
	 (- 11
	    (mod
	     (cl-loop for i from 10 downto 2
		      for char across string
		      summing (* (- char ?0) i))
	     11))))
    (concat string (cond
		    ((= checksum 10) "X")
		    ((= checksum 11) "0")
		    (t (char-to-string (+ ?0 checksum)))))))

(defun isbn-valid-p (isbn)
  ;; If we have an EAN that contains the ISBN, then chop off the EAN
  ;; stuff and recompute the ISBN.
  (when (and (= (length isbn) 13)
	     (not (string-match "^978" isbn))) ; ISBN-13
    (setq isbn (isbn-compute (substring isbn 3 12))))
  (or (= (length isbn) 13)
      (and (= (length isbn) 10)
	   (equal isbn (isbn-compute (substring isbn 0 9))))))

;;; Google Books API

(defun isbn-lookup-google (isbn vector index)
  (url-retrieve
   (format "https://www.googleapis.com/books/v1/volumes?q=%s%s"
	   isbn
	   (if isbn-google-key
	       (format "&key=%s" isbn-google-key)
	     ""))
   'isbn-parse-google (list vector index) t))

(defun isbn-parse-google (_status vector index)
  (goto-char (point-min))
  (when (search-forward "\n\n" nil t)
    (let* ((main-data (cdr (assq 'items (json-read))))
	   (data (and main-data (aref main-data 0)))
	   (volume (assq 'volumeInfo data))
	   (isbn (cl-loop for entry across
			  (cdr (assq 'industryIdentifiers volume))
			  return (cdr (assq 'identifier entry))))
	   title author thumbnail date)
      (setq title (cdr (assq 'title volume)))
      (when (assq 'subtitle volume)
	(setq title (concat title " ("
			    (cdr (assq 'subtitle volume)) ")")))
      (setq author (mapconcat 'identity (cdr (assq 'authors volume))
			      ", "))
      (setq date (cdr (assq 'publishedDate volume))
	    thumbnail (cdr (assq 'thumbnail
				 (cdr (assq 'imageLinks volume)))))
      (when (and title author)
	(setcdr (aref vector index)
		(list title author date thumbnail isbn)))))
  (kill-buffer (current-buffer)))

;;; OpenLibrary API

(defun isbn-lookup-openlibrary (isbn vector index)
  (url-retrieve
   (format "http://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data"
	   isbn)
   'isbn-parse-openlibrary
   (list vector index) t))

(defun isbn-parse-openlibrary (_status vector index)
  (goto-char (point-min))
  (when (search-forward "\n\n" nil t)
    (let ((data (cdar (json-read)))
	  title author date thumbnail)
      (when data
	(setq title (cdr (assq 'title data)))
	(when (cdr (assq 'authors data))
	  (setq author (mapconcat
			(lambda (elem)
			  (cdr (assq 'name elem)))
			(cdr (assq 'authors data))
			", ")))
	(setq date (format-time-string
		    "%Y-%m-%d"
		    (and (cdr (assq 'publish_date data))
			 (apply 'encode-time
				(mapcar
				 (lambda (elem)
				   (or elem 0))
				 (parse-time-string
				  (cdr (assq 'publish_date data)))))))
	      thumbnail (cdr (assq 'large
				   (cdr (assq 'cover data)))))
	(when (and title author)
	  (setcdr (aref vector index)
		  (list title author date thumbnail))))))
  (kill-buffer (current-buffer)))

;;; LibraryThing API

(defun isbn-lookup-librarything (isbn vector index)
  (url-retrieve
   (format "http://www.librarything.com/services/rest/1.1/?method=librarything.ck.getwork&isbn=%s&apikey=%s"
	   isbn
	   isbn-librarything-key)
   'isbn-parse-librarything
   (list vector index) t))

(defun isbn-parse-librarything (_status vector index)
  (goto-char (point-min))
  (when (search-forward "\n\n" nil t)
    (let* ((data (libxml-parse-xml-region (point) (point-max)))
	   (entry (assq 'item (assq 'ltml (cdr data)))))
      (when (and (nth 2 (assq 'title entry))
		 (nth 2 (assq 'author entry)))
	(setcdr (aref vector index)
		(list (nth 2 (assq 'title entry))
		      (nth 2 (assq 'author entry))
		      "1970-01-01"
		      nil)))))
  (kill-buffer (current-buffer)))

;;; Goodreads search.

(defun isbn-lookup-goodreads (isbn vector index)
  (let ((dummy (get-buffer-create " *goodreads*")))
    (url-retrieve
     (format "https://www.goodreads.com/search?q=%s&qid=" isbn)
     (lambda (_)
       (goto-char (point-min))
       (when (search-forward "\n\n" nil t)
	 (let ((dom (libxml-parse-html-region (point) (point-max))))
	   (cl-loop for elem in (dom-by-tag dom 'script)
		    when (equal (dom-attr elem 'type) "application/ld+json")
		    return
		    (let ((json (json-parse-string (dom-text elem))))
		      (setcdr (aref vector index)
			      (list
			       (gethash "name" json)
			       (gethash "name" (elt (gethash "author" json) 0))
			       (cl-loop for p in (dom-by-tag dom 'p)
					when (equal (dom-attr p 'data-testid)
						    "publicationInfo")
					return
					(format-time-string
					 "%Y-%m-%d" (encode-time
						     (decoded-time-set-defaults
						      (parse-time-string
						       (dom-text p))))))
			       (gethash "image" json)))))))
       (kill-buffer (current-buffer))
       ;; We use the dummy buffer as the synchronising thing with
       ;; `isbn-lookup' because Goodreads will redirect us to a
       ;; different buffer and then kill the original buffer.  Very
       ;; confusing.
       (kill-buffer dummy))
     nil t t)
    dummy))

(defun isbn-search-goodreads (string)
  (cl-loop with data
	   for candidate in (isbn-search-goodreads-1 string)
	   when (and (y-or-n-p (format "Is it %s - %s? "
				       (caar candidate)
				       (caadr candidate)))
		     (setq data (isbn-try-candidate
				 (shr-expand-url
				  (cdar candidate)
				  "https://www.goodreads.com/"))))
	   return data))

(defun isbn-try-candidate (url)
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously url)
	   (goto-char (point-min))
	   (prog1
	       (and (search-forward "\n\n" nil t)
		    (libxml-parse-html-region (point) (point-max)))
	     (kill-buffer (current-buffer))))))
    (cl-loop for elem in (dom-by-tag dom 'script)
	     when (equal (dom-attr elem 'type) "application/ld+json")
	     return (let ((json (json-parse-string (dom-text elem))))
		      (and (gethash "isbn" json)
			   (list (gethash "isbn" json)
				 (gethash "image" json)))))))

(defun isbn-search-goodreads-1 (string)
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously
			       (format
				"https://www.goodreads.com/search?q=%s&qid="
				(browse-url-encode-url string)))
	   (goto-char (point-min))
	   (prog1
	       (and (search-forward "\n\n" nil t)
		    (libxml-parse-html-region (point) (point-max)))
	     (kill-buffer (current-buffer))))))
    (cl-loop for book in (dom-by-tag dom 'tr)
	     when (equal (dom-attr book 'itemtype) "http://schema.org/Book")
	     collect (cl-loop for link in (dom-by-tag book 'a)
			      when (member (dom-attr link 'class)
					   '("authorName" "bookTitle"))
			      collect (cons (string-clean-whitespace
					     (dom-texts link))
					    (dom-attr link 'href))))))

(provide 'isbn)

;;; isbn.el ends here.
