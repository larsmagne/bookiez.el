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
;; Currently supported are Google Books, isbndb and
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
(require 'sgml-mode)
(require 'mm-url)

(defvar isbn-isbndb-key nil
  "To use the isbndb lookup, get an access key.")

(defvar isbn-google-key nil
  "If you do a lot of requests, put your key here to avoid rate limiting.")

(defvar isbn-librarything-key nil
  "The key for the LibraryThing API.")

(defvar isbn-lookup-types
  `(goodreads
    google
    openlibrary
    ,@(if isbn-isbndb-key '(isbndb)))
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
			   isbn result index isbn)
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

(defun isbn-format (isbn)
  (cond
   ;; 978-1-56619-909-4
   ((length= isbn 13)
    (format "%s-%s-%s-%s-%s"
	    (substring isbn 0 3)
	    (substring isbn 3 4)
	    (substring isbn 4 9)
	    (substring isbn 9 12)
	    (substring isbn 12)))
   ;; 0-321-57346-X
   ((length= isbn 10)
    (format "%s-%s-%s-%s"
	    (substring isbn 0 1)
	    (substring isbn 1 4)
	    (substring isbn 4 9)
	    (substring isbn 9)))
   (t
    isbn)))

(defun isbn-covers (isbn)
  "Return cover URLs for ISBN."
  (cl-loop for result in (isbn-lookup isbn t)
	   for cover = (nth 3 result)
	   when (cl-plusp (length cover))
	   collect cover))

(defun isbn-first-result (result)
  (let ((first
	 (cl-loop for elem across result
		  when (cdr elem)
		  return (cdr elem))))
    ;; Extend with genres from Goodreads, if any.
    (cl-loop for elem across result
	     when (nth 6 elem)
	     return (nconc first (list nil (nth 6 elem))))
    first))

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

(defun isbn--fetch-data (url &optional type)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (unwind-protect
	(and (search-forward "\n\n" nil t)
	     (cond
	      ((eq type 'xml)
	       (libxml-parse-xml-region (point) (point-max)))
	      ((eq type 'html)
	       (libxml-parse-html-region (point) (point-max)))
	      (t
	       (json-parse-buffer))))
      (kill-buffer (current-buffer)))))

;;; Google Books API

(defun isbn-lookup-google (isbn vector index find-isbn)
  (url-retrieve
   (format "https://www.googleapis.com/books/v1/volumes?q=%s%s"
	   isbn
	   (if isbn-google-key
	       (format "&key=%s" isbn-google-key)
	     ""))
   'isbn-parse-google (list vector index find-isbn) t))

(defun isbn-parse-google (_status vector index find-isbn)
  (goto-char (point-min))
  (let ((charset nil)
	json)
    (when (re-search-forward "^content-type.*charset=\\([-A-Z0-9]+\\)" nil t)
      (setq charset (match-string 1)))
    (when (search-forward "\n\n" nil t)
      (if (and charset
	       (equal (downcase charset) "utf-8"))
	  ;; Eh; there must be a better way to get the mojibake in
	  ;; the http buffer (which is multibyte!) decoded.
	  (let ((string (buffer-substring (point) (point-max))))
	    (delete-region (point) (point-max))
	    (with-temp-buffer
	      (set-buffer-multibyte nil)
	      (insert string)
	      (set-buffer-multibyte t)
	      (goto-char (point-min))
	      (setq json (cdr (assq 'items (json-read))))))
	(setq json (cdr (assq 'items (json-read)))))
      (cl-loop for data across (setq k json)
	       for volume = (assq 'volumeInfo data)
	       for isbn = (cl-loop for entry across
				   (cdr (assq 'industryIdentifiers volume))
				   for i = (cdr (assq 'identifier entry))
				   when (equal i find-isbn)
				   return i)
	       for title = (cdr (assq 'title volume))
	       when isbn
	       return
	       (let ((author (mapconcat 'identity (cdr (assq 'authors volume))
					", "))
		     (date (cdr (assq 'publishedDate volume)))
		     (thumbnail
		      (cdr (assq 'thumbnail
				 (cdr (assq 'imageLinks volume))))))
		 (when (assq 'subtitle volume)
		   (setq title (concat title " ("
				       (cdr (assq 'subtitle volume)) ")")))
		 (when (and title author)
		   (setcdr (aref vector index)
			   (list title author date thumbnail)))))
      (kill-buffer (current-buffer)))))

;;; OpenLibrary API

(defun isbn-lookup-openlibrary (isbn vector index find-isbn)
  (url-retrieve
   (format "http://openlibrary.org/api/books?bibkeys=ISBN:%s&format=json&jscmd=data"
	   isbn)
   'isbn-parse-openlibrary
   (list vector index find-isbn) t))

(defun isbn-parse-openlibrary (_status vector index _find-isbn)
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

(defun isbn-search-openlibrary (author)
  (cl-loop for work across
	   (gethash "docs"
		    (isbn--fetch-data
		     (format "https://openlibrary.org/search.json?author=%s&sort=new&fields=title,author_name,language,isbn,publish_date,key,number_of_pages_median,publish_year"
			     (browse-url-encode-url author))))
	   collect (list (seq-into (gethash "author_name" work) 'list)
			 (gethash "title" work)
			 (seq-into (gethash "isbn" work) 'list)
			 (elt (gethash "publish_year" work) 0)
			 (let ((pages (gethash "number_of_pages_median" work)))
			   (if pages
			       (format "%s pages" pages)
			     "")))))

;;; ISBNdb API.

(defun isbn-lookup-isbndb (isbn vector index _find-isbn)
  (let ((url-request-extra-headers
         `(("Authorization" . ,isbn-isbndb-key))))
    (url-retrieve
     (format "https://api2.isbndb.com/book/%s" isbn)
     (lambda (_)
       (goto-char (point-min))
       (unwind-protect
	   (and (search-forward "\n\n" nil t)
		(when-let* ((json (json-parse-buffer))
			    (book (gethash "book" json)))
		  (setcdr (aref vector index)
			  (list (string-join
				 (cl-loop for author across
					  (gethash "authors" book)
					  collect author)
				 ", ")
				(gethash "title" book)
				(gethash "date_published" book)
				(gethash "image" book)))))
	 (kill-buffer (current-buffer)))))))

(defun isbn-author-isbndb (author)
  (let ((url-request-extra-headers
         `(("Authorization" . ,isbn-isbndb-key))))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "https://api2.isbndb.com/author/%s?pageSize=2000" author))
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (json-parse-buffer))
	(kill-buffer (current-buffer))))))

;;; Goodreads search.

(defvar isbn-ignored-genres '("Fiction" "Audiobook" "Nonfiction"
			      "Short Stories" "Novels")
  "Too-general genres to be ignored.")

(defun isbn-lookup-goodreads (isbn vector index _find-isbn)
  (let ((dummy (get-buffer-create " *goodreads*")))
    (url-retrieve
     (format "https://www.goodreads.com/search?q=%s&qid=" isbn)
     (lambda (_)
       (goto-char (point-min))
       (when (search-forward "\n\n" nil t)
	 (let ((dom (libxml-parse-html-region (point) (point-max))))
	   (cl-loop
	    for elem in (dom-by-tag dom 'script)
	    when (equal (dom-attr elem 'type) "application/ld+json")
	    return
	    (let ((json (json-parse-string (dom-text elem))))
	      (setcdr
	       (aref vector index)
	       (list
		(string-clean-whitespace
		 (isbn--decode-html-entities (gethash "name" json)))
		(string-clean-whitespace
		 (isbn--decode-html-entities
		  (gethash "name" (elt (gethash "author" json) 0))))
		(cl-loop for p in (dom-by-tag dom 'p)
			 when (equal (dom-attr p 'data-testid)
				     "publicationInfo")
			 return
			 (format-time-string
			  "%Y-%m-%d" (encode-time
				      (decoded-time-set-defaults
				       (parse-time-string (dom-text p))))))
		(gethash "image" json)
		;; Also add an extra slot for genres.
		nil ;; ISBN in the Google result?
		(cl-loop for span in
			 (dom-by-class
			  dom "BookPageMetadataSection__genreButton")
			 for genre = (dom-texts span)
			 unless (member genre isbn-ignored-genres)
			 collect genre)))))))
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

;; LibraryThing API.

(defun isbn-search-librarything (string)
  (let ((json (isbn--fetch-data
	       (format
		"https://www.librarything.com/api/talpa.php?search=%s&token=%s&limit=50"
		(browse-url-encode-url string)
		isbn-librarything-key))))
    (cl-loop for work across (gethash "resultlist" (gethash "response" json))
	     collect (list (gethash "title" work)
			   (gethash "isbns" work)))))

(defun isbn-isbns-librarything (isbn)
  "Return other ISBNs that ISBN has been published under."
  (cl-loop for elem in
	   (dom-by-tag
	    (isbn--fetch-data
	     (format "https://www.librarything.com/api/%s/thingISBN/%s"
		     isbn-librarything-key isbn)
	     'xml)
	    'isbn)
	   collect (nth 2 elem)))

;;; Decoding.

(defun isbn--decode-html-entities (string)
  (let ((mm-url-html-entities
	 (cl-loop for name across sgml-char-names
		  for char from 0
		  when name
		  collect (cons (intern name) char))))
    (mm-url-decode-entities-string string)))

(provide 'isbn)

;;; isbn.el ends here.
