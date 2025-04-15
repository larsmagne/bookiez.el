;;; bookiez.el --- Managing Books  -*- lexical-binding: t; -*-
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: books

;; bookiez.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; bookiez is a library for looking up ISBNs via the Google Book API,
;; displaying the result, and maintaining a database of books.

;;; Code:

;; (bookiez-display-isbn "9780307596888")

(require 'isbn)
(require 'vtable)
(require 'svg)

(defvar bookiez-file "~/.emacs.d/bookiez.data")

(defun bookiez-thumbnail (thumbnail isbn)
  (if (and thumbnail
	   (plusp (length thumbnail)))
      thumbnail
    (format "http://covers.librarything.com/devkey/%s/large/isbn/%s"
	    isbn-librarything-key isbn)))

(setq bookiez-last-isbn nil)

(defun bookiez-display-isbn (isbn &optional save)
  (when save
    (message "Querying %s" isbn)
    (bookiez-play "71-On the Beach .mp3"))
  ;; If we have an EAN that contains the ISBN, then chop off the EAN
  ;; stuff and recompute the ISBN.
  (when (and (= (length isbn) 13)
	     (not (string-match "^978" isbn))) ; ISBN-13
    (setq isbn (isbn-compute (substring isbn 3 12))))
  (setq bookiez-last-isbn isbn)
  (if (or (= (length isbn) 13)
	  (and (= (length isbn) 10)
	       (equal isbn (isbn-compute (substring isbn 0 9)))))
      (bookiez-display-isbn-1 isbn save)
    ;; If the ISBN is totally invalid, say so before querying.
    (message "Invalid ISBN %s" isbn)
    (bookiez-play "74-kaffe matthews - still striped .mp3")))

(define-derived-mode bookiez-book-mode special-mode "Bookiez"
  "Mode to display a book.")

(defvar-keymap bookiez-book-mode-map
  "c" #'bookiez-book-edit)

(defvar bookiez-book-isbn nil)

(defun bookiez-display-isbn-1 (isbn &optional save)
  (cl-destructuring-bind (title author date thumbnail)
      (or (bookiez-lookup isbn)
	  (isbn-lookup isbn)
	  '(nil nil nil nil))
    (setq date (or date "1970-01-01"))
    (if (not title)
	(progn
	  (message "No match for %s" isbn)
	  (bookiez-play "45-VENOZ TKS - Carry On Sergeant. Right Oh, Sir!.mp3"))
      (switch-to-buffer "*Bookiez Book*")
      (let ((inhibit-read-only t))
	(erase-buffer)
	(bookiez-book-mode)
	(setq-local bookiez-book-isbn isbn)
	(insert author "\n" title "\n" date "\nISBN" isbn "\n\n")
	(let ((file (expand-file-name (format "%s.jpg" isbn)
				      "~/.emacs.d/bookiez-cache/")))
	  (if (file-exists-p file)
	      (progn
		(insert-image (create-image file))
		(insert "\n"))
	    (url-retrieve (bookiez-thumbnail thumbnail isbn)
			  'bookiez-image-fetched
			  (list (current-buffer) (point))
			  t)))
	(goto-char (point-min))
	(setq bookiez-last-isbn nil)
	(bookiez-play "61-KREVmorse .mp3")
	(when save
	  (bookiez-add-book author title isbn date thumbnail
			    (eq save 'ebook)
			    nil)
	  (bookiez-cache-image isbn thumbnail))))))

(defun bookiez-book-edit ()
  "Edit the book data in the current buffer."
  (interactive)
  (let ((isbn bookiez-book-isbn))
    (cl-loop for book in bookiez-books
	     when (equal isbn (nth 2 book))
	     do (let ((author (read-string "New author name: " (nth 0 book)))
		      (title (read-string "New book title: " (nth 1 book))))
		  (setf (car book) author)
		  (setf (cadr book) title)))
    (bookiez-write-database)
    (bookiez-display-isbn-1 isbn)))

(defun bookiez-lookup (isbn)
  (cl-loop for elem in bookiez-books
	   when (equal isbn (nth 2 elem))
	   return (list (nth 1 elem) (nth 0 elem)
			(nth 3 elem) (nth 5 elem))))

(defun bookiez-play (file)
  (call-process "amixer" nil nil nil "-c" "0" "set" "Speaker" "100%")
  (start-process
   "*mpg*" (get-buffer-create "*mpg123*")
   "mpg123"
   "-a" "hw:0"
   ;;"-f" "1000"
   "-n" "10"
   (expand-file-name file "/music/repository/Various/Ringtones")))

(defun bookiez-add-ebook-manually ()
  (interactive)
  (bookiez-add-book-manually t))

(defvar bookiez-author-history nil)

(defvar bookiez--unknown-isbn -4000)

(defun bookiez-add-book-manually (&optional ebook)
  (interactive)
  (let ((author (read-string "Author: " nil 'bookiez-author-history))
	(title (read-string "Title: "))
	(date "1970-01-01")
	(isbn bookiez-last-isbn)
	(thumb nil))
    (unless isbn
      (when-let ((match (isbn-lookup (concat author " " title))))
	(when (y-or-n-p (format "Is this %s? "
				(car match)))
	  (setq date (nth 2 match)
		isbn (nth 4 match)
		thumb (nth 3 match)))))
    (unless isbn
      (setq isbn (read-string "ISBN: ")))
    (when (zerop (length isbn))
      (setq isbn (format "%s" (cl-decf bookiez--unknown-isbn))))
    (bookiez-add-book author title isbn date thumb ebook
		      (y-or-n-p "Book read? "))
    (setq bookiez-last-isbn nil)))

(defun bookiez-image-fetched (_status buffer point)
  (goto-char (point-min))
  (when (or (search-forward "\n\n" nil t)
	    (search-forward "\r\n\r\n" nil t))
    (let ((image (buffer-substring (point) (point-max))))
      (kill-buffer (current-buffer))
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
	  (when-let ((im (create-image image nil t)))
	    (save-excursion
	      (goto-char point)
	      (insert-image im "*"))))))))

(defun bookiez-start-server ()
  (setq server-use-tcp t
	server-host (if (equal (system-name) "cat")
			"fw"
		      (system-name))
	server-name "bookiez")
  (server-start))

(defvar bookiez-books nil)

(defun bookiez-add-book (author title isbn date thumbnail ebook
				read)
  (bookiez--possibly-read-database)
  (let ((do-insert t)
	(update-read t))
    (cl-loop for book in bookiez-books
	     when (or (equal isbn (nth 2 book))
		      (and (equal author (car book))
			   (equal title (cadr book))))
	     do (message "%s/%s (%s) already exists in the database%s"
			 author title isbn
			 (if (setq update-read book)
			     "; marking as read"
			   ""))
	     (setq do-insert nil))
    (cond
     (do-insert
      (push (list author title isbn date
		  (format-time-string "%Y-%m-%d")
		  thumbnail
		  (if ebook "ebook" "paper")
		  (if read "" "unread"))
	    bookiez-books)
      (bookiez-write-database))
     (update-read
      (setcdr (nthcdr 6 update-read)
	      (delete "unread" (nthcdr 7 update-read)))
      (nconc update-read (list (concat "read:"
				       (format-time-string "%Y-%m-%d"))))
      (bookiez-write-database)))))

(defvar bookiez--database-timestamp nil)

(defun bookiez--possibly-read-database ()
  (when (or (null bookiez-books)
	    (null bookiez--database-timestamp)
	    (time-less-p bookiez--database-timestamp
			 (file-attribute-modification-time
			  (file-attributes bookiez-file))))
    (bookiez-read-database)
    (setq bookiez--database-timestamp
	  (file-attribute-modification-time
	   (file-attributes bookiez-file)))))

(defun bookiez-read-database ()
  (setq bookiez-books nil)
  (with-temp-buffer
    (insert-file-contents bookiez-file)
    (while (not (eobp))
      (let ((book (split-string (buffer-substring (point) (line-end-position))
				"\t")))
	(while (< (length book) 7)
	  (nconc book (list "")))
	(push book bookiez-books))
      (forward-line 1))
    (setq bookiez-books (nreverse bookiez-books))))

(defun bookiez-write-database ()
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file bookiez-file
      (dolist (book bookiez-books)
	(insert (mapconcat (lambda (elem)
			     (subst-char-in-string ?\t ?  elem))
			   book
			   "\t")
		"\n")))
    (setq bookiez--database-timestamp
	  (file-attribute-modification-time (file-attributes bookiez-file)))))

(defun bookiez (&optional start-server)
  "List the books in the bookiez database."
  (interactive)
  (when start-server
    (bookiez-start-server))
  (bookiez--possibly-read-database)
  (pop-to-buffer "*Bookiez*")
  (bookiez-mode)
  (bookiez-display-authors))

(defun bookiez-display-authors ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :columns '((:name "Books" :min-width 6)
		(:name "Name" :max-width 60))
     :comparitor (lambda (o1 o2)
		   (equal (cadr o1) (cadr o2)))
     :objects-function
     (lambda ()
       (let ((authors (make-hash-table :test #'equal)))
	 (dolist (book bookiez-books)
	   (dolist (author (split-string (car book) ", "))
	     (cl-incf (gethash author authors 0))))
	 (sort
	  (let ((res nil))
	    (maphash (lambda (k v)
		       (push (list v k) res))
		     authors)
	    res)
	  (lambda (a1 a2)
	    (string< (cadr a1) (cadr a2))))))
     :keymap bookiez-mode-map)
    (goto-char (point-min))))

(defun bookiez-mark-as-read ()
  "Mark the book under point as read."
  (interactive)
  (let ((isbn (get-text-property (line-beginning-position) 'bookiez-isbn)))
    (unless isbn
      (error "No ISBN on the current line"))
    (bookiez-display-isbn-1 isbn t)))

(defun bookiez-display-books (author)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert author "\n\n")
    (let ((books nil)
	  start)
      (dolist (book bookiez-books)
	(when (equal author (car book))
	  (push book books)))
      (setq books (sort books (lambda (b1 b2)
				(string< (nth 3 b1) (nth 3 b2)))))
      (dolist (book books)
	(setq start (point))
	(insert (nth 3 book) " " (nth 1 book) "\n")
	(put-text-property start (1+ start) 'bookiez-thing
			   (bookiez-thumbnail (nth 5 book) (nth 2 book)))
	(put-text-property start (1+ start) 'bookiez-isbn (nth 2 book))))
    (goto-char (point-min))
    (forward-line 2)))

(defun bookiez-display-cover (thumbnail)
  (save-excursion
    (forward-line 1)
    (if (looking-at "\\*")
	;; Delete previously-inserted image.
	(delete-region (point) (progn (forward-line 1) (point)))
      (insert "\n")
      (forward-line -1)
      (url-retrieve thumbnail
		    'bookiez-image-fetched
		    (list (current-buffer) (point))
		    t))))

(defvar-keymap bookiez-mode-map
  :parent vtable-map
  "RET" #'bookiez-show-author
  "c" #'bookiez-edit-author
  "a" #'bookiez-add-book-manually
  "i" #'bookiez-add-isbn
  "e" #'bookiez-add-ebook-manually
  "r" #'bookiez-mark-as-read)

(defun bookiez-edit-author (name new-name)
  "Edit the author name under point."
  (interactive (list (nth 1 (vtable-current-object))
		     (read-string "New name: "
				  (nth 1 (vtable-current-object)))))
  (cl-loop for book in bookiez-books
	   for authors = (split-string (car book) ", ")
	   when (member name authors)
	   do (let ((new-list (delete name authors)))
		(push new-name new-list)
		(setcar book (string-join new-list ", "))))
  (bookiez-write-database)
  (forward-line 1)
  (vtable-revert-command))

(defun bookiez-add-isbn (isbn)
  "Add ISBN to the database."
  (interactive "sISBN: ")
  (bookiez-display-isbn isbn t))

(define-derived-mode bookiez-mode special-mode "Bookiez"
  "Mode for bookiez mode buffers."
  (setq truncate-lines t))

(defvar-keymap bookiez-author-mode-map
  :parent vtable-map
  "RET" #'bookiez-author-display-book
  "&" #'bookiez-author-goodreads
  "c" #'bookiez-author-edit-book
  "C" #'bookiez-author-edit-isin
  "q" #'bury-buffer)

(define-derived-mode bookiez-author-mode special-mode "Bookiez"
  "Mode to display books."
  (setq truncate-lines t))

(defun bookiez-show-author (author)
  "Show the data for AUTHOR."
  (interactive (list (cadr (vtable-current-object))))
  (switch-to-buffer "*Bookiez Author*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-author-mode)
    (make-vtable
     :columns '((:name "Cover")
		(:name "Format")
		(:name "Read")
		(:name "Year")
		(:name "Bought")
		(:name "Read-Time")
		(:name "Title" :primary t))
     :objects-function
     (lambda ()
       (seq-filter (lambda (elem)
		     (member author (split-string (car elem) ", ")))
		   bookiez-books))
     :getter #'bookiez--get-book-data
     :keymap bookiez-author-mode-map)
    (goto-char (point-min))))

(defun bookiez-list ()
  "List all the books."
  (interactive)
  (bookiez--possibly-read-database)
  (switch-to-buffer "*Bookiez*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-author-mode)
    (make-vtable
     :columns '((:name "Format")
		(:name "Read")
		(:name "Year")
		(:name "Bought")
		(:name "Read-Time")
		(:name "Author" :max-width 25)
		(:name "Title"))
     :objects-function (lambda () bookiez-books)
     :getter #'bookiez--get-book-data
     :keymap bookiez-author-mode-map)
    (goto-char (point-min))))

(defun bookiez--get-book-data (object column table)
  (cl-destructuring-bind ( author title isbn published-date
			   bought-date _thumbnail format
			   . read)
      object
    (pcase (vtable-column table column)
      ("Format"
       (if (equal format "paper")
	   "📓"
	 "📄"))
      ("Read"
       (if (member "unread" read)
	   "⚫"
	 "🟢"))
      ("Year"
       (if (equal published-date "1970-01-01")
	   ""
	 (substring published-date 0 4)))
      ("Bought"
       ;; Registration started in 2013, so the data before that
       ;; isn't accurate.  And the second date is when ebook data
       ;; was imported, so it's not accurate either.
       (cond
	((or (string< bought-date "2013-02-01")
	     (equal bought-date "2025-04-14"))
	 "")
	((< (length bought-date) 4)
	 bought-date)
	(t
	 (substring bought-date 0 4))))
      ("Read-Time"
       (or
	(cl-loop for elem in read
		 when (string-match "\\`read:\\(.*\\)" elem)
		 return (match-string 1 elem))
	""))
      ("Author"
       author)
      ("Title"
       title)
      ("Cover"
       (let ((file (expand-file-name (format "%s.jpg" isbn)
				     "~/.emacs.d/bookiez-cache/")))
	 (propertize "*" 'display 
		     (if (file-exists-p file)
			 (create-image file nil nil :height 100 :max-width 100)
		       (let ((svg (svg-create 60 100)))
			 (svg-rectangle svg 0 0 60 100 :fill "#202020")
			 (svg-image svg)))))))))

(defun bookiez-author-display-book ()
  "Display the book under point."
  (interactive)
  (bookiez-display-isbn (nth 2 (vtable-current-object))))

(defun bookiez-author-goodreads ()
  "Go to the Goodreads for the book."
  (interactive)
  (browse-url
   (format "https://www.goodreads.com/search?q=%s"
	   (nth 2 (vtable-current-object)))))

(defun bookiez-author-edit-book ()
  "Edit the author/book name under point."
  (interactive)
  (let* ((current (vtable-current-object))
	 (author (read-string "New author name: " (nth 0 current)))
	 (title (read-string "New book title: " (nth 1 current))))
    (cl-loop for book in bookiez-books
	     when (eq current book)
	     do (setf (car book) author)
	     (setf (cadr book) title))
    (bookiez-write-database)
    (forward-line 1)
    (vtable-revert-command)))

(defun bookiez-author-edit-isin ()
  "Edit the ISBN of the book under point."
  (interactive)
  (let* ((current (vtable-current-object))
	 (isbn (read-string "New ISBN: ")))
    (cl-loop for book in bookiez-books
	     when (eq current book)
	     do
	     (setf (nth 2 book) isbn)
	     (when-let ((urls (isbn-covers isbn)))
	       (setf (nth 5 book) (car urls))
	       (bookiez-cache-image isbn (car urls))))
    (bookiez-write-database)))

(defun bookiez-fill-isbn ()
  "Query for ISBN for books that lack it."
  (cl-loop with data
	   for book in bookiez-books
	   for isbn = (nth 2 book)
	   for string = (format "%s %s" (nth 0 book)
				(nth 1 book))
	   when (and (not (isbn-valid-p isbn))
		     (y-or-n-p (format "Query %s? " string))
		     (setq data (isbn-search-goodreads string))
		     (car data))
	   do
	   (setf (nth 2 book) (car data))
	   (when (zerop (length (nth 5 book)))
	     (setf (nth 5 book) (cadr data))))
  (bookiez-write-database))

(defun bookiez-missing-isbn ()
  (pop-to-buffer "*missing*")
  (erase-buffer)
  (cl-loop for book in bookiez-books
	   for isbn = (nth 2 book)
	   for string = (format "%s %s" (nth 0 book)
				(nth 1 book))
	   when (not (isbn-valid-p isbn))
	   do (insert string "\n")))

(defun bookiez-fill-image-cache ()
  (cl-loop for book in bookiez-books
	   do (bookiez-cache-image (nth 2 book) (nth 5 book))))

(defun bookiez-cache-image (isbn url)
  (unless (file-exists-p "~/.emacs.d/bookiez-cache/")
    (make-directory "~/.emacs.d/bookiez-cache/"))
  (let ((file (expand-file-name (concat isbn ".jpg")
				"~/.emacs.d/bookiez-cache/")))
    (when (and (not (file-exists-p file))
	       url
	       (string-match "\\`http" url))
      (when-let ((buf (ignore-errors (url-retrieve-synchronously url))))
	(with-current-buffer buf
	  (goto-char (point-min))
	  (when (and (search-forward "\n\n" nil t)
		     (not (re-search-forward "<div\\b" nil t))
		     (not (eobp)))
	    (write-region (point) (point-max) file))
	  (kill-buffer (current-buffer)))))))

(defun bookiez-query-covers (&optional from)
  (cl-loop with started = (not from)
	   for book in bookiez-books
	   for url = (nth 5 book)
	   when (equal (nth 1 book) from)
	   do (setq started t)
	   when (and (isbn-valid-p (nth 2 book))
		     started
		     (or (not url)
			 (not (string-match "\\`http" url))
			 (string-match "bks[0-9]+.books.google.com" url)))
	   do (message "Querying %s" (nth 1 book))
	   (when-let ((urls (isbn-covers (nth 2 book))))
	     (setf (nth 5 book) (car urls))))
  (bookiez-write-database))

(provide 'bookiez)
