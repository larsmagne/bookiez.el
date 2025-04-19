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
(require 'server)
(require 'iso8601)
(require 'perplexity)
(require 'openai)

(defvar bookiez-file "~/.emacs.d/bookiez.data")

(defun bookiez-thumbnail (thumbnail isbn)
  (if (and thumbnail
	   (cl-plusp (length thumbnail)))
      thumbnail
    (format "http://covers.librarything.com/devkey/%s/large/isbn/%s"
	    isbn-librarything-key isbn)))

(defvar bookiez-last-isbn nil)
(defvar bookiez-books nil)

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
  "a" #'bookiez-add-book-manually
  "&" #'bookiez-book-goodreads
  "l" #'bookiez
  "c" #'bookiez-book-edit)

(defvar bookiez-book-isbn nil)

(defun bookiez-book-goodreads ()
  "Go to the Goodreads for the book."
  (interactive)
  (browse-url
   (format "https://www.goodreads.com/search?q=%s" bookiez-book-isbn)))

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
		(insert-image (create-image file nil nil :max-width 800
					    :max-height 800))
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
  (when (file-exists-p file)
    (call-process "amixer" nil nil nil "-c" "0" "set" "Speaker" "100%")
    (start-process
     "*mpg*" (get-buffer-create "*mpg123*")
     "mpg123"
     "-a" "hw:0"
     ;;"-f" "1000"
     "-n" "10"
     (expand-file-name file "/music/repository/Various/Ringtones"))))

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
      (when-let ((match (isbn-search-goodreads (concat author " " title))))
	(when (y-or-n-p (format "Is this %s? "
				(car match)))
	  (setq isbn (car match)
		thumb (cadr match)))))
    (unless isbn
      (setq isbn (read-string "ISBN: ")))
    (when (zerop (length isbn))
      (setq isbn (format "%s" (cl-decf bookiez--unknown-isbn))))
    (bookiez-add-book author title isbn date thumb ebook
		      (y-or-n-p "Book read? "))
    (bookiez-cache-image isbn thumb)
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
  (switch-to-buffer "*Bookiez*")
  (bookiez-mode)
  (bookiez-display-authors))

(defvar-keymap bookiez-mode-map
  :parent vtable-map
  "RET" #'bookiez-show-author
  "l" #'bookiez-list
  "a" #'bookiez-add-book-manually
  "&" #'bookiez-goodreads
  "c" #'bookiez-edit-author
  "i" #'bookiez-add-isbn
  "SPC" #'bookiez-toggle-tracking
  "n" #'bookiez-search-tracked-authors
  "e" #'bookiez-add-ebook-manually)

(defun bookiez-goodreads ()
  "Go to the Goodreads for the author."
  (interactive)
  (browse-url
   (format "https://www.goodreads.com/search?q=%s"
	   (nth 2 (vtable-current-object)))))

(define-multisession-variable bookiez-tracked-authors nil)

(defun bookiez-display-authors ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-mode)
    (setq truncate-lines t)
    (let* ((tracked (multisession-value bookiez-tracked-authors))
	   (table
	    (make-vtable
	     :columns '((:name "Tracked" :min-width 6)
			(:name "Books" :min-width 6)
			(:name "Author" :max-width 60))
	     :getter
	     (lambda (object column table)
	       (pcase (vtable-column table column)
		 ("Tracked"
		  (if (car object) "✴️" ""))
		 (_
		  (elt object column))))
	     :objects-function
	     (lambda ()
	       (let ((authors (make-hash-table :test #'equal)))
		 (dolist (book bookiez-books)
		   (dolist (author (split-string (car book) ", "))
		     (cl-incf (gethash author authors 0))))
		 (sort
		  (let ((res nil))
		    (maphash (lambda (k v)
			       (push (list (member k tracked) v k)
				     res))
			     authors)
		    res)
		  (lambda (a1 a2)
		    (string< (caddr a1) (caddr a2))))))
	     :keymap bookiez-mode-map)))
      ;; This may not exist in all vtable versions.
      (when (fboundp 'vtable-comparitor)
	(setf (vtable-comparitor table)
	      (lambda (o1 o2)
		(equal (nth 2 o1) (nth 2 o2))))))))

(defun bookiez-mark-as-skipped ()
  "Mark the book under point as skipped."
  (interactive)
  (bookiez-mark-as-read nil "skipped"))

(defun bookiez-mark-as-read (&optional unknown-date read-prefix)
  "Mark the book under point as read.
If given a prefix, don't mark it read on a specific date."
  (interactive "P")
  (let ((book (vtable-current-object)))
    (unless book
      (error "No book on the current line"))
    (if unknown-date
	;; Remove anything in the unread/read section.
	(setcdr (nthcdr 6 book) nil)
      ;; Remove the "unread"...
      (setcdr (nthcdr 6 book)
	      (delete "unread" (nthcdr 7 book)))
      ;; ... and add a read:.
      (nconc book (list (format "%s:%s"
				(or read-prefix "read")
				(format-time-string "%Y-%m-%d")))))
    (bookiez-write-database)
    (vtable-update-object (vtable-current-table) book book)
    (message "Marked %s as read" (nth 1 book))))

(defun bookiez-mark-as-unread ()
  "Mark the book under point as unread."
  (interactive)
  (let ((book (vtable-current-object)))
    (unless book
      (error "No book on the current line"))
    ;; Remove the previous data (if any) and mark as unread.
    (setcdr (nthcdr 6 book) (list "unread"))
    (bookiez-write-database)
    (vtable-update-object (vtable-current-table) book book)
    (message "Marked %s as unread" (nth 1 book))))

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

(defun bookiez-author-delete-book ()
  "Delete the book under point."
  (interactive)
  (let ((book (vtable-current-object)))
    (unless book
      (user-error "No book on the current line"))
    (unless (yes-or-no-p (format "Really delete %s: %s? "
				 (car book) (cadr book)))
      (user-error "Aborting"))
    (setq bookiez-books (delq book bookiez-books))
    (bookiez-write-database)
    (vtable-remove-object (vtable-current-table) book)
    (message "Removed %s: %s" (car book) (cadr book))))

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

(defun bookiez-toggle-tracking ()
  "Toggle whether to track the author under point."
  (interactive)
  (let* ((object (vtable-current-object))
	 (author (nth 2 object)))
    (setf (car object) (not (car object)))
    (setf (multisession-value bookiez-tracked-authors)
	  (delete author (multisession-value bookiez-tracked-authors)))
    (when (car object)
      (push author (multisession-value bookiez-tracked-authors)))
    (vtable-update-object (vtable-current-table) object object)))

(defun bookiez-search-tracked-authors (year)
  "Search for new books from all tracked authors that are newer than YEAR."
  (interactive "nSearch for book never than year: ")
  (switch-to-buffer "*Bookiez Search*")
  (let ((inhibit-read-only t)
	data comments)
    (erase-buffer)
    (bookiez-search-mode)
    (if (eq bookiez-assistant-function 'perplexity-query)
	(dolist (author (multisession-value bookiez-tracked-authors))
	  (message "Querying %s..." author)
	  (cl-destructuring-bind (adata acomments)
	      (bookiez-query-assistant-author
	       author
	       (format "Only include books that are published after %s. If there are no books from this author published after %s, don't output anything."
		       year year))
	    (setq data (append data adata)
		  comments (append comments acomments))))
      (cl-multiple-value-setq (data comments)
	(bookiez-query-assistant-author
	 (multisession-value bookiez-tracked-authors)
	 (format "Only include books that are published after %s. If there are no books from this author published after %s, don't output anything."
		 year year))))
    (clear-minibuffer-message)
    (when data
      (make-vtable
       :row-colors '("#404040" "#202020")
       :divider-width 2
       :columns '((:name "Author" :primary t)
		  (:name "Year" :max-width 10)
		  (:name "Title" :max-width 40)
		  (:name "Comment"))
       :objects (mapcar (lambda (b)
			  (list (nth 0 b) (nth 2 b) (nth 1 b) (nth 3 b)))
			data)
       :keymap bookiez-search-mode-map))
    (goto-char (point-max))
    (insert "\n")
    (dolist (comment (delete "" comments))
      (let ((start (point)))
	(insert comment "\n\n")
	(save-restriction
	  (narrow-to-region start (point))
	  (fill-region (point-min) (point-max)))))
    (goto-char (point-min))))

(defun bookiez-edit-author (name new-name)
  "Edit the author name under point."
  (interactive (list (nth 2 (vtable-current-object))
		     (read-string "New name: "
				  (nth 2 (vtable-current-object)))))
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
  "Add the book with ISBN to the database."
  (interactive "sISBN: ")
  (bookiez-display-isbn isbn t))

(define-derived-mode bookiez-mode special-mode "Bookiez"
  "Mode for bookiez mode buffers."
  (setq truncate-lines t))

(defvar-keymap bookiez-author-mode-map
  :parent vtable-map
  "RET" #'bookiez-author-display-book
  "l" #'bookiez
  "A" #'bookiez-author-display-author
  "i" #'bookiez-add-isbn
  "a" #'bookiez-add-book-manually
  "&" #'bookiez-author-goodreads
  "c" #'bookiez-author-edit-book
  "C" #'bookiez-author-edit-all-data
  "r" #'bookiez-mark-as-read
  "u" #'bookiez-mark-as-unread
  "k" #'bookiez-mark-as-skipped
  "DEL" #'bookiez-author-delete-book
  "s" #'bookiez-author-search
  "n" #'bookiez-author-search-new-books
  "m" #'bookiez-author-search-missing-books
  "q" #'bury-buffer)

(define-derived-mode bookiez-author-mode special-mode "Bookiez"
  "Mode to display books."
  (setq truncate-lines t))

(defun bookiez-show-author (author)
  "Show the data for AUTHOR."
  (interactive (list (nth 2 (vtable-current-object))))
  (switch-to-buffer "*Bookiez Author*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-author-mode)
    (make-vtable
     :row-colors '("#404040" "#202020")
     :divider-width 2
     ;:column-colors '("#404040" "#202020")
     :columns '((:name "Cover")
		(:name "Format")
		(:name "Status")
		(:name "Published" :primary t :width 5)
		(:name "Bought" :width 5)
		(:name "Read" :min-width 12)
		(:name "Title" :min-width 80))
     :objects-function
     (lambda ()
       (seq-filter (lambda (elem)
		     (member author (split-string (car elem) ", ")))
		   bookiez-books))
     :getter #'bookiez--get-book-data
     :formatter #'bookiez--formatter
     :keymap bookiez-author-mode-map)))

(defun bookiez-author-display-author ()
  "Display the author of the book under point."
  (interactive)
  (bookiez-show-author (nth 0 (vtable-current-object))))

(defun bookiez-author-search ()
  "Search for books by the author under point."
  (interactive)
  (bookiez-search-author (nth 0 (vtable-current-object))))

(defun bookiez-author-search-new-books ()
  "Search for new books by the author under point."
  (interactive)
  (let ((author (nth 0 (vtable-current-object))))
    (bookiez-search-author-new-books
     author
     (cl-loop for book in bookiez-books
	      when (member author (split-string (nth 0 book) ", "))
	      maximize (string-to-number (substring (nth 3 book) 0 4))))))

(defun bookiez-author-search-missing-books ()
  "Search for missing books by the author under point."
  (interactive)
  (let ((author (nth 0 (vtable-current-object))))
    (bookiez-search-author
     author
     (concat
      "Do not include books from this list: "
      (string-join
       (cl-loop for book in bookiez-books
		when (member author (split-string (nth 0 book) ", "))
		collect (nth 1 book))
       ", ")
      ;; Apparently, something like this is needed to make it shut up
      ;; about what it's excluding.
      "That is, if a book appeared on the preceding list, do not "
      "include that in your output.  You do not need to mention that "
      "you've excluded these books. "))))

(defun bookiez-list ()
  "List all the books."
  (interactive)
  (bookiez--possibly-read-database)
  (switch-to-buffer "*Bookiez List*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-author-mode)
    (make-vtable
     :row-colors '("#202020" "#000000")
     :columns '((:name "Format")
		(:name "Status")
		(:name "Author" :max-width 30)
		(:name "Title"))
     :objects-function (lambda () bookiez-books)
     :getter #'bookiez--get-book-data
     :formatter #'bookiez--formatter
     :keymap bookiez-author-mode-map)))

(defun bookiez--formatter (value column table)
  (propertize
   (pcase (vtable-column table column)
     ("Read"
      (if (equal value "")
	  ""
	(string-clean-whitespace
	 (format-time-string
	  "%b %e, %Y"
	  (encode-time
	   (decoded-time-set-defaults
	    (iso8601-parse-date value)))))))
     ("Published"
       (if (equal value "1970-01-01")
	   ""
	 (substring value 0 4)))
     ("Bought"
      ;; Registration started in 2013, so the data before that
      ;; isn't accurate.  And the second date is when ebook data
      ;; was imported, so it's not accurate either.
      (cond
       ((or (string< value "2013-02-01")
	    (equal value "2025-04-14"))
	"")
       ((< (length value) 4)
	value)
       (t
	(substring value 0 4))))
     (_
      value))
   'face 'vtable))

(defun bookiez--get-book-data (object column table)
  (cl-destructuring-bind ( author title isbn published-date
			   bought-date _thumbnail format
			   . read)
      object
    (pcase (vtable-column table column)
      ("Format"
       (if (equal format "paper")
	   "📘"
	 "📄"))
      ("Status"
       (cond ((member "unread" read)
	      "🟣")
	     ((cl-loop for elem in read
		       when (string-match-p "\\`skipped:" elem)
		       return t)
	      "❌")
	     (t
	      "✔️")))
      ("Published"
       published-date)
      ("Bought"
       bought-date)
      ("Read"
       (or
	(cl-loop for elem in read
		 when (string-match "\\`\\(read\\|skipped\\):\\(.*\\)" elem)
		 return (match-string 2 elem))
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

(defun bookiez-author-edit-all-data ()
  "Edit all the data of the book under point."
  (interactive)
  (let ((book (vtable-current-object)))
    (cl-loop for slot in '("Author" "Title" "ISBN"
			   "Published Date" "Bought Date"
			   "Thumbnail" "Format")
	     for n from 0
	     do (setf (nth n book)
		      (read-string (concat slot ": ") (nth n book))))
    (when-let ((urls (isbn-covers (nth 2 book))))
      (setf (nth 5 book) (car urls))
      (bookiez-cache-image (nth 2 book) (car urls)))
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

(defun bookiez-fill-dates ()
  (let ((isbn-lookup-types '(goodreads)))
    (cl-loop for book in bookiez-books
	     for isbn = (nth 2 book)
	     when (isbn-valid-p isbn)
	     do
	     (message "Querying %s" (nth 1 book))
	     (when-let ((data (isbn-lookup isbn)))
	       (when (nth 2 data)
		 (setf (nth 3 book) (nth 2 data))
		 (message "Date for %s is %s" (nth 1 book) (nth 2 book))))
	     (sleep-for 2)))
  (bookiez-write-database))

(defvar bookiez-goodreads-data (make-hash-table :test #'equal))

(defun bookiez-get-goodreads-data ()
  (let ((isbn-lookup-types '(goodreads)))
    (cl-loop for i from 1
	     for book in bookiez-books
	     for isbn = (nth 2 book)
	     when (isbn-valid-p isbn)
	     do
	     (message "Querying %d %s" i (nth 1 book))
	     (when-let ((data (isbn-lookup isbn)))
	       (setf (gethash isbn bookiez-goodreads-data) data))
	     (sleep-for 2))))

(defun bookiez-list-duplicate-isbn ()
  (pop-to-buffer "*duplicates*")
  (erase-buffer)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (book bookiez-books)
      (let ((isbn (nth 2 book)))
	(when-let ((other (gethash isbn table)))
	  (insert (format "%s %s -> %s %s\n" (nth 0 other) (nth 1 other)
			  (nth 0 book) (nth 1 book))))
	(setf (gethash isbn table) book)))))

(defvar bookiez-assistant-function 'perplexity-query
  "Function to retrieve data from an assistant.")

(defun bookiez-query-assistant-author (author &optional extra-text)
  (let ((result
	 (funcall
	  bookiez-assistant-function
	  (concat
	   (if (consp author)
	       (concat "List, in chronological order, "
		       "all books published by the following authors: "
		       (string-join author ", ")
		       ". ")
	     (concat "List all books published by " author
		     " in chronological order.  "))
	   "For each book, include (in this order) "
	   "the author name, the book name, the publication year, "
	   "and the type of book (i.e., novel, short story collection, etc), "
	   "and use a semicolon as the separator. "
	   "Do not number the responses. "
	   "Do not include omnibus editions. "
	   "Only include books where the author is the main contributor. "
	   "Do not include books that are just edited by the author. "
	   (or extra-text "")))))
    (cl-loop for line in (string-lines result)
	     if (string-match ";.*;.*;" line)
	     collect (split-string
		      (replace-regexp-in-string "\\[[0-9]+\\]" "" line)
		      "; *")
	     into data
	     else
	     collect line into comments
	     finally (cl-return (list data comments)))))

(defvar-keymap bookiez-search-mode-map
  "&" #'bookiez-search-goodreads
  "b" #'bookiez-search-bookshop
  "u" #'bookiez-search-biblio)

(define-derived-mode bookiez-search-mode special-mode "Bookiez"
  "Mode to search for books."
  (setq truncate-lines t))

(defvar bookiez-author)

(defun bookiez-search-author-isbndb ()
  (interactive)
  (let ((author (nth 0 (vtable-current-object))))
    (bookiez--search-author-render
     (cl-loop for book across (gethash "books" (isbn-author-isbndb author))
	      collect (list author
			    (gethash "title" book)
			    (let ((date (gethash "date_published" book)))
			      (if date
				  (string-to-number (substring date 0 4))
				0))
			    (gethash "language" book)))
     nil)))

(defun bookiez-search-author (author &optional extra-text)
  (cl-destructuring-bind (data comments)
      (bookiez-query-assistant-author author extra-text)
    (unless data
      (error "No data for %s" author))
    (bookiez--search-author-render data comments)))

(defun bookiez-search-author-new-books (author last-year)
  (cl-destructuring-bind (data comments) (bookiez-query-assistant-author author)
    (unless data
      (error "No data for %s" author))
    (bookiez--search-author-render
     (cl-loop for (title year comment) in data
	      when (> (string-to-number year) last-year)
	      collect (list title year comment))
     comments)))

(defun bookiez--search-author-render (data comments)
  (switch-to-buffer "*Bookiez Search*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bookiez-search-mode)
    (when data
      (make-vtable
       :row-colors '("#404040" "#202020")
       :divider-width 2
       :columns '((:name "Year" :primary t :max-width 10)
		  (:name "Title" :max-width 40)
		  (:name "Comment"))
       :objects (mapcar (lambda (b)
			  (list (nth 0 b) (nth 2 b) (nth 1 b) (nth 3 b)))
			data)
       :getter
       (lambda (object column _table)
	 (nth (1+ column) object))
       :keymap bookiez-search-mode-map))
    (goto-char (point-max))
    (insert "\n")
    (dolist (comment comments)
      (let ((start (point)))
	(insert comment "\n\n")
	(save-restriction
	  (narrow-to-region start (point))
	  (fill-region (point-min) (point-max)))))
    (goto-char (point-min))))

(defun bookiez-search-goodreads ()
  "Search Goodreads for the book under point."
  (interactive)
  (browse-url
   (format "https://www.goodreads.com/search?q=%s %s"
	   (nth 0 (vtable-current-object))
	   (nth 2 (vtable-current-object)))))
  
(defun bookiez-search-bookshop ()
  "Search Bookshop for the book under point."
  (interactive)
  (browse-url
   (format "https://bookshop.org/beta-search?keywords=%s %s"
	   (nth 0 (vtable-current-object))
	   (nth 2 (vtable-current-object)))))

(defun bookiez-search-biblio ()
  "Search Biblio for the book under point."
  (interactive)
  (browse-url
   (format "https://www.biblio.com/search.php?stage=1&title=%s %s"
	   (nth 0 (vtable-current-object))
	   (nth 2 (vtable-current-object)))))

(provide 'bookiez)
