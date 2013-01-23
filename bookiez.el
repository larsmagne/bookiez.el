;;; bookiez.el --- Managing Books
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

;; (bookiez-display-isbn "1931520003")

(require 'json)
(require 'cl)
(require 'isbn)

(defvar bookiez-file "~/.emacs.d/bookiez.data")
(defvar bookiez-mode nil)

(defun bookiez-thumbnail (thumbnail isbn)
  (or thumbnail
      (format "http://covers.librarything.com/devkey/%s/large/isbn/%s"
	      isbn-librarything-key isbn)))

(setq bookiez-last-isbn nil)

(defun bookiez-display-isbn (isbn &optional save)
  (message "Querying %s" isbn)
  (bookiez-play "71-On the Beach .mp3")
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

(defun bookiez-display-isbn-1 (isbn &optional save)
  (destructuring-bind (title author date thumbnail) (isbn-lookup isbn)
    (setq date (or date "1970-01-01"))
    (if (not title)
	(progn
	  (message "No match for %s" isbn)
	  (bookiez-play "45-VENOZ TKS - Carry On Sergeant. Right Oh, Sir!.mp3"))
      (switch-to-buffer "*isbn*")
      (erase-buffer)
      (bookiez-mode)
      (insert author "\n" title "\n" date "\nISBN" isbn "\n\n")
      (url-retrieve (bookiez-thumbnail thumbnail isbn)
		    'bookiez-image-fetched
		    (list (current-buffer) (point))
		    t)
      (setq bookiez-last-isbn nil)
      (bookiez-play "61-KREVmorse .mp3")
      (bookiez-add-book author title isbn date thumbnail))))

(defun bookiez-play (file)
  (start-process
   "*mpg*" nil
   "mpg123-alsa"
   "-a" "hw:1.0"
   "-f" "3000"
   "-n" "10"
   (expand-file-name file "/music/repository/Various/Ringtones")))

(defun bookiez-add-book-manually ()
  (interactive)
  (bookiez-add-book (read-string "Author: ")
		    (read-string "Title: ")
		    (or bookiez-last-isbn (read-string "ISBN: "))
		    "1970-01-01"
		    nil)
  (setq bookiez-last-isbn nil))

(defun bookiez-image-fetched (status buffer point)
  (goto-char (point-min))
  (when (or (search-forward "\n\n" nil t)
	    (search-forward "\r\n\r\n" nil t))
    (let ((image (buffer-substring (point) (point-max))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char point)
	  (insert-image (create-image image nil t) "*"))))))

(defun bookiez-start-server ()
  (setq server-use-tcp t
	server-host (system-name)
	server-name "bookiez")
  (server-start))

(defvar bookiez-books nil)

(defun bookiez-add-book (author title isbn date thumbnail)
  (unless bookiez-books
    (bookiez-read-database))
  (let ((do-insert t)
	(update-read nil))
    (loop for book in bookiez-books
	  for unread = (member "unread" (nthcdr 6 book))
	  when (or (equal isbn (nth 2 book))
		   (and (equal author (car book))
			(equal title (cadr book))))
	  do (message "%s/%s (%s) already exists in the database%s"
		      author title isbn
		      (if (setq update-read (and unread book))
			  "; marking as read"
			""))
	  (setq do-insert nil))
    (cond
     (do-insert
      (push (list author title isbn date
		  (format-time-string "%Y-%m-%d")
		  thumbnail
		  "unread")
	    bookiez-books)
      (bookiez-write-database))
     (update-read
      (setcdr (nthcdr 5 update-read)
	      (delete "unread" (nthcdr 6 update-read)))
      (nconc update-read (list (concat "read:"
				       (format-time-string "%Y-%m-%d"))))
      (bookiez-write-database)))))

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
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file bookiez-file
      (dolist (book bookiez-books)
	(insert (mapconcat (lambda (elem)
			     (subst-char-in-string ?\t ?  elem))
			   book
			   "\t")
		"\n")))))

(defun bookiez ()
  "List the books in the bookiez database."
  (interactive)
  (bookiez-start-server)
  (unless bookiez-books
    (bookiez-read-database))
  (pop-to-buffer "*Bookiez*")
  (erase-buffer)
  (bookiez-mode)
  (bookiez-display-authors))

(defun bookiez-display-authors (&optional focus)
  (setq bookiez-mode 'author)
  (erase-buffer)
  (let ((authors nil)
	start)
    (dolist (book bookiez-books)
      (unless (member (car book) authors)
	(push (car book) authors)))
    (setq authors (sort authors 'string<))
    (dolist (author authors)
      (when (and focus
		 (equal focus author))
	(setq focus (point)))
      (setq start (point))
      (insert author "\n")
      (put-text-property start (1+ start) 'bookiez-thing author))
    (goto-char (or focus (point-min)))))

(defun bookiez-choose ()
  "Choose the author or book under point."
  (interactive)
  (let ((thing (get-text-property (line-beginning-position) 'bookiez-thing)))
    (when thing
      (cond
       ((eq bookiez-mode 'author)
	(bookiez-display-books thing))
       ((eq bookiez-mode 'book)
	(bookiez-display-cover thing))))))

(defun bookiez-display-books (author)
  (setq bookiez-mode 'book)
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
			 (bookiez-thumbnail (nth 5 book) (nth 2 book)))))
  (goto-char (point-min))
  (forward-line 2))

(defun bookiez-quit ()
  "Pop to the previous level."
  (interactive)
  (if (eq bookiez-mode 'book)
      (progn
	(goto-char (point-min))
	(bookiez-display-authors
	 (buffer-substring (point) (line-end-position))))
    (bury-buffer)))

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

(defvar bookiez-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'bookiez-choose)
    (define-key map "q" 'bookiez-quit)
    (define-key map "a" 'bookiez-add-book-manually)
    (define-key map "i" 'bookiez-add-isbn)
    map))

(defun bookiez-add-isbn (isbn)
  "Add ISBN to the database."
  (interactive "sISBN: ")
  (bookiez-display-isbn isbn t))

(defun bookiez-mode ()
  "Mode for bookiez mode buffers.

\\{bookiez-mode-map}"
  (interactive)
  (setq major-mode 'bookiez-mode)
  (setq mode-name "Bookiez")
  (set (make-local-variable 'bookiez-mode) 'author)
  (use-local-map bookiez-mode-map)
  (setq truncate-lines t))

(provide 'bookiez)
