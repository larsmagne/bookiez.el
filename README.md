Bookiez
=======

bookiez is an Emacs package for keeping track of books you own.  The
basic idea is that you enter ISBNs, and then bookiez queries various
ISBN lookup providers, and then stores the information.

The information is then presented either on a per-author basis:

![](https://lars.ingebrigtsen.no/wp-content/uploads/2025/07/2025-07-25-3.jpg)

And you can list all the books by the author:

![](https://lars.ingebrigtsen.no/wp-content/uploads/2025/07/2025-07-25-4.jpg)

Or you can list all the books in one buffer:

![](https://lars.ingebrigtsen.no/wp-content/uploads/2025/07/2025-07-25-5.jpg)

And you can look at the detailed information about the book:

![](https://lars.ingebrigtsen.no/wp-content/uploads/2025/07/2025-07-25-6.jpg)

bookiez keeps track of the date you bought the book, when it was
published, and when you marked it as read (or "skipped").

To get started, put the bookiez directory in your load path and
autoload the top-level command:

    (push "~/src/bookiez.el/" load-path)
    (autoload 'bookiez "bookiez" nil t)
	
Then just type `M-x bookiez' to start bookiez.

To enter a new book, either use the 'i' command, or just start typing
the ISBN followed by a `RET'.  bookiez will then query the ISBN using
Goodreads, the Google Book API, the OpenLibrary API, and optionally
the ISBNDB API.

If you wish to use ISBNDB, you need to get an API key from ISBNDB, and
set `isbn-isbndb-key' to that key.  See
https://isbndb.com/isbndb-api-documentation-v2.

Tracked Authors
===============

If you have authors you follow particularly, these are called "tracked
authors" (because you're keeping track of them).  To mark an author
this way, use the `SPC' command in the author buffer.  The `n' command
will then report on new books from all these authors.  As explained in 
this blog post:
https://lars.ingebrigtsen.no/2025/04/17/perplexingly-book-learned-emacs/ 
There really are no good ways to get this data at the present.  So
bookiez will use an LLM to try to figure out this stuff.  The most
useful backend for this is Perplexity.ai.  You need to get an API key
for that, and set `query-assistant-perplexity-key' for this command to
work.

You do not need to use the LLM stuff to use bookiez -- it's only used
for a couple of commands like this.  And as usual with LLMs, the
output is not to be trusted, and will usually give different results
ever time you use the commands.  That is, the LLMs will invent books
that don't exist, and they will also skip books that do exist.  But
it's moderately useful anyway.

Commands
========

There's a whole bunch of commands in the various bookiez buffers --
for searching Goodreads, Bookshop and Biblio, for instance.  Use the
normal Emacs commands (`C-h b' etc) to get a list of available
commands in each buffer.

Data Format and Exports
=======================

The data is written in a JSON format to `bookiez-data-file', which is
~/.emacs.d/bookiez.json by default.

You can export the data to HTML (suitable for putting on a web site to
look at your books while on the go) by using the `M-x
bookiez-export-html' command.  Afterwards, just put the resulting
files in a directory on a web server.  The top-level HTML file will be
called "authors.html".

The assets/bookiez.css file can be edited to adjust the look.  All the
elements should have semi-logical names to allow you to style (or
remove) elements as needed without changing the code.

Barcode Scanners
================

If you have a lot of books to enter, it's strongly recommended to get
an ISBN barcode scanner.  I've been using a Datalogic Gryphon D120 for
over a decade:

![](https://lars.ingebrigtsen.no/wp-content/uploads/2025/07/2025-07-25-7.jpg)

It's most convenient if it works like a USB HID keyboard and just
outputs the ISBN followed by a `RET', which will make it work
automatically with bookiez.

It can also be convenient to have your Emacs respond to a barcode
scanner no matter which window has focus -- that way you can just grab
the scanner after buying some new books without futzing around at all.
But this requires some setup.

You need this package from Microsoft Github:

https://github.com/larsmagne/libinput.el

And then:

	apt install libinput-tools
	adduser `whoami` input
	
Then log out and in again to ensure that you're allowed to read the
libinput device.  (Note that some may consider this to be a security
issue -- basically all programs that are running as yourself may then
read all input events.)

	libinput list-devices
	
and look at the output to find your barcode scanner.  Mine looks like:

	Device:                  © Datalogic 2002 Datalogic Bar Code Scanner
	Kernel:                  /dev/input/event21

So then you 

    (setq bookiez-barcode-device "© Datalogic 2002 Datalogic Bar Code Scanner")
	
After restarting bookiez, bookiez will then automatically listen for
events from the barcode scanner and mark new scanned ISBNs as newly
bought books.  If you scan a code twice, it'll mark the book as read.

Other Inputs
============

In general, any barcode reading device that can trigger a command can
be used.  On the Emacs side, start an Emacs server with 
`M-x server-start'.

Then use `emacs-client' to enter a book into the database by calling a
script that looks like this (with the ISBN as the first parameter):

	#!/bin/bash
	emacsclient --eval "(bookiez-enter-isbn \"$1\")"

The `bookiez-enter-isbn' function will play different sort of beeps
based on whether the ISBN is invalid, whether it can't be found, or
whether it entered the book successfully.  This way you can enter
books rapidly without looking at the screen.

Webcam Barcode Camera
=====================

Instead of a USB HID barcode scanner, you can also use a webcam with
barcode scanning.  For instance, under X you may automate it something
like this:

	#!/bin/bash

	echo "Finding Emacs with bookiez..."
	BOOKIEZ=$(xdotool search --name 'Bookiez List')

	if [ "$BOOKIEZ" = "" ]; then
		echo "ERROR: Couldn't find *Bookiez List*"
		exit 10
	fi

	echo "Scanning..."
	zbarcam --set ean13.enable=1 --raw --prescale=640x480 /dev/video48 | while read -r line; do
		echo -n "Sending $line ..."
		CURRENTWINDOW=$(xdotool getwindowfocus)
		xdotool windowfocus "$BOOKIEZ"
		xdotool key --window "$BOOKIEZ" i
		xdotool type --window "$BOOKIEZ" "$line"
		xdotool key --window "$BOOKIEZ" Return
		xdotool windowfocus "$CURRENTWINDOW"
		aplay --quiet ~/sound/woop.wav
		echo " sent"
	done
