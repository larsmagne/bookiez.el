bookiez is an Emacs package for keeping track of books you own.  The
basic idea is that you enter ISBNs, and then bookiez queries various
ISBN lookup providers, and then stores the information.

The information is then presented either on a per-author basis:

![](https://lars.ingebrigtsen.no/?p=114764)

And you can list all the books by the author:

![](https://lars.ingebrigtsen.no/?p=114765)

Or you can list all the books in one buffer:

![](https://lars.ingebrigtsen.no/?p=114766)

And you can look at the detailed information about the book:

![](https://lars.ingebrigtsen.no/?p=114767)

bookiez keeps track of the date you bought the book, when it was
published, and when you marked it as read (or "skipped").

To get started, put the bookiez directory in your load path and
autoload the top-level command:

    (push "~/src/bookiez/" load-path)
    (autoload 'bookiez "bookiez" nil t)
	
Then just type `M-x bookiez' to start bookiez.

To enter a new book, either use the `i' command, or just start typing
the ISBN followed by a `RET'.  bookiez will then query the ISBN using
Goodreads, the Google Book API, the OpenLibrary API, and optionally
the ISBNDB API.

If you wish to use ISBNDB, you need to get an API key from ISBNDB, and
set `isbn-isbndb-key' to that key.  See
https://isbndb.com/isbndb-api-documentation-v2.

If you have a lot of books to enter, it's strongly recommended to get
an ISBN barcode scanner.  I've been using a Datalogic QuickScan Mobile
QM2131 for over a decade:

![](https://lars.ingebrigtsen.no/?p=114768)

It's most convenient if it works like a USB HID keyboard and just
outputs the ISBN followed by a `RET', which will make it work
automatically with bookiez.

