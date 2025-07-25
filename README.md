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

