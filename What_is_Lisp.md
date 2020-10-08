# What is Lisp? 
*(and why should we care)*

**L**isp foundations were laid in the Summer of 1956 (yes, 1956!) in a Darthmouth Summer Research project on AI. During
this meeting, Newell, John & Simon described a language called IPL2 which was a list proccessing language for the 
Rand Corporation's JOHNIAC computer. There were two problems with this language, the first one was that it was based on
a JOHNIAC loader they happen to have for that computer. If no one else had the loader, they couldn't use the language.
The second one was that Fortran was more widely available and was thought to be a better choice for list processing.

However, as they went along developing the language, which was intended to be used for Artificial intelligence, questions
started arising as to whether or not Fortran could really do the job that they wanted it to do conveninetly. At first, 
an attempt was made to develop a list processing language within Fortan. It seemed to them, to be easier, since they 
believed that writing a new compiler would take too long. By the end of 1956 they though they'd achieved what they 
needed with FPLP *(Fortran List Processing Language)*.  However, problems started to pop up in that language, for example
the "IF" statements in Fortan 1 and Fortran 2 were akward to use, that lead to the introduction of a new Fortan function 
to make it easier to use "IF" statements.

In the summer of 1956, John McCarthy, the father of Lisp, spent time upgrading FLPL for greater use, introducing
the maplist function and lambda notation, based on the work of Church in 1941. By the fall of 1958, John McCarthy
began working on a Lisp Compiler as he no longer felt Fortran was a worthwhile language for what he needed. Lisp also became one of the first 
programming language to have it's own interpreter with the implemantation of the "eval" command! From that point on, 
Lisp was programming language!

Lisp began growing and spreading out, and spawning many different dialects:

* MacLisp - this started in 1960 and was a variation of Lisp 1.5 developed at M.I.T for project MAC (no relation to Macintosh computers!)

* Interlisp - this began in 1966 by BBN Technologies in Cambridge Massachusetts for D.E.C PDP-1 computer

* Lisp Machine Lisp, Scheme & Nil - All came out in the middle on 1970.

* Franz Lisp, Common Lisp & Le Lisp came out in the Mid 1980's.

* T, Chez Scheme, Emacs Lisp, Auto Lisp & Pico Lisp -  All came out in 1985.

This isn't even a full list of **ALL** of the Lisps available, just a small list.

Despite it's now wide useage, there was a problem, all of the lisps were not the same, and a lisp written with one kind of lisp
wasn't guaranteed to run on another machine runningn a different Lisp. ANSI *(American Nation Standards Institute)* stepped in in 1984
and developed a standard version of Common Lisp, anyone using it could code Lisp on one machine and that code would
run just fine on another.  That didn't mean the end of the other Lisps, however, some are still very much 
alive and in use, for example, Emacs Lisp is still very much the defacto Lisp of Emacs!

Okay, now we know what it is, what can it do for me!

Pretty much anything you want it to do!  Seriously!   Common Lisp can:

* Write a webserver
* Allow a webpage on that same webserver to be viewable in HTML.
* Make calls back and forth from a database you designed in Common Lisp
* Handle all calls to for usernames and passwords to that same database

You can also:

* Write a window manager (Stumpwm is great example of a window manager written in Common Lisp)
* Write a game (graphic or not!)
* Write system scripts
* Write a shell 

That's just the tip of the iceberg! There's literally nothing Common Lisp *can't* do!  On top of that, it can be run either compiled *or* as a script!

Let's get started and learn!
