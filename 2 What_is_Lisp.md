# What is Lisp? 
*(and why should we care)*

**Lisp** is a portmanteau (a blending of two words) that described it's primary function **LIS**t **P**rocessing. It's founder,
John McCarthy was an AI (artifical intelligence) researcher and he discovered that the programming languages available at that 
time just weren't powerful enough to solve complex issues like [Automated Theorm Proving](https://en.wikipedia.org/wiki/Automated_theorem_proving), 
[Computer Vision](https://en.wikipedia.org/wiki/Computer_vision) and other logic operations. Bear in mind, the computers in that
day were different, for example, John McCarthy started working with a [JOHNNIAC](https://en.wikipedia.org/wiki/JOHNNIAC) computer which
filled up a whole room, had vacuum tube [Selectron tubes](https://en.wikipedia.org/wiki/Selectron_tube) for memory, each one was capable
of holding only 32 bytes (256 bits) of memory each, so space was tight, to say the least!  So the language needed had to be compact and
had to be capable of running on 32 bytes of memory and had to be able to perform powerful calculations.


Lisp foundations were laid in the Summer of 1956 (yes, 1956!) in a Darthmouth Summer Research project on AI. During
this meeting, Newell, John & Simon described a language called IPL2 which was a list proccessing language for the 
Rand Corporation's JOHNIAC computer. There were two problems with this language, the first one was that it was based on
a JOHNIAC loader they happen to have for that computer. If no one else had the loader, they couldn't use the language.
The second one was that Fortran was more widely available and was thought to be a better choice for list processing.

However, as they went along developing the language, which was intended to be used for Artificial intelligence, questions
started arising as to whether or not Fortran, which is the language they were using at that time, could really do the job that they 
wanted it to do conveninetly. At first, an attempt was made to develop a list processing language within Fortan. It seemed to them, to be easier, since they 
believed that writing a new compiler would take too long. By the end of 1956 they thought they'd achieved what they 
needed with FPLP *(Fortran List Processing Language)*.  However, problems started to pop up in that language, for example
the "IF" statements in Fortan 1 and Fortran 2 were akward to use, that lead to the introduction of a new Fortran function 
to make it easier to use "IF" statements.

In the summer of 1956, John McCarthy, the father of Lisp, spent time upgrading FLPL for greater use, introducing
the maplist function and [lambda notation](https://en.wikipedia.org/wiki/Lambda_calculus), based on the work of [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church)
in 1941. By the fall of 1958, John McCarthy
began working on a Lisp Compiler as he no longer felt Fortran was a worthwhile language for what he needed. Lisp also became one of the first 
programming language to have it's own interpreter with the implementation of the "eval" command! From that point on, 
Lisp was programming language on its own!

Lisp began growing and spreading out, and spawning many different dialects:

* MacLisp - this started in 1960 and was a variation of Lisp 1.5 developed at M.I.T for project MAC (no relation to Macintosh computers!)

* Interlisp - this began in 1966 by BBN Technologies in Cambridge Massachusetts for D.E.C PDP-1 computer

* Lisp Machine Lisp, Scheme & Nil - All came out in the middle on 1970.

* Franz Lisp, Common Lisp & Le Lisp came out in the Mid 1980's.

* T, Chez Scheme, Emacs Lisp, Auto Lisp & Pico Lisp -  All came out in 1985.

This isn't even a full list of **ALL** of the Lisps available, just a small list.

Despite it's now wide useage, there was a problem, all of the lisps were not the same, and a lisp written with one kind of lisp
wasn't guaranteed to run on another machine running a different Lisp. ANSI *(American Nation Standards Institute)* stepped in in 1984
and assisted in the development of a standard version of Common Lisp, anyone using it could code Lisp on one machine and that code would
run just fine on another.  That didn't mean the end of the other Lisps, however, some are still very much alive and in use, for example, 
Emacs Lisp is still very much the defacto Lisp of Emacs!

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

**Common Lisp**, itself is a general purpose language and an Artificial intelligence language, that's why McCarthy designed it so many years ago. However,
it's more than that. It's object oriented, all code is reusable and portable, meaning it can be coded on a Linux machine, and run on a Window or Linux machine with no change 
to the code itself (one caveat - as long as it's a script this is true, compiling it changes this!). It can be run either as a script or compiled. It understands the
differences between an integer, a character, a float, a long float and a decimal with no assistance from the programmer needed.

For example, in C, if I were to write code for a program that used integrs, I'd have to declare them and tell C to include the standard input-output system first:

   ```
   #include <stdio.h>
   Def Int(balance) 
   ```
   
   Before I could even code what I wanted the program to do, in Common Lisp, those two lines are totally unecessary, Common Lisp already knows what 
   standard input and output is and integers can be coded on the fly if need be! Further, Common Lisp can be used to make calls to the processor directly.
   It's also extremely expandable, in fact [there are a ton of libraries](https://www.quicklisp.org/beta/releases.html) listed on this page that expand out
   the capabilities of Common Lisp greatly. It's almost as if Common Lisp were the programming language equivalent of Emacs - everyhthings there, and if you 
   can't find what your looking for, you can code it in and add it!

## So do people still use it? ##

As a matter of fact, yes!  Common Lisp is still being used today, by both hobbyists and professionals. On the Hobbyists side, [StupmWm](http://stumpwm.github.io/)
is a complete window mannager written completly in Common Lisp, [Climacs](https://common-lisp.net/project/climacs/) is an implementation
of Emacs in Common Lisp, as are [Hemlock](https://github.com/bluelisp/hemlock) and [Clocc](http://clocc.sourceforge.net/). On the companies side,
there's actually a fairly large list [available here](https://common-lisp.net/lisp-companies), this list includes Grammarly!  This isn't the only list either,
[there's this one](http://pchristensen.com/blog/lisp-companies/) and you can always do a google search on Lisp jobs anytime!


   With that in mind, let's keep on learning!
