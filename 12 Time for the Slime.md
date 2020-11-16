# Time for the Slime

**N**ow that we're starting to work on code, we need to get a slightly better environment to work with. Don't get me wrong, the REPL is great for typing in 
one list and testing it, but once we start working on bigger programs, we'll need something a bit better. We're going to need an **I.D.E**. An IDE is an **I**tegrated
**D**evelopment **E**nvironment. This will let us code full-length programs and test them with the code still open. Almost none of the Common Lisp distributions come 
with I.D.E's of their own, except the commercial divisions, however, that doesn't mean we're out of luck! If you're running Emacs, you have three choices for Common Lisp
I.D.E's, if you're running VIM you have two I.D.E's you can choose from!  

The IDE's work pretty much the same way (except for one, and I'll point that one out to you !), they allow you to run a Common Lisp REPL inside of Emacs (or Vi), this, 
in turn, allows you to test the code you're working on while your working with, like in the example below:


<a href="rel"><img src="https://raw.githubusercontent.com/cuichaox/visual-cells/master/demo/slime-screenshot.png" height="600" width="950"></a>


The window on the left is the actual code, and the window on the right is the result of that code. If that code had an error, we'd see the REPL and it would 
let us know what exact code is causing the problem, just as it did when we just ran the REPL by itself. With that in mind, let's get into the actual I.D.E's themselves.


## Vi Users

Vi users can user Slimv or Vlime.

**[Slimv](https://github.com/kovisoft/slimv)** stands for **S**uperior **L**isp **I**nteraction **Mode**  for **V**im. It's a Vim plugin created by Tamas Kovacs.  

**[Vlime](https://github.com/vlime/vlime)** is also a plugin for VIM users and it allows VIM users to turn VIM into an I.D.E for Common Lisp.

Of the two, Slimv is a bit older, it was first released in January 2009, Vlime was first released in May of 2017.  Vlime is still considered a Beta
version at this time, Slimv appears to be out of Beta, however it hasn't been updated since 09/2019, the most [recent version is 0.9.14](https://github.com/kovisoft/slimv).
Both Slimv and Vlime rely on **SWANK**.   Slimv comes with it's own version of Swank which it can run at will. Vlime has a wrapper, that is a program that knows
how to download Swank when needed. Now that's not the only difference between the two version, if you want to see a complete list of what those differences are, feel
free to [check out this page](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/#comparison-of-slimv-and-vlime).

## Emacs users

Emacs users can use **ILISP**, **Sly** or **Slime**.

**[ILISP](https://sourceforge.net/projects/ilisp/)** is one of the earliest Common Lisp IDE's for Emacs. It's last update was in 2002 and is said
to have been superceded by Slime.

**[SLIME](https://github.com/slime/slime)** stands for **S**uperior **L**isp **I**nteraction **M**ode for **E**macs. It started life as **SLIM** back in 2003,
an Emacs extension written by Eric Marsden. Luke Gorrie and Helmut Eller then took over and turned it into **SLIME**

**[SLY](https://github.com/joaotavora/sly)** is SLY is Sylvester the Cat's Common Lisp IDE for Emacs. This is a fork of SLIME. This came about because the creater 
of Sly felt that Luke Gorrie and Helmut Eller and everyone else that was coding SLIME wasn't taking care of the bug  reports in a timely manner, and they also
had their own hacks ("hacks", in this case, means original code for SLIME) that they wanted to share with everyone else.

Both Slime and Sly use SWANK, however, Sly calls their version SLYNK. The main difference between SLIME and SLY is that SLIME leans toward being a conservative 
build, thus focus is on stability, whereas SLY likes to be cutting edge and sometimes this causes SLY to crash.  I've used both and find SLIME to be 
the more stable of the two.

## Ok, but what the heck's SWANK?

Swank is a [wire protocol](https://en.wikipedia.org/wiki/Wire_protocol) that allows the Common Lisp REPL to run inside Emacs. It works by starting a network session 
on port 4005 (that's it's default port to use, but it can be made to use a port of your own choosing!) and within that session, it opens up the REPL, allowing you
to send commands to and from it. As a result, it allows you to test code that's being written in Emacs.  (* Just a note, I'm more familiar with Slime so I'll be referring
to it more than Sly, Ilisp, Slimv or Vlime.).  

Not only can Swank allow you to communicate with the Repl through Emacs, but SWANK can be used to communicate to a remote computer running a Common Lisp session or, if you
happen to use StumpWm, you can communicate directly with the Window Manager in Common Lisp and make any kind of change you'd like to that Window manager!


This gives us the ability to write code using the 
REPL and we can test that code without having to leave emacs and with the code still visible to us.  It's literally the heart of SLIMV, VLIME, SLIME and SLY!
