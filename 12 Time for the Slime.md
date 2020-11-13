# Time for the Slime

**N**ow that we're starting to work on code, we need to get a slightly better environment to work with. Don't get me wrong, the REPL is great for typing in 
one list and testing it, but once we start working on bigger programs, we'll need something a bit better for us so that we can test the program to see if it's 
going to do what we want it to do, and for that we have two options for Emacs users and two for Vi users.

## Vi Users

Vi users can user Slimv or Vlime.

**[Slimv](https://github.com/kovisoft/slimv)** stands for **S**uperior **L**isp **I**nteraction **Mode**  for **V**im. It's a Vim plugin created by Tamas Kovacs.  

**[Vlime](https://github.com/vlime/vlime)** doesn't stand for anything, and is links Common Lisp with Vim.

Of the two, Slimv is a bit older, it was first released in January 2009, Vlime was first released in May of 2017.  Vlime is still considered a Beta
version at this time, Slimv appears to be out of Beta, however it hasn't been updated since 09/2019, the most [recent version is 0.9.14](https://github.com/kovisoft/slimv).
Both Slimv and Vlime rely on **SWANK**.   Slimv comes with it's own version of Swank which it can run at will. Vlime has a wrapper, that is a program that knows
how to download Swank when needed. Now that's not the only difference between the two version, if you want to see a complete list of what those differences are, feel
free to [check out this page](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/#comparison-of-slimv-and-vlime).

## Emacs users

Emacs users can use **Sly** or **Slime**.

**[SLIME](https://github.com/slime/slime)** stands for **S**uperior **L**isp **I**nteraction **M**ode for **E**macs. It started life as **SLIM** back in 2003,
an Emacs extension written by Eric Marsden. Luke Gorrie and Helmut Eller then took over and turned it into **SLIME**

**[SLY](https://github.com/joaotavora/sly)** is SLY is Sylvester the Cat's Common Lisp IDE for Emacs. This is a fork of SLIME. This came about because the creater 
of Sly felt that Luke Gorrie and Helmut Eller and everyone else that was coding SLIME wasn't taking care of the bug  reports in a timely manner, and they also
had their own hacks ("hacks" means code) that they wanted to share with everyone else.

Both Slime and Sly use SWANK, however, Sly calls their version SLYNK. The main difference between SLIME and SLY is that SLIME leans toward being a conservative 
build, thus focus is on stability, whereas SLY likes to be cutting edge and sometimes this causes SLY to crash.  I've used both and find SLIME to be 
the more stable of the two.

## Ok, but what the heck's SWANK?

Swank is a [wire protocol](https://en.wikipedia.org/wiki/Wire_protocol) that allows the Common Lisp REPL to run inside Emacs. This gives us the ability to write code using the 
REPL and we can test that code without having to leave emacs and with the code still visible to us.  It's literally the heart of SLIMV, VLIME, SLIME and SLY!
