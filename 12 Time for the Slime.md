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
Besides that, Vlime runs a swank server only when it needs to, Slimv keeps the Swank server running constantly (I'll explain what that is in a moment!). For a better
(and for that matter, more complete comparison of the two, please [check out this page](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/#comparison-of-slimv-and-vlime) as
it's got a pretty extensive comparison of the two!
