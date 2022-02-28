# Adventure Game Revisited

If you remember, back in [Chapter 15](https://github.com/Vorlonhomeworld/BBCL/blob/main/15%20More%20inputs.md) we coded a really simple adventure game. It worked, but
it was necessary to code the prompt over and over again.  Wouldn't it be easier to code the prompt *just once*?  Turns out , it is, *but* the game would need to 
recoded a bit.  To demonstrate this, we're going to work through a fully functional adventure game ported to Common Lisp called Dunnet.

## Introducing Dunnet

Dunnet was originally written in 1982 in MacLisp (which is what Elisp (Emacs Lisp) is based on) for the DECSYSTEM-20, then later ported to Emacs, Xemacs and it can also
be found on Macintosh computers. The version we'll be looking at is a version that's been ported to Common Lisp by [Jack Rosenthal](https://github.com/jackrosenthal)
called [dunnet-cl](https://github.com/jackrosenthal/dunnet-cl/blob/master/dunnet.lisp). This is a full featured game which includes items you can pick up, at least 
one *potentially* fatal "monster", a pretty well done parser, a good map, pretty much anything you'd want in an adventure game. 

The first 31 lines are strictly comments. The first line gives you the title of the file, it's not necessary to do this, but it's a good practice to do so!
Lines three and four show the copyrights for the Emacs version of the game and the Common Lisp version.  Lines 6 - 9 credit the original author, state when this was
created , what version of the game this is and list a keyword to assist in finding this game in Github.  Lines 11 through 24 print out the GNU Emacs license.

Lines 26 through 31 are the start of the global variables - these lines are comments to let.   Line 33 is a line of code specifically for sbcl only, any other type of 
Common Lisp would ignore this. Essentially, it's telling sbcl to not print (muffle-conditions) any kind of style warnings.  The "#+" portion specficially tell Common 
Lisp that a function is coming up, and writing it that way is an acceptable shorthand for the word "function".

Lines 35 through 37 are a macro that helps us set up our parser. It's job is to look at anything we type in 
and specifically look for [predicates](https://www.thoughtco.com/what-is-a-predicate-1691010) 
