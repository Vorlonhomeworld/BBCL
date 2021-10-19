# Adventure Game Revisited

If you remember, back in [Chapter 15](https://github.com/Vorlonhomeworld/BBCL/blob/main/15%20More%20inputs.md) we coded a really simple adventure game. It worked, but
it was necessary to code the prompt over and over again.  Wouldn't it be easier to code the prompt *just once*?  Turns out , it is, *but* the game would need to 
recoded a bit.  To demonstrate this, we're going to work through a fully functional adventure game ported to Common Lisp calle Dunnet.

## Introducing Dunnet

Dunnet was originally written in 1982 in MacLisp (which is what Elisp (Emacs Lisp) is based on) for the DECSYSTEM-20, then later ported to Emacs, Xemacs and it can also
be found on Macintosh computers. The version we'll be looking at is a version that's been ported to Common Lisp by [Jack Rosenthal](https://github.com/jackrosenthal)
called [dunnet-cl](https://github.com/jackrosenthal/dunnet-cl/blob/master/dunnet.lisp). This is a full featured game which includes items you can pick up, at least 
one *potentially* fatal "monster", a pretty well done parser, a good map, pretty much anything you'd want in an adventure game.



