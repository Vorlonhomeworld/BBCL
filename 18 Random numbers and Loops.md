# Random numbers

  Let's say you want to generate a hash for a password, or perhaps simulate a dice roll. To do either of those you need a random number generator. Common Lisp is capable of 
doing this. All you really need to tell it is what number to start at and what number to end at and that's it's to be random and you're ready to go!

```

( + 1 (random 80 ))

```

will produce random numbers from 1 to 80 in the REPL.
