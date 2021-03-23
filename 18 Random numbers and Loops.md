# Random numbers

  Let's say you want to generate a hash for a password, or perhaps simulate a dice roll. To do either of those you need a random number generator. Common Lisp is capable of 
doing this. All you really need to tell it is what number to start at and what number to end at and that's it's to be random and you're ready to go!

```

( + 1 (random 80 ))

```

will produce random numbers from 1 to 80 in the REPL.  If you just enter in:

```

(random 80)

```

You'll still get random numbers, but one of them will be zero. If you're okay with zero being one of the random numbers, no problem, you can write (random 80), 
otherwise, use ( + 1 (random 80)) to rule out getting zero ever.

  It works that way even outside the REPL, for example, if I were to code a simple Common Lisp script that threw a die (single dice) that could roll anything from 1 to 80,
I would code it like:  

```

;; random dice throw

(setf  *random-state* (make-random-state t))

(defun dice ()
  (+ 1 (random 80 )))
(format t "Dice 1:~%")
(format t "+-------------------------------+~%~%")
(format t "~A~%" (dice))

```

Notice that (+ 1 (random 80) is still there, but we've had to add more to it. First we have to call the variable **\*random-state\***.  

  [Random-state](https://quickref.common-lisp.net/random-state.html#Introduction) is a set of somewhat random number generators. This part of the code is critical, without it
the code *only* creates one random number and it will always be the same, no matter how many times you run the program, *even IF every other part of the code is correct*.
For example, if I comment out the second line of this code like this:

```

;; random dice throw

;;(setf  *random-state* (make-random-state t))

(defun dice ()
  (+ 1 (random 80 )))
(format t "Dice 1:~%")
(format t "+-------------------------------+~%~%")
(format t "~A~%" (dice))


```

  The (setf *random-state*....) won't be seen or run, so this will give me the same value every time I run it. The reason for this is that computers are *deterministic* , that
is, the first step determines the next step. For example if we tell the computer we're counting and start with "1", it would likely determine the next step would be to 
count "2".  With \*random-state*\ used, it changes that first step every time based an algorithm  that's part of that function.
  I also call random state as "\*random-state\*" rather than as just "random-state" so that I can call this from anywhere in the program. I'm also using the \*random-state\* 
function to tell (make-random-state) what to use to create that randomness as well
