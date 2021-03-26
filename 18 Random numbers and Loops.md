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

There's one little *gotcha* that you *do* have to be aware of with this, the first number that you use can't just be a number, it has to be either a positive number ( + 1
, + 2  ...etc...) or a negative number ( -1, -2, ...etc...).  Putting in *just* the number, for example:

```

( 1 ( random 80)) 

```
 
 won't work, even though it's *technically* a positive number. The only thing you'll get out of it is an error:
 
 
 
 <a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/random_error.jpg" ></a>
 
 You absolutely *have* to include the plus or minus next to the number in order for this to work.

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

  The (setf \*random-state\*....) won't be seen or run, so this will give me the same value every time I run it. The reason for this is that computers are *deterministic* by 
nature, that is, the first step determines the next step. For example if we tell the computer we're counting and start with "1", it would likely determine the next step would 
be to count "2".  With \*random-state\* used, it changes that first step every time based an algorithm  that's part of that function. I also call random state as 
"\*random-state\*" rather than as just "random-state" so that I can call this from anywhere in the program. I'm also using the \*random-state\* function to tell 
(make-random-state) what to use to create that randomness as well.

  The (make-random-state t) is a *boolean* function, that is, it's value can either be true or false (in Common Lisp, the word *nil* is sometimes used for false) only.
In this case, I've set (make-random-state) to true by including the letter "t" on the end. If I wanted it to be false I would use "nil" on the end of it , which would 
be displayed as (make-random-state nil). By telling make-random-state that it's value is true it means that it's making a random state of a random state. Had we set it to nil 
it would have run and the numbers would have appeared to be random, however, this pattern would then repeat, with exactly the same numbers, so it wouldn't be truly random.

  The (defun dice ()  statement  creates the function (dice) and sets it up without any input. Notice that the statement (defun dice () isn't closed yet? That's because even
though we're not giving it an input, we *will* be giving it an operation to perform on the next line, the (+ 1 (random 80 ))) operation I mentioned at the top where
we tell it to give us a number from 1 to 80 randomly. 

  What follows that line are three print statements - admittedly, this is a bit fancy, but why not?  The top line prints out  "Dice 1: " with a carriage return added to the
end (that's the "~%" at the end of the line. The second prints out "+----------------------------------------------------------------------+ " with two carriages returns at 
the end. Finally, the last line prints out the randomly generated number for the dice and prints it, the "~A" is the placeholder where we want the number to print. We 
follow that up with a carriage return and once we've closed the quote we add in "(dice)" to tell "~A" what value it's printing.

  We can make our dice roller roll a die 5 times in a row easily and with very few lines of code if we incorporate a loop within it.  A loop is a set of instructions that tell
Common Lisp to repeat an instruction a set amount of times. Without the loop, we'd have to code five repeating lines of format t statements, but with a loop, we only have to 
code it once.  In this example, we'll roll our die 5 times:

```

(setf  *random-state* (make-random-state t))

(defun dice ( )
  (+ 1 (random 20)))

(loop for x from 1 to 5
   do (format t "~% ~A~%" (dice)))


```

This produces:

<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/dice_throw.jpg" ></a>

  We still start off with setting up "\*random-state\*" and  we still tell it to "make-random-state t" just as before and we still define our function "dice". Now we can call it
something else if we wanted, but because this is a dice thow, it's easier to call it a name that makes it easy for us to remember why we're using this. We *do* change the random
number we want to produce to be anything from 1 to 20. At this point, we set up our loop.

  Common Lisp loops start off by declaring the loop function right away, it also does one other thing, it takes a shortcut in the syntax. Most other coding language would
code this loop as:

```

for x = 1 to 5
  next x

```

In Common Lisp the "equals" part of the loop is understood to exist, so it doesn't need to be written out, it's enough to tell it that "x" runs from 1 to 5. Additionally, we 
don't have to code the "next x" portion either, Common Lisp undertands that both are there so we don't have to tell it, just that one line on the top is enough. By the way,
"x" is a variable that we hadn't delcared up to this point, and that's perfectly ok within the loop, it serves as a place holder for a varaible we'll call shortly.


  The **"do"** statement on the next line *absolutely has to be there* otherwise, Common Lisp won't run, even if everything else if coded correctly. This statement tells
Common Lisp what it's supposed to be **do**ing for 5 times, that is, it's supposed to print a carriage return ("~%") , then it's going to print something that's going to 
be defined later ("\~A") then another carriage return ("\~%"), the placeholder we put in earlier ("\~A") turns out to be our random dice throw, which prints 5 times.


  

