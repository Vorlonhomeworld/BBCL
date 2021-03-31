# Comments 

  So, why worry about comments, I mean, if you know the language wouldn't that make *any* Common Lisp program easy to understand?  I'd say it depends on the program! What would 
you think if you saw this ([courtesy of this webpage](https://forum.dvdtalk.com/tech-talk/419224-lisp-study-obfuscation.html) ):

```
(defun evaluate_program (x)
(do ((validp (not (null x)) (and validp (not (equal t (setq x (evaluate_statement x)))))))
((atom x) (if validp "(Yes this is a legitimate program.)" "(No this is not a legitimate program.)"))
)
)

```

Yes, I know the indentation is wrong (among a lot of other things)! That said, it's pretty dense, huh ? I'd say it would be tought to understand unless you had the coder right
there with you to walk you through it. Don't ever code like that, *it's wrong*. If anything, you want  your code to be understood, after all, you might not be there to explain
it, you might have coded an error into the program and someone else has to fix it...etc.   Always comment your code, even if you never intend to share your code, you *yourself*
might actually forget why you coded what you coded. Comments make it *so* much easier!
  
  Common Lisp has two types of comments, and an accepted style for using them.  The first are single line comments, marked with semi-colons (**;**). Common Lisp also has 
multi-line comments that start with a pound sign followed by a bar (**#|**) and end with the reverse of this sign, the bar then the pound sign (**|#**). There's also an 
accepted way to use the semi-colons:

**;;;;** At the top of the file

**;;;** At the beginning of the line of code

**;;** Any indented code gets two semi colons

**;** At the end of a line of code.

A great example of this [is found on StackOverflow](https://stackoverflow.com/questions/6365334/lisp-commenting-convention):

```

;;;; At the top of source files

;;; Comments at the beginning of the line

(defun test (a &optional b)
  ;; Commends indented along with code
  (do-something a)                      ; Comments indented at column 40, or the last
  (do-something-else b))                ; column + 1 space if line exceeds 38 columns
  
  ```
  
  A multi-line set of code can be wrapper up in between **#|** and **|#**, for example:
  
  ```
  
  #| This is a multi-line set of code. This is use to either explain what variables
     are being used and why or it's to insert a license or a note to anyone else
     that tries to modify the code without fulling understanding it. This beats having
     to write ;; on each line , huh ? |#
     
```

For example, the DND dice roller code I showed you [on chapter 18](https://github.com/Vorlonhomeworld/BBCL/blob/main/18%20Random%20numbers%20and%20Loops.md) should really
have been commented like this:

```

;;;; DND Dice Roller

#| DND Dice Roller in Common Lisp
 This rolls one d20 for the following stats
 Strength
 Intelligence
 Dexterity
 Wisdom
 Charisma |#

;;; set up our random state  - this makes our dice completely random)
(setf  *random-state* (make-random-state t))

;;; Speaking of dice - here they are  now !
(defun dice ( )

 ;; Our dice will roll from 1 to 20
  (+ 1 (random 20)))

;;; We want to set up the following routine to roll 3 set of strength, intelligence, dexterity, wisdom and Charima rolls
(loop for x from 1 to 3 ; increase the number 3 for how every many players you have on hand

   ;; print each attribute plus the dice roll (Str: Int: Dex: Wis: Cha: plus a carriage return after each attribute
   do (format t "~%~%Str: ~A~%Int: ~A~%Dex: ~A~%Wis: ~A~%Cha: ~A~%~%" (dice)(dice)(dice)(dice)(dice)))
   
```

It makes the program longer, to be sure, *but* each line is clearly understood , you'll never have to worry about forgetting what each of the
functions do, and it's less confusing for everyone involved!



## Document Strings


  There's another form of documentation used in Common Lisp known as "Documents strings" or "DocStrings". Docstrings are a bit different than the comments above.
Comments are meant to be read in the code and docstrings are meant to be extracted out of the code either through an API (**A**pplication **P**rogramming **I**nterface)
or through the repl. The Docstrings are written differently and tend to be a bit more terse as programmers are reading these.

A Docstring is written with quotation marks on the front and the back, for example:

```

(defun myfunction () 
  "This is my function There are many like it This one is mine" (foo)
  )

(defun animalMother ()
  "Born to kill" (foo)
  )


```

We start off by defining our function, in this case called myfunction, ours is a blank function. We leave it opened and on the next line we enclose our docstring 
("This is my function.....") in quotes then we end it by closing the parenthesis.  We do the same thing with the function animalMother.  While I can read this in the source 
code, I can also pull this out in the REPL (or SLIME) by making a request for that documentation by loading this code in first. On my computer, this script is called 
"fmj.lisp".  I would load it in by entering in:


```


sbcl --load fmj.lisp


```

This would load up the REPL (or SLIME if I'm using that).  I would then call up the documentation like this:


 <a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/docstring.jpg" ></a>




