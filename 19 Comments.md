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

Yes, I know the indentation is wrong (among a lot of other things)! That said, it's pretty dense, huh ? I'd say it would be tought to undertsand unless you had the coder right
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

