# Defun

Okay, enough with the numbers for a while! Time to get back to coding. This next concept is one of the building blocks of Common Lisp. It allows you to create functions
and then later call them at will. This gets used instead of goto statements, and what's more, it's pretty easy.  I'll show you an example of this so you
get the idea. In the last chapter I showed you the **exp** function, well, in this chapter we're going to create a small (very small) program in the REPL
using that function called "Euler's Calculator":

```

(defun uc (a)
(exp a))

```

The **defun** function allows us to **DE**fine the **FUN**ction we're using by first giving it a name. The format for the defun function is:
**(Defun** *function name* *( function variable - **or** it can be left blank))*. for example, I gave my function the name "uc" for Euler's Calcuator.
I then gave it the variable name **(a)** , now that variable name is *optional only*, it depends on what I need it to do. In this case it was easier
to give the function a name, then call that function again on the next line. If the function were bigger or it called other functions, it would
be easier to leave the variable name blank. Now I want you to notice that we've created a nested list at this point. Since it hasn't closed yet, we can continue to 
enter a second or even a third line in the REPL. Once the list is closed the REPL won't allow us to add anything further to this function, that's why this list is 
unclosed on the  first line.

The second line shows us calling **exp** and instead of giving it a number, we're telling it to multiply Euler's number by **a**. You're probably wondering how
this is going to work since "a" has no definition other than a, right?  Keep watching, all will be explained!  Now, we close this loop at the end and Common Lisp's
REPL responds to us to let us know it understood what we asked it to do:

```
UC

```

That's it telling us that the function is defined.  Now , if you mess up, don't worry, just go back up where the code is on this page, and retype it in.
Common Lisp will respond slightly differently, but it still means the function has been defined:

```
WARNING: redefining COMMON-LISP-USER::UC in DEFUN
UC

```

It's telling us that the function has been changed, but it still recognizes it as a function!

OK, now we get to use it by calling it back and adding in a number (which is what "a" will now equal):

```
* (uc 4)    **[Hit Enter]**
54.59815

```

You entered in **(uc 4)** and Common Lisp substituted **(defun uc (a)**  with **(defun uc (4)**, which changed the next line from **(exp a))** to **(exp 4))**. At that 
point, all you had to do was hit enter and that was it!  It's just that easy in Common Lisp!

