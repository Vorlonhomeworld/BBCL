# Basic Decisions

**S**o far, we've had Common Lisp print things for us, now we'll get into Common Lisp's decision making ability. We'll start simple and build
out from there.  First, we'll look at the **Let** and **Case** functions.

**Let** allows us to set up a variable to equal something, for example if I wanted to set up a program that asked for a password, I could use the **Let** function
to set up the password to be a certain passsword *(just a quick note here, this is a REALLY insecure way of doing this and it's only being used as an
example - don't ever do this in real life!)*.  The format would be:

```

(let ((password :sherlock))

```

Notice that this list is still opened, the word "let" starts the list, password :sherlock is a sub list with two parentheses on one side and two on the other, but
we still have one paranethesis that remains opened, that's because we need to fill in the rest of this example, if we close it, we may not be able 
to refer back to it with the set of commands that actually verify this is the correct password! Note that this is a very simple example which will not
take user input - we'll expand on this and make it take user input later on, but for now, we'll have it loop through three or more 
possible choices of password without user input.  

To get it to compare our password with the next set of passwords, we'll use the **Case** command. The case command runs through our choices and
sees if any of these choices match our password. Notice that it's indented. It's formatted that way to show that there are two loops (a list inside a list) 
running together.  With one list being indented, it makes it very easy to see that the indented list is a sub-list (in otherwords, it depends on the first list) 
of the first list. Please indent it, it makes it easier to find and debug!  

The case loop looks like this:

```
(case password
  (:enter "Ha ha! Not even close!")
  (:password "Usually, this is a good password choice for a typical windows admin, however, we're not, so it's not right!")
  (:p455w0rd "No L33t speak, please!")
  (otherwise "Please try again!")))

```

This gives our case loop 3 passwords to work with along with the answers we want the program to give, we also have an answer if the password used doesn't match
any of the ones we provided for it.

In order for this to work correctly, the let command and the arguments that go along with it have to be in the same list. If any part of the let loop is outside the list, you'll 
get errors on this.  Let me explain this differently:

**(** let *((password :sherlock))*             < - The ((password :sherlock)) is opened and closed, however, notice the word "let" is still opened?

*(* case password                              < - The "case" statement is also opened, so at this point we need two additional parentheses to close this list  up

  *(* :enter "Ha ha! Not even close!" *)*      < - This opens and closes on the same line - it's a list within a list within another list!    
  
  *(* :password "Usually, this is a good password choice for a typical windows admin, however, we're not, so it's not right!") < - This also opens and closes on the same line.
  
  *(* :p455w0rd "No L33t speak, please!")  < - This also opens and closes on the same line too!
  
  (otherwise "Please try again!")*)* **)**  < - The second parenthesis closes the statment that started with "case", the third parenthesis closes the "let" loop completely!
  
  
What this shows us is that you can have lists inside lists as long as the main list remains open. If I were to attempt to close one portion of this and run it, I'd get an 
error, even though the text is exactly the same as before:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/choice_error.jpg" height="600" width="1200"></a>

This is because Common Lisp has global and local variables - the variable "password" is considered local and therefore it only exists within the list. We haven't yet 
talked about global lists, but we will later on. For now, be aware that local variables exist only within the one list, global variables exists througout the duration of the
program and they can be called from any list within the program.

Running the correct code in SLIME gives us an answer of "Please try again!" because none of the password choices match the password we set at the top with our let function:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Repl_password.png" height="600" width="950"></a>

This works, of course, but what if we want to get input from the user first? We can do that, with a few changes to the code. The first and biggest change to our code would be 
to use (read-line) rather than (read) to get the user's input. The second change would be to change  ((equal response '007 ) to ((string-equal response "007").

The reason for this is while (read) works and can make decisions for us, it's *really* insecure. I mean, as long as the person entering input does what we *expect* them
to do, the code will run and we'll have no problems, however, let's say the user decides to enter "Bond, James Bond" as the password, our program won't like it and it'll crash:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Common_Lisp_Crash.png" height="600" width="1200"></a>

Specifically, the **","** character is the cause of the crash, because Common Lisp interprets that character as an escape character and it will cause the program to crash. Yes,
I realize we're on our own computer so a crash isn't a big deal at *this* stage of the game, *however*, once we start coding for real, that can become a real problem! If we 
code a web interface and we use (read) to accept user input and they enter in anything with **","** in it, the program will crash and the user may be left with a terminal
that has exactly the same access we have on that system! Fortunately for us, Common Lisp has been around the block a few times, so it has tools to help us out with this.

If we switch (read) to (read-line), that stops the problem right away because [(read)](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm#r) runs the input
through the Lisp parser, [(read-line)](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm) doesn't use the parser, it stores what you gave it and compares it 
to the string you tell it to compare it with (which is why we have to change ((equal response) to ((string-equal response)) on the next line so that the string our user 
inputted has something to compare with!

```

(defun getchoice3 ()
  (let ((choice 1))
    (format t  "~%Enter your password:  ")
    (let ((response (read-line))
      (cond ((sting-equal response "007")
	     (format t "Welcome James Bond, 007 ~%" ))
	    (t (format t "Incorrect Reponse, session terminated!"))))))

(getchoice3)

```

Notice that we define our function, **getchoice3** first, ( **(defun getchoice3 ()**), we call it getchoice3 because we *are* asking the user to choose a password, now 
we can call this  *a*, *a1*, *b*..etc..., but it's easier on us if we give this a name that reflects what it's doing. It makes it easier for us to find and debug
this if we need to or make quick changes to it (and we'll be doing just that in a moment). Notice also, after getchoice3 we have an empty set of parantheses? This is called
an empty list and it's perfectly valid in Common Lisp. It's empty because we're going to be reading a response later and working with it. 

The **let** command  assigns the variable "choice"  *temporarily* to "1". This is because the let command requires a variable and that variable requires something
be assigned to it. We're still using **format t** to print out the request for the password, but just underneath, notice that we're asking Common Lisp to read the response?
It looks similar to the let list above it where we assigned the variable of "choice" to it, doesn't it? Although it looks similar, there's a key difference here, on this line,
the function "read-line" at the end means that Common Lisp is now reading what we just typed in. The word "response" isn't a function, it's a variable as well, and it's used 
to show that we're reading the **response** from the user. That's all that's needed for Common Lisp to read what we just entered in for our password.

Below that, (***cond ((string-equal response "007")***), that's us telling Common Lisp that "if the response equals 007 we do the next line below it, (the *(format t 
"Welcome James Bond, 007 ")*. The next line starting with *(t (format.....))* that's our "else" statement (as in if the password is right do this, else do that instead!).  
The (getchoice3) line by itself tells Common Lisp to run the defined function called getchoice3 at the top. If that wasn't there, we could still call this code but it 
wouldn't run!

So let's go a head and call it and make sure it works!   We can run this in SLIME by pressing Ctrl-C Ctrl-K and we'll see this:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/REPL_choice.png" height="600" width="950"></a>


The **CL-User>** prompt doubles as an input prompt **only** for the first password attempt, any time after that, the prompt is correct. 

You could also start a terminal in Emacs, if you prefer, by clicking Meta (that's the Alt key) + x then type in **"term"**. Hit enter and
Emacs will ask you which terminal you want to to start, and it may show your default terminal.  Once the terminal is selected, just go 
to the directory your Common Lisp lives in and type in:

``` sbcl --script choice.lisp ```

(choice.lisp is the name it has on my computer - you can name it something different if you wish! ).   

The computer respopnds with:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/REPL_choices_terminal.png" height="600" width="950"></a>


In looking at the code, it **does** work, but shouldn't we give the user another chance if the get it wrong?  Maybe they just hit the wrong key, it happens to 
us all from time-to-time, right?  Turns out, that's easily done, all we have to do is tell it to run getchoice3 if the password's wrong:

```

(defun getchoice3 ()
  (let ((choice 1))
    (format t  "~%Enter your password:  ")
    (let ((response (read-line)))
      (cond ((string-equal response "007")
	     (format t "Welcome James Bond, 007 ~%" ))
	    (t (format t "Incorrect Response!") (getchoice3))))))

(getchoice3)

```

Since we called our function getchoice3, calling it after an incorrect reponse would sent us right back to the top. Sounds good, right?  Let's test it to be sure, remember, Ctrl-C, Ctrl-K gets this code to run:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/REPL_choices2.png" height="600" width="950"></a>


OK, so now we have an option if the wrong password is entered, but what about if it's correct?  Once again, not too difficult, we just add that list
into our code and we're good to go:

```
(defun welcome ()
  (format t "Agent logged in. Status: Active, License to Kill: Active.~%~%"))


(defun getchoice3 ()
  (let ((choice  1))
    (format t  "~%Enter your password:  ")
    (let ((response (read-line)))
      (cond ((string-equal response "007")
	     (format t "~%~%Welcome James Bond, 007 ~%")(welcome))
	    (t (format t "~%~%Incorrect Response!~%~%") (getchoice3))))))
(getchoice3)

```

Here we're adding in a list (or a subroutine if that makes more sense for you) called welcome. It's an empty list because we're going to define what it does
later on in the code. At the end of it, we have two carriage returns (that's what the "~%" is for, remember! ).  So, we can run this in SLIME (it will
generate a few errors, but the code really does run, I promise you! ) and get:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/REPL_2_loops.png" height="600" width="950"></a>


By the way, if your (welcome) list shows up above the banner "Welcome James Bond" you may have a mis-placed parenthesis somewhere in your program. Double check it and make 
sure your parentheses are correct! I actually had this issue occur when I was coding this the first time - so if you *do* have a missing or incorrectly closed parenthese, don't 
worry, you're in good company!














