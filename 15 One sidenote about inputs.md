# One sidenote about Inputs

Although our code runs and takes input, there is *one thing* I want to point out. It's insecure.  This code will run exactly as expected, *unless* 
the user enters an unacceptable type of input. Right now, we're just using our directory in our computer, so it's not such a big deal, *but* if you code a 
GUI interface or a web interface and don't make it more secure (which I'll be showing you in a second), you can potentially let anyone have access to
your computer, or the computer you're running your web page / database / mud / wiki  at whatever your access level is! That's not good at all! 
Let's use the program we  just wrote at the end of the last chapter as an example:

```

(defun welcome ()
  (format t "~% Status: Logged In   Agent ID: 007   License to Kill:Active     Messages from M: 0~%~%"))


(defun getchoice3 ()
  (let ((choice 1))
    (format t  "~%Enter your password:  ")
    (let ((response (read)))
      (cond ((equal response '007)
	     (format t "~%~%Welcome James Bond, 007 ~%")(welcome))
	    (t (format t "~%Incorrect Reponse.~%") (getchoice3))))))

(getchoice3)

```

It absolutely runs as expected, *unless* someone decides to enter anything with the **" , "** character in it, even if it's "Bond, James Bond".
The program can't handle that and crashes:



<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Common_Lisp_Crash.png" height="600" width="1200"></a>


While the crash itself is bad as it is, if a clever hacker sees that they can make your program crash, they can get it to exit to the system, spawn a shell and
get access at whatever level you have access on that computer. In the hacker community this is known as "getting root" or "root shelling" and the bottom line
here is, I don't want anyone getting a root shell on anything I've written!  One change is all that's needed to keep that from happening, instead of using
**(read)** to read the user's input, we'll need to use **(read-line)** instead.


## So what's the difference ?

Both functions read data, however the (read) function returns just the text it read, the (read-line) function return the text it read, plus it adds #\newline 
(literally a line feed) at the end of the text or data. This makes it difficult to add nonsense characters or characters that would make the program crash,
which can be used to spawn shells. Even though the (read) command can be made to ignore errors with (read (ignore-errors)), it's still possible to add extra
characters that shouldn't be there and cause problems with the code (like spawning a shell) therefore, it's still a bad idea to use (read) for any input
*except* for input that you have absolute control over. Use (read-input) for any user input, (read) for anything else.

For example, program we wrote above can easily be re-written to use (read-line) like this:

```

(defun welcome ()
  (format t "~% Status: Logged In   Agent ID: 007   License to Kill:Active     Messages from M: 0~%~%"))


(defun getchoice3 ()
  (let ((choice 1))
    (format t  "~%Enter your password:  ")
    (let ((response (read-line)))
      (cond ((string-equal response "007")
	     (format t "~%~%Welcome James Bond, 007 ~%")(welcome))
	    (t (format t "~%Incorrect Reponse.~%") (getchoice3))))))

(getchoice3)

```

This allows us to keep going, even if someone tries to crash the program by entering in **","**.  As you can see, Common Lisp doesn't even bat an eye and keeps
on running:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/choice_corrected.PNG" height="600" width="1200"></a>

