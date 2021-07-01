
# Input & Output

Common Lisp has built in commands for reading and writing files.   For example, if I wanted to open a file and write a list of names into that file, for example, I could do
that fairly easily in Common Lisp with the following code:

```

#| Writes a file in Common Lisp |#

(with-open-file (stream "C:/users/micha/common_lisp/my_database.db" :direction :output)
  (format stream "Eisner, Michael  alias King Farqhad")
  (terpri stream)
  (format stream "Torvalds, Linus alias Linux Emperor")
  (terpri stream)
  (format stream "You can even use a straight text format and store it this way too! ")
  )

```

The first line tells Common Lisp that we actually want to open a file and stream to it, then it tells it not only what it's name is, but 
what the full path is to that file name, and that we're outputting the data we're streaming into the file.

The next line uses a format statement, but rather than the typical (format t "Eisner, Michael....") we're changing it to (format stream....)
because we just told Common Lisp that we want to stream data to it. The command we've been using so far (format t "...." ....) won't work in this case.
Once we establish that this is the stream that we want our file to capture, we have to tell it **what** we want it to capture, so we print that data
in quotes.

The next line, *(terpri stream)* literally is a command to insert a carriage return into the stream.  **Terpri** is a concatenation of  "**Ter**minal **pri**nt".
Now, we're using it for *this* example, but to be honest, there are other ways to do this and I'll show you that in a second!  

The remaining lines are a repeat of the two lines above, then we end this file.   

This file will run, but will **not** print anything to the screen *if it's been coded correctly* :


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/write.jpg" height="600" width="950"></a>


To read the data back out in Common Lisp you would code:

```

#| Reads Data form the file file.db.  It's in plain text
|#

(let ((in (open "C:/Users/micha/common_lisp/my_database.db" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
	  while line do (format t "~a~%" line))
    (close in)
    )
  )

```

The first line establishes the **in**put (called "in" on line 1 of this code) as the same file we wrote to, it also runs a check to be sure this file exists where we say 
it does with the ":if-does-not-exist" code, the next 3 lines read through the code line by line, the second line of code (loop for line = (read-line in nil) reads the
lines of input up to an empty section, that section is treated as the end of the file.  It won't treat a blank space (space after a line of text) as the end of the file, it
has to encounter a line with nothing on it for that to occur.

The fourth line tells it while it's reading the line, it needs to print out the characters (the "~a" is Common Lisp's way of stating that it's looking to print ascii characters.
Once it runs into an empty line, it's to consider it the end of the file and then , on the fifth line, it's to close that input.

Running this code produces:



<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/read.jpg" height="600" width="950"></a>


