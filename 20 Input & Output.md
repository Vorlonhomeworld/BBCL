
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

The fourth line tells it while it's reading the line, it needs to print out the characters while it's seeing them, up to the end of the file (the "~a" is Common Lisp's way of
stating that it's looking to print ascii characters). Once it runs into an empty line, it's to consider it the end of the file and then , on the fifth line, it's to close 
that file.

Running this code produces:



<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/read.jpg" height="600" width="950"></a>


Just in case you're wondering, **yes** you can run this code without the terpri function. Common Lisp will totally let you run the code like this:

```

#| Writes a file in Common Lisp |#

(with-open-file (stream "C:/users/micha/common_lisp/my_database1.db" :direction :output)
  (format stream "Eisner, Michael  alias King Farqhad")
;  (terpri stream)
  (format stream "Torvalds, Linus alias Linux Emperor")
;  (terpri stream)
  (format stream "You can even use a straight text format and store it this way too! ")
  )

```

With the (terpri stream) command commented out, Common Lisp doesn't see it and therefore won't run that command.  Common Lisp will still run just as before, with no output to
the screen:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/write1.jpg" height="600" width="950"></a>


However, when you go to read it back out, you'll notice that all entries run together and aren't seperated by line breaks at all:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/read1.jpg" height="600" width="950"></a>

You need to have line breaks inserted into the text if you want to view them with line breaks, so (terpri stream) can be used, but if you recall I said this wasn't the only 
way to get line breaks, right?  You can actually insert line breaks using "~%" as we did before with the (format t "this text~%") before.  If we use the same program we used 
before but add the "~%" after each word we want to have on it's own line, it would look like this:

```

#| Writes a file in Common Lisp |#

(with-open-file (stream "C:/users/micha/common_lisp/my_database2.db" :direction :output)
  (format stream "Eisner, Michael  alias King Farqhad~%")
;  (terpri stream)
  (format stream "Torvalds, Linus alias Linux Emperor~%")
;  (terpri stream)
  (format stream "You can even use a straight text format and store it this way too!~% ")
  )
  
  ```
  
  Notice that (terpri stream) is still commented out, *but* at the end of each printed statement, we have the "~%" character. This is going to force a line break between 
  each line we're asking the file to stream. Now, it still runs the same way, with nothing printed to the screen, just as before. When we ask for it to be printed back out
  it will do so and it will add the line breaks right back in:
  
  <a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/read2.jpg" height="600" width="950"></a>


