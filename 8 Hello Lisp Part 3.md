# Hello Lisp - Part III


**I**n addition to the ways you can print decimals, octal, hex, binary, and other type of numbers, you can *also* use the format command to 
convert our numbers into Roman numbers, print out carriage returns, even print out tabs. For example, let's say you want to convert our current year,
2020 into Roman numbers. You can do this quickly in Common Lisp:

```

(format t "~@r" 2020)

```

The **r** portion of the directive lets Common Lisp know we're asking it to print a Radix ( a set of numbers based on a number system - for example, our typical number
system is a 10 based number system, therefore, it's Radix is 10, an Octal radix would be 8, a binary radix would be 2) but we haven't yet told it what kind, that's what the 
preceeding symbol (**@**) is for, it tells Common Lisp that we want a Roman number Radix (Roman Number - the newer version). Common Lisp can 
also print older version of Roman Numbers by adding in the **:** into the command (format t "~:@r" 2020)!

Go ahead, give them both a try, you should get Roman numbers instead of "2020"! Be careful with this one because if you forget the ampersand **"@"**, instead
of getting Roman Numerals, Common Lisp will show you another trick, it will convert "2020" into "two thousand twenty". It's not a bug, it's yet another feature of the
**format** command!

The format command can also be used to print a carriage return. Let's say you want to print out Radius and Area, but have them be one on top of the other.
It's totally possible to do *and* have it all one line!

```

(format t "Radius:~%Area:")

```
The **%** is Common Lisp's directive for a carriage return. Just remember the **~** (it's called a "tilde" just in case you didn't know that! ) is there to let Common Lisp
know we want to give it a directive! Go ahead, give it a try!


It's also possible to print tabs within Common Lisp, the **"~t"** allows us to do this. This example might look a bit odd, but it's not really, I promise!
If I wanted to print the follow:  

```

Name:  Galen


```

in Common Lisp, I could do it by entering this:

```
(format t "~a:~10t~a" :name "Galen")  

```

All we're really doing is chaining directive together, like we'd do in Linux if we used the "|" (pipe) command between two commands:

**format t** we've already seen, tells Common Lisp that we want to print something to the s**T**andard output, the screen, the **~a** directive is Common Lisps's 
**A**esthetic directive, this tells Common Lisp that we're going to supply it with something and it needs to be printed out as a human readable output.  The **:**
is our way of telling Common Lisp that we want to **modify** our print command to including, in this case, tabs. The **~10t** is literally our tab command, it tells
Common Lisp that we want 10 tabs, and yes, it does need to be in that format or it won't work. We follow it up with **~a**, another aesthetic command because we're
going to supply Common Lisp with something else to print. 

If you haven't picked up on this, the "~a" are also place holders for something we'll put it a bit later.  Speaking of, the **:name** is our first place holder. I realize
we said we wanted to print "Name:", Common Lisp needs to have the colon placed before the word we're working with, you'll see the same thing when we talk about numbers, 
it's a variation of Lambda formatting, where the operation comes before the number or word, for example in Common Lisp, if we want it to add 4+4, we wouldn't
write it that way, we'd place the addition sign first, followed by the two numbers to add:

```

(+ 4 4)

```

The same thing's true for adding a colon before the word "Name".  Now, you don't have to have the colon, if you don't want it, OR you can add it on your own if you'd 
prefer, you'd need to make only one slight change to the code we entered above:


```

(format t "~a~10t~a" "Name:" "Galen")  

```

Notice that we removed the ":" after the first "~a" ?  That allows us to add in the colon on our own, or use a different symbol other than a colon if we wanted to, 
say for instance, we wanted a table to print out instead, like so:

```

(format t "~a~10t~a" "Name |" "Galen")

```

