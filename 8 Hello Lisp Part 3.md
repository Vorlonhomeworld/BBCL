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
If I wanted to print the following:  

```

Name:  Galen


```

in Common Lisp, I could do it by entering this:

```
(format t "~a:~10t~a" :name "Galen")  

```

All we're really doing is chaining directives together, like we'd do in Linux if we used the "|" (pipe) command between two commands:

**format t** we've already seen, tells Common Lisp that we want to print something to the s**T**andard output, the screen, the **~a** directive is Common Lisps's 
**A**esthetic directive, this tells Common Lisp that we're going to supply it with something and it needs to be printed out as a human readable output.  The **:**
is our way of telling Common Lisp that we want to **modify** our print command to including, in this case, tabs. The **~10t** is literally our tab command, it tells
Common Lisp that we want 10 tabs, and yes, it does need to be in that format or it won't work. We follow it up with **~a**, another aesthetic command because we're
going to supply Common Lisp with something else to print. 

If you haven't picked up on this, the "~a" are also place holders for something we'll put in a bit later.  Speaking of, the **:name** is our first place holder. I realize
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
It's totally up to us - just follow the format, and you can pretty much make whatever changes you want!

### Review

**Y**eah, I know, who likes quizes, right? However, reptition is the best way to learn something, so, I'll give you a few things to practice on.
*Be awware* that unlike typical tests, the answers for these questions will *not* be in the back or in the next section, basically, if you miss something, you'll need
to read the **Hello Lisp** *(1,2 and 3)* to find the answer.   These won't be diffcult at all, I promise!


**1.  Print "Hello, World!" in the REPL.**

**2. Print pi so that it shows only 3 digits.**

**3. Can you do the same thing as you did in number 2 a different way? If so, feel free to do it within the REPL.**

*(Spoiler: You CAN do this two different ways in Common Lisp - if you get stuck, look at "Hello Lisp, Chapter 2")*
     
**4.Print the year 1984 in Hexadecimal.**

**5.Print the year 2099 in Octal.**

**6.Print the year 1999 in Binary.**

**7.Print 1963 in NEW Roman Numberals.**

**8.Now preint 1963, but in OLD Roman Numerals.**

*(If you get stuck, check "Hello Lisp, Part 3")*

**9. Print your name followed by your occupation, but seperate them with 4 tabs**

**10. Print your name followed by your occupation, but this time, put a carriage return between your name and your occupation**

*(If you get stuck, check "Hello Lisp, part 3")*
      
      
