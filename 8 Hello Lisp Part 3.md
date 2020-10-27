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
The **%** is Common Lisp's directive for a carriage return. Just remember the **~** (it's caled a "tilde" just in case you didn't know that! )is there to let Common Lisp
know we want to give it a directive! Go ahead, give it a try!


Tabs

```

tabs = (format t "~a:~10t~a" :name "David Trombly")  
yeilds  Name:     David Trombly-


````
