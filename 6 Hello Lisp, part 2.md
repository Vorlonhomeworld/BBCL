## Hello Lisp, Part II

In the last chapter, we wrote our first program. As I believe repetition is essential to learning, we're going to continue with format t statements, only
we're going to add a bit more to them. The format statement actually has [a lot of prefix parameters avaiable](https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/#subsections-summary-table) for your usage. Don't worry, we won't go through all of them in this chapter, just a few!  

As I mentioned in the last chapter, (format t "Hello World") is how we get Common Lisp to print out "Hello World". Now, what if we wanted to print out a number, let say, 
*pi*?  Common Lisp knows what pi is, we we could literally enter the following statement

```
(format t pi)

```
The  repl would print out pi, but it would also throw an error:

```
* (format t pi)

debugger invoked on a SB-KERNEL:CASE-FAILURE in thread
#<THREAD "main thread" RUNNING {10010B0523}>:
  3.141592653589793d0 fell through ETYPECASE expression.
  Wanted one of (SIMPLE-STRING STRING SB-FORMAT::FMT-CONTROL).

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(SB-FORMAT::%FORMAT #<SYNONYM-STREAM :SYMBOL SB-SYS:*STDOUT* {100014E8A3}> 3.141592653589793d0 NIL NIL)
0]

```

Notice that it's printing pi out as fully as it can, however, that's a problem, because pi can be computed out indefintely, so at some point, the computer **and** the 
REPL are going to throw up their hands and give up, which is just what happened here!  That's where directives come in handy. Directives are Common Lisp's way of further
clariying what we want it to do, like say, print pi with a certain amount of decimal places only! We can print this out more sanely, and a bit more humanely for the REPL, 
the computer and our eyes it we tell it how many decimals places we want it to go. It will ignore anything after those places and give us an easier to read number!

What we'll do it let Common Lisp know that we want to add a few optional directive in. We have to start that process by inserting a tilde "~" first. That
lets Common Lisp know that we're adding optional directive into the print statment.  We'll tell pi to print out its value to 2 digits only by entering in:

```

(format t "~v$" 2 pi)

```

Common Lisp is happier with that and responds:

```
* (format t "~v$" 2 pi)
3.14
NIL

```

The **~** lets the format command know we're adding directives to the command.
The **v** lets Common Lisp know that we're going to be adding an argument to whatever it is we want it to print, so it needs to parse that too
The **$** lets Common Lisp know that we want the format to be decimal. This is important because Common Lisp can print, natively in Hex and Octal too!

Yes, we **can** enter the previous command to print pi as (format t "~$" pi) and it will still print out 3.14 , however it we leave out the "v" and try

``` 

(format t "~$" 3 pi)

```

We're going to get 3.00 because we didn't let Common Lisp know that we're going to add an extra argument to pi. In order for this to print
correctly, we'll need to enter the command as:

```

(format t "~v$" 3 pi)

```

Which will give us what we *really* wanted in the first place, 3.142!

Your turn!  Go ahead and tell Common Lisp that you want to print pi to , say 5 digits and then after that, 7 digits!

### Another way of doing the same thing ###

Common Lisp, if nothing else, has different ways of doing the same thing, for example, I just showed you that we can print pi to 3 digits with (format t "~v$" 3 pi),
that's not the only way to do that! We'll still use a directive, but this time we'll write it like this:

```

(format t "~,5f" pi)

```

which will give us:

```
* (format t "~,5f" pi)
3.14159
NIL

```