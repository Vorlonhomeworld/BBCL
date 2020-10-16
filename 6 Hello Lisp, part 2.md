## Hello Lisp, Part II

In the last chapter, we wrote our first program. As I believe repetition is essential to learning, we're going to continue with format t statements, only
we're going to add a bit more to them. The format statement actually has [a lot of prefix parameters avaiable](https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/#subsections-summary-table) for your usage. Don't worry, we won't go through all of them in this chapter, just a few!  

As I mentioned in the last chapter, (format t "Hello World") is how we get Common Lisp to print out "Hello World". Now, what if we wanted to print out a number, let say, 
*pi*. Common Lisp knows what pi is we we could literally enter the following statement

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
REPL are going to throw up their hands and give up, which is just what happened here!  That's where directives come in handy. We can print this out more sanely, and
a bit more humanely for the REPL, the computer and our eyes it we tell it how many decimals places we want it to go. It will ignore anything after those places
and give us an easier to read number!

What we'll do it let Common Lisp know that we want to add a few optional directive in. We have to start that process by inserting a tilde "~" first. That
lets Common Lisp know that we're adding optional directive into the print statment.  We'll tell pi to print out it's value to 2 digits only by entering in:

```

(format t "~v$" 2 pi)

```

Common Lisp is happier with that and responds:

```
* (format t "~v$" 2 pi)
3.14
NIL

```
