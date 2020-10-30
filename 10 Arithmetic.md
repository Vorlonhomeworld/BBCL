# Arithmetic 

**W**ith that discussion of S-expressions over, now it's time to get back to coding, most notably, with the prefix notification we discussed just a chapter back!
Common Lisp has some of the easiest math coding anywhere, although it takes a bit of getting used to. For example, if you want to have the REPL add 3 plus 4, you
wouldn't write:

```
(3 + 4 =)

```

You would use the **prefix notation** and write:

```

(+ 3 4)

```
Notice, you don't have to write the equals sign, nor do you have to tell Common Lisp to print it, it does this for you!  Multiplication is exactly the same way, except
for the operand

```

(* 3 4)

```

Now, division *might* throw you because Common Lisp dosen't default to floating numbers for it's division!  What do I mean by that?  Well, go ahead 
and type in:

``` 

(/ 3 4)

```

You'll see that it gives you **3 / 4** rather than a decimal answer that you were likely looking for!  This is because Common Lisp, by default will give a ratio if the answer 
is not an exact integer. Now, if we want a decimal or floating point answer (same thing, different name!), we can specifically request this and Common Lisp will do it
for us:

```

(float(/ 3 4))

```

will give us 0.75 
