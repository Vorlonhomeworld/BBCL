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

will give us 0.75. 

Yes, we literally just used a list inside of a list, this is called a **nested loop**. It's pretty common to see these in Common Lisp. When you're working with nested lopos, 
just remember that all the loops must be closed or it won't work, just like the example I showed on chapter 2 when I printed part of my LibreOffice Calc formula for my budget:

```
=IF(A38=1,0.2004,IF(A38=2,0.21,IF(A38=3,0.227,IF(A38=4,0.24,IF(A38=5,0.032,IF(A38=6,0.067,IF(A38=7,0.118,IF(A38=8,0.064,IF(A38=9,0.152,IF(A38=10,0.19,IF(A38=11,0.212,IF(A38=12,0.225,IF(A38=13,0.247,IF(A38=14,0.3,IF(A38=15,0.033,IF(A38=16,0.123,IF(A38=17,0.158,IF(A38=18,0.182,IF(A38=19,0.199,IF(A38=20,24.6,IF(A38=21,0.275,IF(A38=22,0.023,IF(A38=23,0.096,IF(A38=24,0.105,IF(A38=25,0.138,IF(A38=26,0.161,IF(A38=27,0.148,IF(A38=28,0.172,IF(A38=29,0.188,IF(A38=30,0.235,IF(A38=31,2.63)))))))))))))))))))))))))))))))

```

That's a nested loop, and every parantheses I opened up at the start had to be closed at the end. In fact if you scroll to the end of that formula you'll see that it has 30 
closed parantheses at the end. If I had any of the loops not closed, that forumla wouldn't work, it would throw an error and I'd have to close that parentheses in order for it
to work. Common Lisp is exactly the same way, and in fact it's possible to close lists inside of another list, and we'll get into that a bit later, I promise!`
