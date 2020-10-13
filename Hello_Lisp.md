## Hello Lisp!

**B**ack in 1973, [Brian Kerhnigan](https://blog.hackerrank.com/the-history-of-hello-world/), the author of "C Programming Language" first referenced
the most popular program ever written. He wrote it first for C's predecessor, B, in the book "A Tutorial Introduction to the Programming Language B"
and it looked like this:

```
main( ) {
extrn a, b, c;
putchar(a); putchar(b); putchar(c); putchar(’!*n’);
} 1 ’hell’;
b ’o, w’;
c ’orld’;

```

It's without a doubt, the most famous program of all. Just about everyone starts their coding journey with this program. Since it's been a good
start for millions of coders, why not do likewise for Common Lisp. Unlike the example you see above, the Common Lisp version is *easy*, literally,
it's one line of code! So, in your REPL, type this in:

```
(format t "Hello, World!")

```

and hit enter, and you should see:

<a href="rel"><img src="/Repl3.jpg"></a>

Recall that Lisp is for **li**st **p**roccessing, well in Common Lisp, any list is defined as commands that exist between parentheses, so every
Common Lisp command has to have parentheses:

**(** format t "Hello, World!" **)**

The **format** command [formats](http://jtra.cz/stuff/lisp/sclr/format.html) your text for you, now it's not the *only* way to print text, but it'll be the one we 
use for now. 

The **t** tells the format command that we want to send our text to the terminal

Finally **"Hello, World!"** is the text we're sending to the terminal.

Go ahead, give it a try, but this time, change **"Hello, World"** to **"My name is** *(insert  your name here)* **"**!

## Debugger ##

We'll come back to printing and a bit more shortly, however, since all programmers are going to make mistakes sometime or another, I wanted to show
you Common Lisps *other* feature, the debugger. We're going to use the Hello World program again, but this time, we're going to introduce a deliberate
error, it won't run, it **will** give an error:

```

(format "Hello, World!")

```

The REPL's going to reply with this:

<a href="rel"><img src="/Repl_Error.jpg"></a>


Looks pretty cryptic, doesn't it?  It really isn't. The very first line lets us know that Common Lisp is sensing an **int**ernal error, now ignore the second
line and instead look at the third line, it's telling us that it's missing one argument **"Invalid number of arguments: 1"**.
The line right underneath that tells us that we're in the debugger amd what options we have (there are others that aren't visible right now, but available!).
At the bottom, it tells us what line our error's in. Granted, we have just one line, but if we had more, it would highlight only the line where the error is occuring.

Now, we can get the debuger to tell us a bit more plainly what's happening if we use the command **RETURN**.  We'd need to type RETURN then hit enter, we'd then be prompted 
enter the line with the error on it and hit enter, Common Lisp would then tell us plainly what the error is:

<a href="rel"><img src="/Repl_Debug.jpg"></a>

It's letting us know that we're missing something specifically for the **FORMAT** function, not only that, it's telling us in plain english as well!

This is only a taste of what the debugger can do. It can step through your code one line at a time, if need be, to help you find your error!

By the way, can you spot what we left out in our last code? There's a working example of this code a bit higher up, I promise you it's literally one missing item!


