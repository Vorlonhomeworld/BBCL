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

Go ahead, give it a try, but this time, change **"Hello, World"** to **"My name is** *(insert  your name here)* **"**)!

