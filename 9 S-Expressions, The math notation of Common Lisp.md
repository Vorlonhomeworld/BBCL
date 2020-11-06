
# So what *is* an S-Expression?

S-Expressions (or Symbolic Expressions) owe their existence to the work of Polish Mathematician, [Jan Łukasiewicz](https://en.wikipedia.org/wiki/Polish_notation) (in case you're wondering, his  name is pronounced as "Yahn Wuka-SHEV-itch"
!) who first considered this as a simpler way to do math, back in 1924. He would later write about it, in 1965, in a paper called
*Remarks on Nicod's Axiom and on "Generalizing Deduction"* on page 180. His idea was to place the operators *before* their
operands (rather than write 3+4, he suggested writing this equation as + 3 4). This type of math would 
become known as **PN** or **Polish Notation** , **NPN** or **Normal Polish Notation**, **Łukasiewicz notation**, **Warsaw
notation**, **Polish prefix notation** or just **Prefix Notation**. John McCarthy used this idea when constructing Lisp 
(this is before it became known as Common Lisp) for a few reasons.

First, when he was putting this language together, computers were the size of a room, and needed an air-conditioning unit also
the size of a room to keep it cool, but for all their size, they were extremely under-powered by todays standards.
Memory was something **less** than 32-bit and that memory was stored in vacuum tubes, not chips like we have today. Further,
personal computers hadn't been invented yet, so only big business and colleges actually had a computer and just to
use it, you had to wait in a line and you had to hand your program over to a technician who would run the program
for you! So, not only did the language John McCarthy was working on have to be powerful (for AI ), but it also
had to be careful how it used the memory and how much of it it actually used at any one time.

Believe it or not, back then, computers were programmed in Binary, Assembly and Fortran. Binary was the toughest, because
it was a two-based system (only two numbers, 0 and 1 ) in lines of 8 (00000001 is an example of a line in binary). 
Assembly was considered a huge improvement because you didn't have to use just numbers, you could actually use
words!  The problem with Assembly was, it did *exactly* what you told it to do and nothing more, so you had to
actually tell it to clear a register for you to make sure you could use it, or add another register to your 
register so you'd have the space to run your program.

## Wait!  What's a register?

I realize we're not learning assembly language, but in order to explain why John McCarthy used the Prefix notation,
we'll have to go into discussing registers briefly. Think of McCarthy's computer's proccesor as an office building with 
4 rooms (our processors have 8 or even 16!). Each room is pretty small, it can hold one worker and one client only. Each room does one or two specific operations,
although two of the rooms **CAN** work together at times. Instead of numbers, the rooms have a two letter code,
your rooms would be:

| Room Name | Job             |
|-----------|-----------------|
|**AX**     |    Accumulator  |
|**BX**     |    Base         |
|**CX**     |    Counter      |
|**DX**     |    Data         |


Now, image that these rooms are set up so that they're two doors apart, one door is labeled and one door
is not. Inside AX, there are actually two departments, one is the door that's labeled, and the other is 
the door right next to it that isn't labeled. The two departments call themselves **AH** (Accumulator High) and 
**AL** (Accumulator low). (By the way, The same thing is true for office BX, it has BH and BL inside, CX had CH and CL 
inside and DX has DH and DL inside.) They divide the work up between themselves. Bear in mind, they get their instruction in 
Binary (zeros and ones ) and Binary comes in packages of 8 (0000001 11000000), and the office starts counting at 
"0" not 1, from left to right. Any job that comes from bits 0 to 7 are handled by **AL** (Accumulator low),
any job that comes from bits 8 to 16 are handled by **AH** (Accumulator high). The same thing is true for 
office BX, it has BH and BL inside and divides the work the same way as does office CX and DX.

If AL is currently handling a job from bits 0 to 7, it's busy and can't handle another job from bits 0 to 7, that next job has to 
wait for AL complete the job and tell the current worker that the job's done and they can **pop** down for a cup of coffee
if they'd like,  if they try to take the second job at the same time (they wouldn't unless the other worker that was
still there didn't get the message to **pop** out of that office for a cup of coffee or a soda) it would overwhelm 
them and the whole office *could* possible shut down. I'm sure you're wondering why, if there's another department, in 
this case it would be AH, why couldn't *it* couldn't take the job instead? Think of this same office building, with the same departments, but now
add to that, that these departments *only* do certain jobs for *certain* parts of the number system, *and all of the departments* only
do one or two types of jobs *only* (yes I know, it sounds like the government!). That's *exactly* how the registers work! They do one or at most two types
of jobs and only for a part of the binary code, either the lower half (from 0 to 7) or the higher half (8-16).  

**AX** (which is composed of **AL** and **AH**) is the **primary accumulator** (that's *really* it's name). It handles input and output and alot of the 
arithmetic operations.

**BX** (which is composed of **BL** and **BH**) is the **base register**  This is used to refer to a file. It more or less creates a reference to a file without taking up 
space as the file would.

**CX** (which is composed of **CL** and **CH**) is the count register. This handles the counting with loops. Basically, if you tell a program to loop 5 times, it keeps
track of what loop the computer's actually on.

**DX** (which is composed of **DL** and **DH**) is the data register. This also handles input and output *and* can work with the **AX** register using *only*
multiplication or division of large numbers.

## Back to Lisp!!

**S**o how does any of this relate to Lisp you might wonder? Well, even though you don't have to make calls to registers in Common Lisp, it has to do it for you. 
McCarthy had to code that into the language so that it would do that for us.  What do I mean by that? Well, do you remember the "Hello World" program we wrote.
Let me show you the same program written in Assembly (this one compiles with NASM and will work with 32-bit Windows):

```

section .text                   ;section declaration

                                ;we must export the entry point to the ELF linker or
    global  _start              ;loader. They conventionally recognize _start as their
			                          ;entry point. Use ld -e foo to override the default.

_start:

                                ;write our string to stdout

    mov     edx,len             ;third argument: message length
    mov     ecx,msg             ;second argument: pointer to message to write
    mov     ebx,1               ;first argument: file handle (stdout)
    mov     eax,4               ;system call number (sys_write)
    int     0x80                ;call kernel

                                ;and exit

  	mov     ebx,0               ;first syscall argument: exit code
    mov     eax,1               ;system call number (sys_exit)
    int     0x80                ;call kernel

section .data                   ;section declaration

msg db      "Hello, world!",0xa ;our dear string
len equ     $ - msg             ;length of our dear string

```

If you recall - our Common Lisp program was one line only, that code that you're seeing above is **all** of the stuff that happens in the background that Common Lisp
does for you. (In case you're wondering, the lines with "ecx" are *still* the **CX** registers, but to accomodate a 32 bit computer a second version of each register
has to be build, so we still have the **CX** register which **still** handles bits 0-16, and we now have the **ECX** or the *Extended* **Count Register** which handles bits
17-32!  The same is true for EDX, EBX and EAX as well, the **E** simply means **extended** (and yes it's divided into two departments as well, just like the the original
registers!). The "*mov*" commands you're seeing are *move* commands, yep, like I said, in Assembly, you literally *had* to tell it to move memory and strings around!)

McCarthy had to be aware of the registers and what they were doing because, as I said earlier, he had a *very* small space to do it in.
In fact, in his paper ["The History of Lisp"](http://jmc.stanford.edu/articles/lisp/lisp.pdf) he describes the computer he was working on at the time, an [IBM 704](https://en.wikipedia.org/wiki/IBM_704) as having 
36 *bits* of memory - that's **4 bytes**. Because there was so little memory, John McCarthy had to find a way to hold the math operations in memory, have 
room left over for calculating the result and have as little moving around as possible so as to not tax the computer he was working with. He found it was easier to use 
the Prefix notation for this rather than regular notation.  Math won't be the only place you find this happening either, when we get to making decisions, you'll see that 
Common Lisp has the same format for that too. Instead of writing (if 5 > 6 do this), you'll write the operator (the ">" sign) first, (> 5 6).

**S**o, in short, John McCarthy used the Prefix notation to use less memory, use the registers more effeciently, tax the computer as little as possible, and still get it to 
calculate powerful equations for A.I, (which could get pretty complicated) *and* he had to do this on a seriously underpowered computer. Common Lisp still operates the same way today and because of this it runs quickly and powerfully on computers even today!

