
# So what *is* an S-Expression?

S-Expressions (or Symbolic Expressions) owe their existence to the work of Polish Mathematician, [Jan Łukasiewicz](https://en.wikipedia.org/wiki/Polish_notation) who
first considered this as a simpler way to do math, back in 1924. He would later write about it in a paper called *Remarks on Nicod's Axiom and on "Generalizing Deduction"* 
on page 180. His idea was to place the operators *before* their operands (rather than write 3+4, he suggested writing this equation as + 3 4). This type of math would 
become known as **PN" or Polish Notation** , **NPN or Normal Polish Notation"**, **Łukasiewicz notation, Warsaw notation, 
Polish prefix notation** or just **Prefix Notation**. John McCarthy used this idea when constructing Lisp (this is before
it became known as Common Lisp.) for a few reasons.

First, when he was putting this language together, computers were the size of a room, and needed an air-conditioning unit 
the size of a room to keep it cool, but for all their size, they were extremely under-powered by todays standards.
Memory was something like 32-bit and that memory was stored in vacuum tubes, not chips like we have today. Further,
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

## Wait?  What's a register

I realize we're not learning assembly language, but in order to explain why John McCarthy used the Prefix notation,
we'll have to go into discussing registers briefly. Think of your computer's proccesor as an office building with 
4 rooms. Each room is pretty small, it can hold one worker and one client only. Each room does one specific operation,
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
office BX, it has BH and BL inside, CX had CH and CL inside and DX has DH and DL inside.

If AL is currently handling a job from bits 0 to 7, it's busy and can't handle another job, the next job has to 
either wait for AL complete the job and tell the worker that the job's done and they can **pop** down for a cup of coffee
if they'd like, or if they try to take the second job at the same time (they wouldn't unless the other worker that was
still there didn't get the message to **pop** out of that office for a cup of coffee or a soda) it would overwhelm 
them and the whole office *could* possible shut down.
