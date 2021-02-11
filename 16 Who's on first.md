# Who's on first?

  So far, we've learned what a list is, that we can have lists inside of lists, we've learned to do arithmetic with Common Lisp and we've learned to how to get Common Lisp to
accept user input.  Now we're going to learn how to access data within lists.  Now why would we want to do that? Simple! A list can hold lots of data together, for example, a
list within Common Lisp can contain a username, a password, the user's access level or it could contain your first name, your last name, your address...etc... pretty much anything 
you want it to hold. If we want to have Common Lisp **only** access a certain portion of that data, we need a way to direct Common Lisp to *only* access that data within
the list! Common Lisp has two functions, CAR and CDR (you'll see that they're actually expandable into more than two in a momemt!) to do just that!
  
## CAR, CDR....what a minute, what?!

  CAR and CDR go back to the beginning of Lisp. When John McCarthy was designing Lisp itself, he had an [IBM 704](https://www.computerhistory.org/chess/stl-431614f6482e6/) 
to work with at the time. Now that particular computer didn't have address registers , but instead, had index registers.  Remember, we talked a bit about computer registers
in [chapter 9](https://github.com/Vorlonhomeworld/BBCL/blob/main/9%20S-Expressions%2C%20The%20math%20notation%20of%20Common%20Lisp.md) of this book?  
 
  The index registers were called A, B and C (in the I.B.M manual), they were also called IR1, IR2 and IR4 in the M.I.T manual. Yes, you're reading this correctly, it does jump
from IR2 to IR4, however, the machine was set up so that registers could be called together (for example IR1 and IR2 could be called at the same time). They were accessed by 
LX instructions, LXA **l**oaded the inde**x** from the **a**ddress field and LXD **l**oaded the inde**x** from the **d**ecrement field. While the 704 had no CAR or CDR 
functions available, the LXA and LXD instructions served as models for CAR and CDR. 
 
  The instructions worked, but they were limited in what they could do. Yes, it was possible to access a specific register directly, in fact, in the IBM manual they had 
a shorthand instruction for this, the capital letter **"C"**, which stood for *Contents Of*.  With this instruction it was possible to access the contents of register
100, for example, by writing C(100), however, no instruction to read the address field or decrement field existed at all! John McCarthy took that existing terminiology and applied it to a list
rather than a register for pretty much the same effect. You have the ability to access a field directly by requesting it through the CAR or CDR command.

  Possibly the best explanation I've come across of this is an explanation that breaks down each of the letters of CADR from Thomas W. Lynch at the Birbeck College, University
  of London, in fact, his paper [is online](https://arxiv.org/ftp/arxiv/papers/1507/1507.05956.pdf) and is a great, and in depth discussion of CAR and CDR if you want to read it!
  In his explanation, he breaks down CAR and CDR into their individual letters and explains what each letter does:
  
  | Letter     |  Command         |   Explanation        |                            
  |------------|------------------|----------------------|
  | a          |   access         | Access the head cell |
  | c          |   complete       | Finish processing    |
  | d          |   drop           | Drop the head cell   |
  | r          |   run            | Run the program      |
  
  The first thing you'll need to know is that Common Lisp reads this request from right to left (remember prefix notation?), so  if we were to enter:
  
  ```
  
  (CAR '(0 1 2 3))
  
  ```
  
  Common lisp would read the **r** first and run the routine, then it would **a**ccess the head cell, in this case, it's "0", then it would complete. This would give us a 
result of "0". 

  Now, if we were to use **cdr** instead, for example:
  
  ```
  
  (CDR '(0 1 2 3))
  
  ```

Common lisp would still **r** run, but instead of access the head cell, it would **d**rop the head cell, which in this case is still "0", then it would complete. This would 
give us a result of (1 2 3)  because we removed the "0" from this list.

  Remember I said this was expandable? It is! All we need to do is repeat a few commands. Let's say I have the following list:
  
  ```
  
  (0 (1 2 3) 4 5)
  
  ```
  
  and what I need to get is the value of "2". If I try **CAR** the list, I'll get **0**, as I would be **r**unning the program, **a**ccessing the head cell, then 
  **c**ompleting. In order to get the value of 2, wouldn't I have to drop that 0 off.  I can try that, since **d** would allow me to **d**rop the head cell off, which is 
  "0". I would enter that request like this:
  
  ```
  
  (cadr '(0 (1 2 3) 4 5))
  
  ```
  Here, I'm **r**unning the program, **d**ropping the head cell off , which is "0", **a**ccessing the head cell, which is now (1 2 3), remember, "0" is dropped off so it's 
  not there any more. Once I do that, I tell the program to **c**omplete. It will do so, but it won't give me "2", instead it'll give me (1 2 3). This isn't what we wanted, 
  we wanted to get   only "2". So, back to the drawing board......or maybe not!
  
  Remember that this command has four letters in it, and we've used all four letters, so how to we get this to go further into our list? Simple, we repeat two of our  
prior steps. We need to repeat the access and drop steps one more time to get what we need, so we'd write this out as:

```

(cadadr '(0 (1 2 3) 4 5))

```

That **now** gives us the answer we were looking for, which is "2".  I'll show this to you another way to give you a better idea of how this is doing this. This too is from 
Thomas W. Lynch's paper:

|  Step | Command   |     Entered in   |     Result              |           Comments                                                                |
|-------|-----------|------------------|-------------------------|-----------------------------------------------------------------------------------|           
|   0   | cadad**r**| (0 (1 2 3) 4 5)  | **run** (0 (1 2 3) 4 5) | runs the command on '(0 (1 2 3) 4 5). Nothing to remove yet                       |
|   1   | cada**d** | (0 (1 2 3) 4 5)  | **drop** ( (1 2 3) 4 5) | drops the head of the cell, which is "0" resulting in ((123(45))                  |
|   2   | cad**a**  | ( (1 2 3) 4 5 )  | **access** (1 2 3)      | accesses the **new** head of the cell, which is (123) and reads that only         |
|   3   | ca**d**   |   (1 2 3)        | **drop**   (2 3)        | drops the head of the **new** cell, which leaves us with (2 3)                    | 
|   4   | c**a**    |      (2 3)       | **access**   (2)        | accesses the head of the new cell which is now ( 2) and reads only that new cell  |
|   5   | **c**     |        (2)       | **complete**            | completes the command, leaving only "2" as the reamining item in that cell        |



