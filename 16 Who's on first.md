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
