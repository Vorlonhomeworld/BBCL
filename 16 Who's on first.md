# Who's on first?

  So far, we've learned what a list is, that we can have lists inside of lists, we've learned to do arithmetic with Common Lisp and we've learned to how to get Common Lisp to
accept user input.  Now we're going to learn how to access data within lists.  Now why would we want to do that? Simple! A list can hold lots of data together, for example, a
list within Common Lisp can contain a username, a password, the user's access level or it could contain your first name, your last name, your address...etc... pretty much anything 
you want it to hold. If we want to have Common Lisp **only** access a certain portion of that data, we need a way to direct Common Lisp to *only* access that data within
the list! Common Lisp has two functions, CAR and CDR (you'll see that they're actually expandable into more than two in a moment!) to do just that!
  
## CAR, CDR....wait a minute, what?!

  CAR and CDR go back to the beginning of Lisp. When John McCarthy was designing Lisp itself, he had an [IBM 704](https://www.computerhistory.org/chess/stl-431614f6482e6/) 
to work with. Now that particular computer didn't have address registers , but instead, had index registers.  Remember, we talked a bit about computer registers
in [chapter 9](https://github.com/Vorlonhomeworld/BBCL/blob/main/9%20S-Expressions%2C%20The%20math%20notation%20of%20Common%20Lisp.md) of this book?  
 
  The index registers were called A, B and C (in the I.B.M manual), they were also called IR1, IR2 and IR4 in the M.I.T manual. Yes, you're reading this correctly, it does jump
from IR2 to IR4, however, the machine was set up so that registers could be called together (for example IR1 and IR2 could be called at the same time). They were accessed by 
LX instructions, LXA **l**oaded the inde**x** from the **a**ddress field and LXD **l**oaded the inde**x** from the **d**ecrement field. While the 704 had no CAR or CDR 
functions available, the LXA and LXD instructions served as models for CAR and CDR. 
 
  The instructions worked, but they were limited in what they could do. Yes, it was possible to access a specific register directly, in fact, in the IBM manual they had 
a shorthand instruction for this, the capital letter **"C"**, which stood for *Contents Of*.  With this instruction it was possible to access the contents of register
100, for example, by writing C(100), however, no instruction to read the address field or decrement field existed at all! John McCarthy took that existing terminiology and applied it to a list
rather than a register for pretty much the same effect. You have the ability to access a field directly by requesting it through the CAR or CDR command.

  Possibly the best explanation I've come across of this is actually breaks down each of the letters of CADR. This is in a paper from Thomas W. Lynch at the Birbeck College, University
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
  
  Common lisp would **r**un the list first, then it would **a**ccess the head cell, in this case, it's "0", then it would complete. This would give us a 
result of "0".  (In the simple version of the program we'll be running with CAR, the "C" portion completes and prints what it accessed automatically!)

  Now, if we were to use **cdr** instead, for example:
  
  ```
  
  (CDR '(0 1 2 3))
  
  ```

Common lisp would still **r**un, but instead of accessing the head cell, it would **d**rop the head cell, which in this case is still "0", then it would complete. This would 
give us a result of (1 2 3)  because we removed the "0" from this list.

Just in case you're scratching your head about this (and I know I was when I was learning this!), when you access any list in Common Lisp, Common Lisp only sees what 
you've accessed and nothing else.  Think of a list like a lunch bag:

```

(defun lunchbag ()
  '(sandwich cookie chips soda))
  
 ```
 
 Imagine the command **(defun '(lunchbag))** as you packing the lunchbag. Were you to look in the lunch bag after you packed it, you would see that the lunchbag has
 **'(sandwich cookie chips soda)** inside.  Now, you getting the sandwich out of the bag would be analagous to **(car '(lunchbag))**. Sandwich is the first thing in the bag 
 (or the head of the list), you'd **r**each  into the bag, **a**ccess the sandwich and **c**omplete the act of getting the sandwich. Your lunchbag is still there, but the 
 only thing that's in your hand is the  sandwich. Same thing is true with Common Lisp.
 
 The **CDR** command would be analagous to the same lunch bag, *however*, in this case, we want to access the cookie first (maybe we need a sugar fix or something!). So we 
 go to the bag which is still **'(sandwich cookie chips soda)** and we remove the sandwich out of the bag. The list (contents of the bag) now changes to **'(cookie chips soda)**
 Since the sandwich is no longer part of that list, we pick up the next thing on that list, which is the cookie.
 
 Essentially, anything **a**ccessed by Common Lisp is pulled and seperated out of the list and is presented singularly, so the **CAR** command is great for looking at single 
 items. Any time something is **d**ropped by the list with the **CDR** command, the rest of the list is intact, except for the item dropped. This can be used for single or 
 multiple items. Those multiple items in the list can be accessed by expanding the CDR command!

  Remember I said this was expandable? It is! All we need to do is repeat a few commands. Let's say I have the following list:
  
  ```
  
  (0 (1 2 3) 4 5)
  
  ```
  
  and what I need to access is "2". If I try the command **CAR** on the list, I'll get **0**, as I would be **r**unning the program, **a**ccessing the head cell, then 
  **c**ompleting. In order to get the value of 2, wouldn't I have to drop that 0 off.  I *could* use **d** to do that, however, it couldn't be the command **cdr** because
  **cdr** would drop *only* the head of this cell off "0". I would wind up with:
  
```

((1 2 3) 4 5)  
  
```

So, that doesn't work, I'm likely going to need to **a**ccess this list at least one more time. I could try this:
  
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

That **now** gives us the answer we were looking for, which is "2". We're **r**unning the program, then we're **d**ropping the head of the cell, which is still "0". We
then **a**ccess the head of the cell, which is now "(1 2 3)", we now again **d**rop the head of the new cell, which is "1". We then **a**ccess the new head of the cell we 
just now created, which is "2". Now we **c**omplete and get our number "2".

I'll show this to you another way to give you a better idea of how this is doing this. This too is from 
Thomas W. Lynch's paper:

|  Step |Command  |     Entered in  |     Result              |           Comments                                                                |
|-------|---------|-----------------|-------------------------|-----------------------------------------------------------------------------------|           
|   0   |cadad**r**| (0 (1 2 3) 4 5)| **run** (0 (1 2 3) 4 5) | runs the command on '(0 (1 2 3) 4 5). Nothing to remove yet                       |
|   1   |cada**d**| (0 (1 2 3) 4 5) | **drop** the "0"        | drops the head of the cell, which is "0" resulting in ((123(45))                  |
|   2   |cad**a** | ( (1 2 3) 4 5 ) | **access** (1 2 3)      | accesses the **new** head of the cell, which is (123) and reads that only         |
|   3   |ca**d**  |   (1 2 3)       | **drop**   (1)          | drops the head of the **new** cell, which leaves us with (2 3)                    | 
|   4   |c**a**   |      (2 3)      | **access**   (2)        | accesses the head of the new cell which is now ( 2) and reads only that new cell  |
|   5   |**c**    |        (2)      | **complete**            | completes the command, leaving only "2" as the reamining item in that cell        |


  As an example of what this can do for you (outside of sorting numbers), consider this example:
  
  You have a list that shows the name, place of birth and current place of employment of an employee, and what you need is to be able to access different parts of his record.
  You can do that with the commands I just showed you.  First, let's start with the employees record, admittedly, this is a simple way to do this:
  
  ```
  (defun records ()
  '(sheridan john earth babylon5))
  
  ```
  
  We can pull his first name out of the list asking for it from this list - remember, we've **de**fined our **fun**ction by calling it "records", so we can access it again
  by calling it by name:
  
  ```
 (cadr (records))
 
 ```
 
 This gives us:
 
 ```
  "John"
 
 ```
  
  *(Remember, it's **r**unning the list (sheridan john earth babylon5), **d**ropping the head of the list off, which is "Sheridan", then **a**ccessing the head of the
  list, which is now "John", then **c**ompleting the job, which causes it to print the last thing it's accessed, "John")*
  
  We can access just his last name by entering in:
  
 ```
 
 (car (records))
 
 ```
 
 which could give us:
 
 ```
 
 "Sheridan "
 
 ```
  
  
 Now, if we wanted to access his place of birth we'd need to enter a different command. Given the table up top, you might think you'd need to enter:
 
 ```
 (cadadr (records))
 
 ```
 
 However, that would give us an error.  If you look at the table above, you'll notice that each time we **a**ccess the list, it focuses on a small section of that list and 
 drops everything else, so were we to use this command we would have dropped everything else off the list and there'd be nothing left to print, which would give us an error.
 That means we need to limit our **a**ccess to one command but **d**rop more of the record off.  What we could do in this example is enter:
 
 ```
 (caddr (records))
 
 ```
 
 This will give us "Earth", which is what we want as the command will **r**un on the list (sheridan john earth babylon5), then it will drop the head of the list off, which is 
 "sheridan", the list then becomes "(john earth babylon5)". We then get the program to **d**rop the new head of the list off, which is "john". The list then becomes "(earth
 babylon5)". At this point we can access the new head of the list which is "earth", and that's what the program will print.
 
 Now, how do you think we'd get this list to print his current place of employment, Babylon5?   Just look up, you'll need to add one more character to the code we just looked 
 at!
 
 One more thing, if you remember, I said that (CDR) can be used to print multiple parts of a list?  Looking at the same list, what if we wanted to print where this employee was
 from and where is was working?  From what we already know, we'd have to **not** use an access command, but because the list has his last name, first name, place of birth
 and place of employment, we'd need to at least remove the first two fields, right?   What we'd need to enter in is:
 
 ```
 
 (cddr (records))
 
 ```
 
 That would give us:
 
 ```

Earth Babylon5

```
