# Who's on first?

  So far, we've learned what a list is, that we can have lists inside of lists, we've learned to do arithmetic with Common Lisp and we've learned to how to get Common Lisp to
accept user input.  Now we're going to learn how to access data within lists.  Now why would we want to do that?  Simple!   A list can hold lots of data together, for example, a
list within Common Lisp can contain a username, a password, the user's access level or it could contain your first name, your last name, your address...etc... pretty much anything 
you want it to hold. If we want to have Common Lisp **only** access a certain portion of that data, we need a way to direct Common Lisp to *only* access that data within
the list! Common Lisp has two functions, CAR and CDR (you'll see that they're actually expandable into more than two in a momemt!) to do just that!
  
## CAR, CDR....what a minute, what?!

  CAR and CDR go back to the beginning of Lisp, when John McCarthy was designing Lisp itself.  He had an [IBM 704](https://www.computerhistory.org/chess/stl-431614f6482e6/) 
  to work with at the time. Now that particular computer didn't have address registers , but instead, had index registers.  Remember, we talked a bit about computer registers
  in [chapter 9](https://github.com/Vorlonhomeworld/BBCL/blob/main/9%20S-Expressions%2C%20The%20math%20notation%20of%20Common%20Lisp.md) of this book?  
 
  The index registers were called A, B and C
