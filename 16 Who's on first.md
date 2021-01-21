# Who's on first?

  So far, we've learned what a list is, that we can have lists inside of lists, we've learned to do arithmetic with Common Lisp and we've learned to how to get Common Lisp to
accept user input.  Now we're going to learn how to access data within lists.  Now why would we want to do that?  Simple!   A list can hold lots of data together, for example, a
list within Common Lisp can contain a username, a password, the user's access level or it could contain your first name, your last name, your address...etc... pretty much anything 
you want it to hold. If we want to have Common Lisp **only** access a certain portion of that data, we need a way to direct Common Lisp to *only* access that data within
the list! Common Lisp has two functions, CAR and CDR (you'll see that they're actually expandable into more than two in a momemt!) to do just that!
  
## CAR, CDR....what a minute, what?!

  CAR and CDR were designed by John McCarthy to allow easy access into parts of a list so that the components of that list could be easily addressed. They were developed 
back in 1950 when John McCarthy was designing Lisp (this is before it was called Common Lisp) on an IBM 704. This computer had registers that were named "A", "B", "C", and
although the registers could be accessed directly, it was a bit cumbersome to do so as one address was constant and the other one was not. 
