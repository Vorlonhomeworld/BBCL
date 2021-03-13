# Sort of Nice *(a look at sorting in Common Lisp)*

  Since we looked at **CAD** and **CDR**, the next step on our journey would be to look at sorting (because sorting uses CAD and CDR and variations of this
  command when it runs! ). Common Lisp actually has not one, but *two* [sort](http://clhs.lisp.se/Body/f_sort_.htm) commands, **sort** and **stable-sort**. They
  both use the same syntax in the same order, however **stable-sort** is actually a more stable version of the two. There's a great example of this given on the clhs.lisp.se
  page I linked to for this command:
  
  ```
  
  (sort (setq committee-data
             (vector (list (list "JonL" "White") "Iteration")
                     (list (list "Dick" "Waters") "Iteration")
                     (list (list "Dick" "Gabriel") "Objects")
                     (list (list "Kent" "Pitman") "Conditions")
                     (list (list "Gregor" "Kiczales") "Objects")
                     (list (list "David" "Moon") "Objects")
                     (list (list "Kathy" "Chapman") "Editorial")
                     (list (list "Larry" "Masinter") "Cleanup")
                     (list (list "Sandra" "Loosemore") "Compiler")))
       #'string-lessp :key #'cadar)
       
```
       
This starts right off with Common Lisp stating that we're going to **sort** the data that folows. The next step is to set up the data, and we'll do that with the **setq**
command. The **setq** command is pretty much like an equals command , it's telling us that "committee-data" is going to equal the data that follows it. The next line starts 
off with the function **vector**. This function **must** be there to distinguish a *vector* from a *list*, otherwise the function isn't going to work, because Common Lisp
expects a list to have different things in it than a vector will.

## List, vector, aren't they the same ?

  They really aren't, even though they can look like they are. For exaxmple:
  
  ``` 
  (defun my_function() )
  
  ```
  
  can be said to be an empty list, it can also be considered a valid vector too, but just because they look the same in this example doesn't mean they *are* the same. They're 
not, and for our sort command to work, we have to tell it which items we're working with. 
    A vector [is defined as](https://www.computerhope.com/jargon/v/vector.htm) *"A pointer or an array with one dimension"*.  A pointer, as the name implies, *points* to 
another variable, either directly or indirectly. For example, in [chapter 11](https://github.com/Vorlonhomeworld/BBCL/blob/main/11%20Defun.md), I created a really simple 
program called "Euler's Calculator":

```

(defun uc (a)
(exp a))


```

in this example (defun uc) is shown to equal "a". When I load this program and then enter:

```

(uc 4) 

```

I'm using the number "4" to refer to the variable (or *pointer* "a") that I setup for UC.

  An array [is defined as](https://www.computerhope.com/jargon/a/array.htm) *"A group of **related** data values grouped together. These values **must** be the same type 
of data*". Strings with strings, numbers with numbers, that kind of thing.  Our example up at the top is an example of an array as all of the values are strings. 

```

 (vector (list (list "JonL" "White") "Iteration")
                     (list (list "Dick" "Waters") "Iteration")
                     (list (list "Dick" "Gabriel") "Objects")
                     (list (list "Kent" "Pitman") "Conditions")
                     (list (list "Gregor" "Kiczales") "Objects")
                     (list (list "David" "Moon") "Objects")
                     (list (list "Kathy" "Chapman") "Editorial")
                     (list (list "Larry" "Masinter") "Cleanup")
                     (list (list "Sandra" "Loosemore") "Compiler")))
                     
```

Each array is a string, surrounded by quotes.

  A *list* is a basic structure of Common Lisp that can contain either a *cons* (a memory **cons**truct that either holds values or pointers to values) or a *null* (and empty 
parantheses).  Because our  vector contains a memory construct, we have to let  Common Lisp know that we're creating a vector out of lists, in this case, two lists are being 
used to create our vector. Once we finish telling Common Lisp what our list is, we have to tell it how to sort it:

```

  (sort (setq committee-data
             (vector (list (list "JonL" "White") "Iteration")
                     (list (list "Dick" "Waters") "Iteration")
                     (list (list "Dick" "Gabriel") "Objects")
                     (list (list "Kent" "Pitman") "Conditions")
                     (list (list "Gregor" "Kiczales") "Objects")
                     (list (list "David" "Moon") "Objects")
                     (list (list "Kathy" "Chapman") "Editorial")
                     (list (list "Larry" "Masinter") "Cleanup")
                     (list (list "Sandra" "Loosemore") "Compiler")))
       #'string-lessp :key #'cadar)
       
```

  The last line (**#'string-lessp :key #'cadar**) tells Common Lisp how were sorting this list out.  First, we tell it to sort alphabetically, that's what the string-lessp is
for. String-lessp is the equivalent of string< and string>. In this case, (lessp), it's looking to see if string 1 is less than string 2, if it is, than it's true, otherwise
it's false. The "#'" by the way is used to pass a function , by name, as an argument to another function.

  Once we tell it to sort alphabetically, we have to tell it **what** we want to sort by, first name, last name, job title. We establish that by using :key #'cadar.
Cadar, as you remember, reads left to right, so it's going to run, access the first list, which is where we see the first name followed by last name, it's going to 
drop the head of the list, which is the first name, access the new head of the list, which is the last name, then complete. Therefore it will sort by the last name.
If you run the command, it does sort as expected:

```

 #((("Kathy" "Chapman") "Editorial")
     (("Dick" "Gabriel") "Objects")
     (("Gregor" "Kiczales") "Objects")
     (("Sandra" "Loosemore") "Compiler")
     (("Larry" "Masinter") "Cleanup")
     (("David" "Moon") "Objects")
     (("Kent" "Pitman") "Conditions")
     (("Dick" "Waters") "Iteration")
     (("JonL" "White") "Iteration"))
     
```

Just remember, **sort** isn't guaranteed to work, but **stable-sort** is, so you may want to stick with stable-sort. For example, if we sort the same list, by job title (and we
can, by the way, just switch the #'key to cadr instead of cadar!) we may not get an alphabetical sort by title, but if we use **stable-sort** we will definetly get:

```

((("Larry" "Masinter") "Cleanup") 
(("Sandra" "Loosemore") "Compiler")
(("Kent" "Pitman") "Conditions") 
(("Kathy" "Chapman") "Editorial")
(("Dick" "Waters") "Iteration") 
(("JonL" "White") "Iteration")
(("David" "Moon") "Objects") 
(("Dick" "Gabriel") "Objects")
(("Gregor" "Kiczales") "Objects"))
  
  ```



