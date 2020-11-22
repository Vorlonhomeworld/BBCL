# Basic Decisions

**S**o far, we've had Common List print things for us, now we'll get into Common Lisp's decision making ability. We'll start simple and build
out from there.  First, we'll look at the **Let** and **Case** functions.

**Let** allows us to set up a variable to equal something, for example if I wanted to set up a program that asked for a password, I could use the **Let** function
to set up the password to be a certain passsword *(just a quick note here, this is a REALLY insecure way of doing this and it's only being used as an
example - don't ever do this in real life!)*.  The format would be:

```

(let ((password :sherlock))

```

Notice that this list is still opened, the word "let" starts the list, password :sherlock is a sub list with two parentheses on one side and the other, but
we still have one paranethesis that remained opened, that's because we need to fill in the rest of this example, if we close it, we may not be able 
to refer back to it with the set of commands that actually verify this is the correct password! Note that this is a very simple example which will not
take user input - we'll expand on this and make it take user input later on, but for now, we'll have it loop through three or more 
possible choices of password without user input.  

To get it to compare our password with the next set of passwords, we'll use the **Case** command. The case command runs through our choices and
sees if any of these choices match our password. Notice that it's indented. It's formatted that way to show that there are two loops (a list inside a list) 
running together.  With one list being indented, it makes it very easy to see that the indented list is a sub-list (in otherwords, it depends on the first list) 
of the first list. Please indent it, it makes it easier to find and debug!  

The case loop looks like this:

```
(case password
(:enter "Ha ha! Not even close!")
(:password "Usually, this is a good password choice for a typical windows admin, however, we're not, so it's not right!")
(:p455w0rd "No L33t speak, please!")
(otherwise "Please try again!")))

```

This gives our case loop 3 password to work with and an answer to give if it's not the ones above it, nor is it correct.

Running this in SLIME gives us an answer of "Please try again!" because none of the password choices match the password we set at the top with our let function. 
That function is the equivalent of saying "Let the password equal "Sherlock".:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Repl_password.png" height="600" width="950"></a>



