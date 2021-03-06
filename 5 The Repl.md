# The REPL

Now you've downloaded your Common Lisp. You're ready to get started in your coding adventure! So, on the command line (or your shell, whichever
it happens to be), type in:

```
sbcl
```
and you should be greeted by:

<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Repl1.png" height="700" width="900"></a>



*(apologies to all Greek speakers, yes my computer's name is a bit rude!
In case you're wondering, the terminal is qterminal and the Window Manager is Xfce4 - not my usual window manager!)*

This is what's known as the **REPL**.  The Repl is an acronym that stands for **R**ead, **E**valuate, **P**rint and **L**oop, because that's exactly what it 
does, it reads your input, evaluates it (checks it for errors, makes sure everything's ok), prints whatever your asking for to the screen, unless you
tell it otherwise, then does it all over again. Some people call it the language shell, however, it's a bit more versatile than that, even if it doesn't
seem so at first! The REPL's primary purpose is to check the code you're writing, but it can also be called to run Common Lisp scripts and I'll show
you that in just a moment. SBCL's REPL also has a debugger built in as well.

## First Steps

Even though you've read the documentation from the website for your Common Lisp implementation and it's told you how complaint it is with Common Lisp, you still want to verify within the REPL which version of Common Lisp
this REPL supports, as there are at least three versions, one of which is the ANSI STANDARD. Simply type this into the REPL and hit enter:

```
(let ((dialects '()))
               (dolist (d '(:ansi-cl :cltl2 :cltl1))
                 (when (member d *features*) (push d dialects)))
               dialects)
```
This set of code tests the type of dialect your Common Lisp is using, and it's looking for one of three responses:

**Ansi-cl**  - This is the one you want as it will be the most portable version

**CLTL1** "Common Lisp, the Language, Edition 1" - This is one of the first books about Common Lisp published. This was published in 1984 so it won't conform to
           the ANSI Standard.
           
**CLTL2** "Common Lisp, the Language, Edition 2" - This is the second edition of "Common Lisp, The Language". This was published in 1990, so it too wouldn't conform to 
           the ANSI Standard.
           
If all works well,  you'll see:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/REPL2.jpg"></a>


*(Yes, this is a seperate operating system. I use this one for work, it's Windows 10)*

Indicating that the implementation of Common Lisp really does use the ANSI Standard for Common Lisp!

With that in mind, there's one other step we should take while we're in
here, and that would be to install QuickLisp!


### What is Quicklisp ###

Quicklisp is a package manager for Common Lisp. As of this writing is has
[1,500 libraries available](https://www.quicklisp.org/beta/releases.html)
and you can download and install them with quicklisp - and to boot, it's
prettty easy to work with! Yes you can use Common Lisp's **asdf** (which
means [Another System Definition Facility](https://common-lisp.net/project/asdf/#:~:text=ASDF%20is%20what%20Common%20Lisp,for%20Another%20System%20Definition%20Facility.&text=And%20you%20must%20typically%20compile,other%20files%20that%20use%20them.) ), however, ASDF is like the
Linux "Make" command where it compiles and causes a library to be useable,
quicklisp downloads, compiles, stores and can call a Common Lisp library
in very easy steps!

Just in case you're not clear what a library is, it's a file or a 
collection of files that add functionality into Common Lisp. For example,
the window manager I'm using right now, StumpWM, is written in Common Lisp, 
all of the graphic capabilities are being handled by CLX, which is a 
Common Lisp library to draw graphics in X-Windows. With C or a few 
other languages, you actually have to learn a different langage (TCL/TK, 
GTK, X-Windows..etc...) to do anything with graphics, with Common Lisp, you
simply add the library and keep going, in Common Lisp!

So, to load QuickLisp, open up your terminal and type in:

```

$ curl -O https://beta.quicklisp.org/quicklisp.lisp

```

As long as you have an internet connection, you should see Common Lisp
download quicklisp into your system.

The next thing you want to do is to verify that this is a correct and
correctly signed version of Quicklisp, step one for that is to 
type in:

```

curl -O https://beta.quicklisp.org/quicklisp.lisp.asc

```

This file is going to be read in a moment to verify if the signature is
correct.  Once it's finished downloading, the second part of the 
of the verification can be done.  Type this in (still in the repl):

```

gpg --verify quicklisp.lisp.asc quicklisp.lisp

```

This checks what the correct signature should be, which is in the 
.asc file, against what the signature is in the quicklisp.lisp file
and it should verify that they match. If they do, you can continue
with your installation (and they should as long as you download
from beta.quicklisp.org).

The next thing we'll need to do is to load in quicklisp, this 
will send a lot of output to your screen and load in a few files.
It's perfectly ok.  Go back to your REPL and  type this in:

```

(quicklisp-quickstart:install)

```

This actually installs quicklisp in and makes it able to run through
your REPL.

Since we're still in the REPL and we just put in Quicklisp, let's 
try it out by using it to install Vecto-1.5. Since I hate installing 
anything without knowning what I'm installing and why I should install it,
I'll tell you about Vecto first then you can decide if you want to.
Vecto-1.5 is a simpler interface for Common Lisp's CL-Vectors library.
It allows you to draw graphics  using Common Lisp and you 
can save them as .png files. This library is used by Movie Charts and 
Easy Street. That and it's got a [a lot of documentation right over here](https://www.xach.com/lisp/vecto/). It's also pretty easy to install.
Within the REPL type in:

```

(ql:system-apropos "vecto")

```

You're telling quicklisp to install the apropriate version of "vecto" 
into your Common Lisp.

To load it into your quicklisp, just type in:

```

(ql:quickload "vecto")

```

There's one more thing you might want to do here if you plan on using 
quicklisp, and that's add it to your Common Lisp init file (the file
your version of Common Lisp looks at when it first starts). You can
do this within the REPL by typing in:

```

(ql:add-to-init-file)


```

### So what IS this init-file?

Since I mentioned it, I may as well talk a bit about the init file.
We may talk more a bit later on, but for now a little introduction would be
good!  The init file (in Sbcl, (Steel Bank Common Lisp) is called
.sbclrc ) is a file that you'd use if you need or want to have certain 
things running everytime you want to run your REPL.  For example, when 
we ran the (ql:add-to-init-file), you probably saw this printed in your 
REPL:

```

#-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
      
```

This set of lines is telling our REPL where quicklisp is and
where it's set up file is (it's in the line (user-homedir-pathname)
In Linux, the home directory is setup by the operating system and 
is a system variable, so all Lisp has to do is ask the system
where the home directory is and it will be told.

The lines underneath that tell it that when quickisp lisp is run
(with the ql-load command, typically, it's to run the command
(load quicklisp-init).  Because it's in the init (initialization) file, 
this will be seen any time the REPL is run!

There's more that can be done, you can add in libraries to run, 
tell the REPL you want to search online repositories with a 
key command or a lisp library, for example [this initialization file](https://github.com/jonatack/dotfiles/blob/master/sbclrc)
asks Common Lisp to load up a few libraries to assist in coding. Yes,
I realize this looks complex, it really isn't. All the stuff marked
with either ";" or ";;" are comments and don't get run by the system.
It's there to make the code more readable!  This will start to 
make more sense once we get into coding Common Lisp.  Speaking of,
let's get coding!


**Next** [Chapter 6. Hello Lisp](https://github.com/Vorlonhomeworld/BBCL/blob/main/6%20Hello_Lisp.md)
