# Get **SLIME**'d

Since I'm an Emacs user and more familiar with Slime, this is going to be a step-by-step guide for Slime, however,if you'd rather use SLY,
I understand! [Here's a guide](https://joaotavora.github.io/sly/#Platforms) to help you install Sly.  On the other hand, if you're a VI user,
that's okay too, [here's a guide](https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/#get-started) to help you install either SLIMV or 
VLIME! The rest of this chapter is going to be about installing SLIME, so if you're installing something else, feel free to skip this
chapter.

**S**ince we already have Common Lisp *and* Quicklisp, we literally only need to get Slime. Now there are two ways to get this, one will
be from Emacs directly. Emacs has two repositories, **Elpa** and **Melpa**. Elpa is the more stable of the two, so I tend to shy away from 
Melpa myself, but, if you want to use it, you have to tell Emacs, because it won't show up as a source for packages unless you do. 

So to access Melpa, you'll need to access your emacs init file (it's usually called **.emacs** or **init.el**) and add the following lines in Emacs:

```

(require 'package)
(add-to-list 'package-archives
'("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)

```
This piece of code tells emacs that we're adding an archive to it's list of archives and that the archive is called "melpa", and that it's 
web address is over at http://melpa.milkbox.net/packages. That last command is to tell it to run that command everytime emacs is started.


Now, a little pro tip for emacs is that you can get it to reload it's init file right away by typing in Alt Key + A. A window will open up on the bottom and you
can type in *load-file* followed by enter. It will ask you which file you want to load at that point you would need to give it the full path to your emacs
init file, .emacs or init.el and hit enter. It will let you know if it reloaded corrrectly.

Once that's done you can access your packages and you should see packages from the Melpa archive, including slime:


<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/melpa.png" height="600" width="950"></a>


Just click on it and select install and it's installed within Emacs. *(Don't think you're done just yet - there's a bit more to do and we'll get to that in a second!)* 

**I**f you like using github better,you can certainly use github! On linux, as long as you have the program git installed, you can pull up your terminal 
of choice and create a directory where slime is going to go, in my case, it's a directory in my userspace called "slime". Just enter in:
**md slime** and hit enter.  Once you've done that enter that directory *(cd slime)* and in that directory clone the github version of Slime by 
entering **git clone https://github.com/slime/slime.git**.   That command causes the directory on github to copy itself to the directory you set aside for 
slime, in my example, the directory's called slime as well.


**N**ow that you have Slime downloaded, you need one extra program in Common Lisp in order to get Slime to run correctly in Emacs.  Access your REPL and 
inside the repl you'll want to use quickload, so enter *(ql:quickload "quicklisp-slime-helper")*.  This instructs Common Lisps' quickload program to 
download and install slime-helper into the quicklisp directory, it will be our go-between file for emacs. Believe it or not, this is an .el file, so 
Emacs can easily read and understand it!

Now we're finally ready to head over to emacs and add some elisp (that's what .el files are, elisp, which isn't the same as Common Lisp! ) into 
Emacs so that SLIME will run. First, we need to let Emacs know where slime is located on our hard drive:

```

*(add-to-list 'load-path "/home/phoenix/slime")*

```

This tells emacs that slime is located in whichever directory you installed it in. In this example it's in a directory off my home directory "phoenix".

In the next set of lines, I've set up the repl to open automatically when Slime does, it's just easier for me (and the ";;" marks are for comments only)

```

;; SLIME ONLY -- SLY CRRASHES WAY TOO MUCH!!!

;; slime repl is automatic
(slime-setup '(slime-repl))

```

The next set of lines are also customization that allows Slime to open up automatically if I ask Emacs to open up a lisp file. By the way, this piece 
came from [Cliki, another great Common Lisp resource](https://www.cliki.net/SLIME%20Tips)!

```

;; make slime connect if Lisp is open

(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'cliki:start-slime)

```

The last two lines load in the slime helper file we downloaded earlier with quicklisp and set the REPL (also known as the 
inferior lisp) to SBCL, which is the REPL we happen to have on the computer!

```

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

```

Lots of work, huh ? Don't worry - it's worth it to see Slime load up with a repl automatically!  We have one last step - and this step is 
purely *optional*. You don't have to do it unless you want to. This step allows swank to load quicker. I stumbled across this in my early
Slime days - it's shown [over in Common-Lisp.net's page](https://common-lisp.net/project/slime/doc/html/Loading-Swank-faster.html) for Slime.
What you do, is to create a Common Lisp core file for emac's usage. 

It's a lot easier than it sounds - trust me, I've done it and I load it daily. The only thing to be aware of is if you update SLIME or SBCL
you **must** create a new core file! The upside with this is Swank loads extremely quickly. To create that core file, open up your REPL and
enter:


```

* (load ".../slime/swank-loader.lisp")
* (swank-loader:dump-image "sbcl.core-with-swank")

```

Once that's finished you can close out of SBCL and open your emacs init file (either .emacs or init.el) add this in beneath your previous
SLIME code:

```

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-with-swank")
              :init (lambda (port-file _)
                      (format "(swank:start-server %S)\n" port-file)))))
                     
```

If you don't want to have to create a new core everytime you update Slime or SBCL, you can use this process instead (it won't be as quick as the one above,
however!) enter your REPL and enter:

```

* (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
* (save-lisp-and-die "sbcl.core-for-slime")

```

Once that's done, you go over to your emacs init file (once again, either .emacs or init.el) and instead of entering the code above, enter this:

```

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))
      
```

I'll include my .emacs as an example so that you can see what the code looks like, once it's put together. A couple of things to point out,
I'm using the Swank core option so my .emacs shows the sbcl.core-with-swank option in it. Also, I've customized a control key shortcut
for reloading my .emacs on the fly. (I'm also using tabs, which is part of the latest version of emacs):

<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/emacs_setup.png" height="600" width="950"></a>


With any luck, once you start emacs and load a Common Lisp program, you should see this:

<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/slime1.png" height="600" width="950"></a>

Just so we're clear on this, your display might not be split in half like mine is, that's a setting that can be done in Emacs pretty easily,
just press Ctrl + x + 3 and you'll get that vertical split  (Ctrl + x + 2 splits it horizontally and Ctrl + x + 1 stops all splits! ).


## So what's the big deal about SLIME anyway

As I mentioned, SLIME is an I.D.E, it does more than what that one screenshot showed you.  For example, look at the next screenshot and you'll see a 
brief program  (one list long) that's in the process of being typed. Take a look at the bottom of the screen, see the additional text on the bottom?
It's letting me know what the format of the run-program command is, what arguments it takes and what keywords it accepts!  

<a href="rel"><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/Slime_2.png" height="600" width="950"></a>

Not only that, but it also gives you, as I mentioned, a testing space inside Common Lisp. Take a look at that one list in action:

<a href="rel><img src="https://github.com/Vorlonhomeworld/BBCL/blob/main/images/slime_action.png" heigh="600" width="950"></a>
                                                                                                                         
                                                                                                                         

