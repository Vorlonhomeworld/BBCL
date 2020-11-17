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


Now, a little pro tip for emacs is that you can get it to reload it's init file right away by typing in Meta Key (usually it's the "Windows" key if you're
using a Windows keyboard) + A. A window will open up on the bottom and you can type in *load-file* followed by enter. It will ask you which file you want to load
at that point you would need to give it the full path to your emacs init file, .enmacs or  init.el file and hit enter. It will let you know if it reloaded
corrrectly.

Once that's done you can access your packages and you should know see packages from the Melpa archive, including slime.  Just click on it and select install and 
it's installed within Emacs. *(Don't think you're done just yet - there's a bit more to do and we'll get to that in a second!)*

**I**f using github is better for you, you can certainly use github as well. On linux, as long as you have git, you can take care of that by pulling up your
terminal of choice and creating a directory where slime is going to go, in my case, it's a directory in my userspace called "slime". Just enter in:
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

The next set of lines are also customization that allows Slime to open up automatically if I ask Emacs to open up a lisp file

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

