## Installation Time

OK, so you've decided on **SBCL**, good stuff!  **(\* Note:** If you decided on a different implementation of Common Lisp, feel free to skip
this chapter as it describes installation for SBCL and won't be of much help with other versions of Common Lisp).

So, head over to [SBCL's website](https://www.sbcl.org), click on the [download link](https://www.sbcl.org/platform-table.html) and locate your
operating system and **correct architecture** (32-bit, 64-bit, which processor..etc...) and click on the correct link. The next few
lines are going to assume Linux is being used (as that's what I use). Version 2.0.9 is the newest version for linux, so I'd click on that link. 
Once it's finished downloading, go ahead and un-pack it (make sure you call it by the correct name, this example assumes the newest 
version of SBCL for Linux which is 2.0.9!):

```
bzip2 -cd sbcl-2.0.9-x86-linux-binary.tar.bz2 | tar xvf -
```

**bzip2** is the program needed to unzip any file with a .bz2 extention

**-cd** is put after bzip2 , the **c** is used to tell the system we want to send the next command to the screen (the standard output).

**d** is used to decompress the file.

**|** is a pipe command used to add another command to the same line, we don't actually need it, we could run the bzip2 on one line
      then run the tar command on a second line, the **|** tells Linux we want to run two commands, back to back, on the same line.

**tar** is being run because the file type (extension) has changed after going through bzip2, it's now a .tar file and 
        tar is what's needed to uncompress the file. *(Incidentally, "TAR" is a portmanteau of TApe aRchive, that tells you
        how far back that command goes!)*
        
**xvf** is added after the tar command and it asks tar to do three things:

**x** tells that tar command to e**X**tract the file or files in this archive

**v** is the verbose command, it tells tar to list what it's extracting to the screen

**f** tells the tar command to do the prior two comamnds to the file. If we didn't use the **f** command , tar would by default
      look for a tape device to do this with.
      
This operation will create the new directory:

```
sbcl-2.0.9-x86-linux

````

in the directory you're working with.

You can then install SBCL to the /usr/local directory in your Linux installation by entering the following command:

```
cd sbcl-2.0.9-x86-linux
sh install.sh
```

**cd** tells Linux you're changing the directory to the one you're entering after that command, in this case, it's the directory **sbcl-2.0.9-x86-linux**.
**sh** is used to run any bash command, typically they'll end in **.sh**, but not always. Alternatively, you can run this command by entering **./install.sh**.


If you want SBCL to be installed in a directory **other** than /usr/local, you'll need to enter a different command:

```
INSTALL_ROOT=/my/sbcl/prefix sh install.sh
```

**INSTALL_ROOT** isn't a system command, it's a command that exists within your Common Lisp directory. Common Lisp needs to know where it's home (root) directory
is, so this command is used to give it that information.

If you've installed SBCL to the /usr/local directory, you can run Common Lisp!  However, if you ran the **INSTALL_ROOT** command, you need
to do one more thing, you need to let the system know where the ***SBCL_HOME*** variable path actually is (where SBCL is installed), you 
can do that with this either of **two** commands *(depending on which shell you're using!)*:

```
export SBCL_HOME=/my/sbcl/prefix/lib/sbcl   (for bash / zsh)
setenv SBCL_HOME /my/sbcl/prefix/lib/sbcl   (for csh / tcsh)
```

**export** tells Linux where to look for it's new home (in the Bash Shell and Zsh's shell)

**setenv** is the same command, only it's the one to use if you're using the Csh or the Tcsh shell.





That's literally it!  You now have the Common Lisp environment on your computer. Now let's start it up....


