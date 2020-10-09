## How do I get Lisp 

**Well** .... before we do that, you should know that you have choices to make here. Even though Common Lisp is a single unified language, it doesn't have 
a single leader, like , say Python, so there's not just one source for it. This means that there are different implementations of Common Lisp available,
each with their own purpose.  Just to give you an idea - I'll show you the different versions, then I'll pare the list down considerably:

**Architecture:**  1 - 64-bit Dec,  2 - Acron RISC, 3- Alpha, 4 - Amiga 500 - 4000, 5 - Android, 6 - AIX, 7 - Apollo, 8- Butteryfly Machine, 9 - Dec
10 - Dec Alpha, 11 - Dec (Ultrix), 12 - DOS, 13 - FreeBSD,  14 - Google App Engine,  15 - I.B.M 9000,  16 - IBM RS/600,  17 - HP700
18 - HP-PA, 19 - HP-UX,  20 - iOS,  21 - JVM,  22 - Linux,  23 - MacOS,  24 - MacOS 7.5 or later,  25 - MacOS (Catalina / Mojave) , 26 - MIPS
27 - MIT Lisp machine, 28 - NetBSD,  29 - NeXT, 30 - NeXT Step,  31 - OpenBSD,  32 - OS/2,  33 - SGI,  34 - Silicon Graphics,  35 - Solaris
36 - Sparc,  37 - Sun3,  38 - Sun4, 39 - Sun DEC Stations,  40 - SunOS,  41 - SVR4,  42 - Unix,  43 - Windows,  44 - Windows 95
45 - Windows 98,  46 - Windows NT,  47 - Windows XP,  48 - Xerox 1186


**Legend:**   $ - Commercial,  A - Active,  F - Free,  C - Caution,  X - Inactive

| Implementation Name | Free  |  Commercial | Architecture      |   Notes                                                   | Legend |
|---------------------|-------|-------------|-------------------|-----------------------------------------------------------|--------|
| \*Lisp              |   X   |             |     ??            | This is an inactive distribution.                         |   F,X  |
| ABCL                |   X   |             | 21,  14           | Runs in the Java Virtual Machine                          |   F,A  |
| ACL                 |   X   |       X     | 45,36,16,17,9,29  | There is a free **AND** a commmerical version available   | F,$,A  |
| AKCL                |       |        X    |37,36,11           | Commercial License only                                   |   $,C  |
| Allegro CL          |       |       X     |22,43,13,35,36     | Current Commercial Implementation                         |   $,A  |
| ALSP                |   X   |             |   ??              | This is a package that runs LISP in ADA. Ada is required  |   X    |
| AwkLisp             |   X   |             |   ??              | This is a Lisp interpreter that runs in Awk. Awk is needed|   A    |
| BBN                 |   X   |             |   8               | Only works on the Butterfly machine.                      |   A    |
| CC.EL               |   X   |             |   ??              | Runs Lisp in Emacs. Emacs is required                     |   A    |
| CLASP               |   X   |             |22, 25             | Current free Implementation, runs in C++ libraries        |   A    |
| CliCC               |   X   |             |   ??              | Runs INSIDE of Common Lisp, converts Lisp to C            |   C    |
| CLISP               |   X   |             |12,32,46,44,4,2,22 | Current Implementation. See further notes below           |   A    |
| CLOE                |       |      X      |27,1,9,12          | Defunct Implementation, no longer available               |   X    |
| Clozure CL          |   X   |             |25,22,13,35,47     | Current Free Implementation of Common Lisp                |   A    |

.... and this isn't even the **whole** list. The actual list is 48 lines long!!  Fortunately, we're not going to list them all. There's really only
8 we need to really look at, and they are:



| Implementation Name                                               | Free |  Commercial|
|-------------------------------------------------------------------|------|------------|
|[ABCL](https://common-lisp.net/project/armedbear/)                 |  X   |            |
|[Allegro](https://franz.com/products/allegro-common-lisp/)         |      |      X     |
|[CLASP](https://github.com/clasp-developers/clasp)                 |  X   |            |
|[CLISP](https://clisp.sourceforge.io/)                             |  X   |            |
|[Clozure CL](https://ccl.clozure.com/)                             |  X   |            |
|[CMU CL](https://www.cons.org/cmucl/)                              |  X   |      X     |
|[ECL](https://common-lisp.net/project/ecl/main.html)               |  X   |            |
|[LispWorks](http://www.lispworks.com/products/lispworks.html)      |  X   |      X     |
|[SBCL](http://www.sbcl.org/)                                       |  X   |            |


The rest of the implementations are either inactive, only work for older mainframes or are just not in existence anymore. Each of the remaining Common Lisp 
implementations emphasize a different facet of Common Lisp, just like different versions of Linux emphasize a different facet of Linux. Most of the time, 
beginners are told to "just get (a particular version)" and that's it. Rather than just leave it there, I'm going to explain the differences between these versions
and allow **you** to make an informed decision.

[ABCL](https://common-lisp.net/project/armedbear/) - ABCL is Armed Bear Common Lisp, a Common Lisp implementation that's meant to run inside the JVM (Java Virtual Machine)
so this is going to be more useful if you're looking to extend Java's capability. You need to have Java to use this as the downloaded file for this is a .jar file 
ABCL features a compiler and an interpreter - but no debugger. **IMPORTANT** This implementation of Common Lisp does **NOT** conform to current Common Lisp Ansi 
standards, so this code would not be portable, in some cases with another Common Lisp implementation. This implemenatation has been updated as of 7/18/2020.


[Allegro](https://franz.com/products/allegro-common-lisp/) - Allegro Common Lisp is a commercial Common Lisp implentation. It's meant to be run in an "Enterprise Server" 
situation where large development in Common Lisp solutions are occuring. It **does** have a debugger, along with an interpreter and a compiler. It can also create
Common Lisp instruction from Java. It does come with a built in database and prolog and other GUI tools for usage. **IMPORTANT** This implementation does **not**
conform to Common Lisp ANSI Standards, therefore code written on this machine may not be portable to another machine. This was most recently updated 8/20/2019.


[Clasp](https://github.com/clasp-developers/clasp) - Clasp  is a free Common Lisp implementation geared for those *really* using C++ but wanting to also add a bit of Common
Lisp into the mix. It uses the [LLVM](https://llvm.org/) compiler, which allows compilation of multiple computer languages. It features a compiler only. **IMPORTANT** This 
implementation does **not** conform to Common Lisp ANSI Standards, therefore code written on this machine may not be portable to another machine. This is hosted on 
Github, various portions of this were updated as late as 2 days ago and as far back as 6 years ago.


[CLISP](https://clisp.sourceforge.io/) - CLISP is a free Common Lisp implementation. It contains an interpreter, a compiler and a debugger. This is written for the desktop
Lisper in mind, so no Java Machines or LLVM compilers. This is just Common Lisp. It also features an extensive help menu online. CLISP conforms "mostly" to 
ANSI standards. The cases where it doesn't are documented openly in their documentation. The implementors admit that CLISP is slower than other implementations of Common Lisp
(in particular, they state is slower than CMU CL). **BE AWARE** CLISP hasn't been updated since 2010!


[Clozure CL](https://ccl.clozure.com/) - Clozure CL is a free Common Lisp that *also* offers paid support for it as well. It contains a compiler and a debugger, *for MacOS* it 
*does have* a graphic UI, but only for MacOS. This was written primarily for Mac Users and power users. All other operatings system will have the standard text interface. It
claims to conform to ANSI Standards of Common Lisp which means code from this Lisp should be fully transportable to any ANSI Conforming version of the Common Lisp compiler /
interpreter. It's hosted on Github, some portions of it were updated as far back as 5 years ago, other portions were updated as recently as 2 days ago.


[CMU CL](https://www.cons.org/cmucl/) - CMU CL is a free Common Lisp implementation that has a compiler, and interpreter and a debugger. The debugger can be run
as a GUI - but it uses Motif as the graphics so you would need to install that. This is a distribution for power users as well. It also comes with an Emacs-like 
(Hemlock) editor. The implementors claim to "mostly" conform to the ANSI Standard for Common Lisp and they do document where they don't in their online docs. 
The docs are extensive but harder to search for as opposed to CLIP. No updates have occured since 12/08/2018.


[ECL](https://common-lisp.net/project/ecl/main.html) - ECL or Embedable Common Lisp is a Lisp implemetation designed for,as the name implies, Embedded Lisp applications. 
It's more for those working with Embedded devices in Common Lisp, or those needing quick easy access to C programs via libraries. It features a byte-code compiler and an
interpreter. It works by producing libraries that can be accessed by the C programs. It states that it's fully compliant with current Common Lisp ANSI standards, so it's 
most certainly portable from one computer to another. It's hosted on Gitlab, and the updates range everywhere from 12 years (for the configuration file) to 3 weeks ago.).


[Lispworks](http://www.lispworks.com/products/lispworks.html) - Lispworks is a commercial Lisp implementation with various platform variations from Professional
to Hobbyist. It aims to be a fully commercial, fully GUI version of Common Lisp, and it also aims to reach out to the widest possible audience, from Professionals, 
embeddeded device user, to the home Common Lisper. It offers many features, depending on which platform you use including a full GUI evelopment environment. It can 
also be used to create apps for Android as well. It states that it's fully ANSI Compliant, meaning the code would be compatible from one Ansi Standard Complaint 
implementation. to another Ansi Stantstf Compliant implementation. This runs on multiple platforms including ARM processors, Android and iOS.  The last update for this 
was 11/13/2017.


[SBCL](http://www.sbcl.org/) - SBCL or  Steel Bank Common Lisp is a free implementation of Common Lisp. It describes it self as a high performance Common Lisp Compiler.
It features a Compiler, a debugger and an interpreter. It is "mostly conforming" to the ANSI Common Lisp Standard. This can be used by power users, but it's not an
implementation that is geared only for powerusers. It can be used by the casual home user as easily as the poweruser. It has versions for Linux, MacOS X, Solaris, FreeBSD
NetBSD, OpenBSD, DragonFly BSD, DebianGnu BSD and Windows. It's latest update is 9/27/2020.


