

Contents
-
1. Requirements
2. How to install Common Lisp and quicklisp
3. How to use

If you:  
- Are new to Common Lisp, start at the beginning.
- Are a Common Lisp and Quicklisp pro, skip to step 3, "How to use".
- Have a system which is not a combination of {x86, x86-64}{Mac, Windows, Linux}, you will have to build GLFW 3.2 yourself

1.Requirements
-
- A Common Lisp which supports [CFFI](https://common-lisp.net/project/cffi/) and [bordeaux-threads](https://common-lisp.net/project/bordeaux-threads/)
- [quicklisp](https://www.quicklisp.org/beta/)
- [OpenGL](https://www.opengl.org/) 
- an internet connection to download quicklisp libraries


2.Installing Common Lisp and Quicklisp
-
[see here](https://github.com/gregcman/shared-documents/blob/master/INSTALL-CL-AND-QUICKLISP.md)

3.How to use
-

`(ql:quickload :sucle)`  
`(sucle:start)`

Upon starting you will see a black screen. 
[TODO]:add a splash screen and startup menu. Add a test world
1. Click on the black area and press "E"
2. The mouse should disappear, and the test world will load if it exists.
3. Now you will be able to move around

F - fly  
E - toggle input  
WASD move  
Space jump  
shift down when flying  
Scroll wheel change block  
Right click to place block left click to remove  

## How to really use

Using the command line as a REPL is a very difficult way to progam.  
Recommended IDEs:  
- [emacs](https://www.gnu.org/software/emacs/) + [SLIME](https://www.cliki.net/slime-howto)
- [lem](https://github.com/cxxxr/lem)
