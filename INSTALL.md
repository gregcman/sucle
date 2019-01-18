

Contents
-
1. Requirements
2. How to install Common Lisp and quicklisp
3. How to use
4. Opengl Mishaps

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
[see here](https://github.com/pupcraft/shared-documents/blob/master/INSTALL-CL-AND-QUICKLISP.md)

3.How to use
-

`(ql:quickload :sucle)`  
`(sucle:start)`

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

4.Will My OpenGL setup work out of the box?
-
Yes if:
- If your OpenGL driver provides a compatibility profile, [which is likely for NVidia GPUs](https://stackoverflow.com/questions/4113989/why-were-display-lists-deprecated-in-opengl-3-1), for supporting display lists.

Otherwise:
For unsupported display lists, a few places would need minor edits to switch from display lists to vertex arrays for creation, deletion, rendering, and iterating, which requires an ability to refactor Common Lisp code.
- If display lists are not supported, the code must be refactored to use vertex arrays. If you are having trouble with this please [open an issue](https://github.com/pupcraft/sucle/issues). The transition is straightforward as the display lists are used as nothing but interleaved float data. Display lists were chosen over vertex arrays because they were faster on my particular NVidia GPUs.
