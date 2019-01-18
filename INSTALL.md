

Contents
-
1. Requirements
2. How to install Common Lisp
3. How to install quicklisp
4. How to use
5. Opengl Mishaps

If you:  
- Are new to Common Lisp, start at the beginning.
- Are a Common Lisp and Quicklisp pro, skip to step 5, "How to use".
- Have a system which is not a combination of {x86, x86-64}{Mac, Windows, Linux}, you will have to build GLFW 3.2 yourself

1.Requirements
-
- A suitable Common Lisp
- [This other utility library](https://github.com/terminal625/utility)
- [sucle, this repository](https://github.com/terminal625/sucle)
- [quicklisp](https://www.quicklisp.org/beta/)
- [OpenGL](https://www.opengl.org/) 
- [GLFW 3.2](https://www.glfw.org/)
- an internet connection to download quicklisp libraries

Installation
-

### 2. Installing/choosing the Common Lisp implementation:
[Install SBCL](http://www.sbcl.org/). This means clicking on your architecture [from this table](http://www.sbcl.org/platform-table.html) and running the platform specific installer.
If using Linux, an easier way to install is to enter
`apt-get install sbcl` at the command line.

Otherwise [skip if using SBCL]:
Things to keep in mind when using another Common Lisp implementation:
- [ECL](https://common-lisp.net/project/ecl/) has been tested before, but I do not recommend it as ECL creates a lot more garbage than SBCL thus causing frame stuttering on garbage collection.
- If the implementation's [internal-time-units-per-second](http://clhs.lisp.se/Body/v_intern.htm) is too low, the microsecond clock may become too coarse grained, thus messing up frames.
- The implementation needs to support [CFFI](https://common-lisp.net/project/cffi/) and [bordeaux-threads](https://common-lisp.net/project/bordeaux-threads/)

### How to run the Common Lisp REPL
Most implementations start through an executable that has a similar name as the implementation. In the case of installing SBCL with a Linux package manager, entering `sbcl` at the command line will run SBCL. However, there are installation situations in which you may need to execute a script or navigate to a directory to run the Common Lisp implementation, and this is implementation specific. 

### 3. Using Quicklisp:
[Quicklisp webpage](https://www.quicklisp.org/beta/)  
What is Quicklisp? Quicklisp is the de facto package manager for Common Lisp. 
#### Installation
[Installation instructions](https://www.quicklisp.org/beta/#installation)
1. Download the [quicklisp.lisp](https://beta.quicklisp.org/quicklisp.lisp) file. 
2. Open up your Common Lisp REPL
3. [Load](http://clhs.lisp.se/Body/f_load.htm) the file into Common Lisp. It should look something like this:  
`CL-USER> (load "~/path-to-the-quicklisp-file-goes-here/quicklisp.lisp")`
4. Install quicklisp in the default home location. For a custom installation directory, see the [quicklisp website](https://www.quicklisp.org/beta/#loading).  
`CL-USER> (quicklisp-quickstart:install)`
5. [optional] Add quicklisp to implementation startup with   
`CL-USER> (ql:add-to-init-file)`

Everything Quicklisp downloads, which is for the most part Common Lisp code, should be inside the `quicklisp` directory. Removing quicklisp amounts to deleting the `quicklisp` directory and being careful to remove the [Optional from step 5 above] code added by quicklisp to the implementation initialization file.
#### For returning quicklisp users:
1. Open the Common Lisp REPL
2. If quicklisp has been added to implementation startup as in step 5 above then it is already loaded. Otherwise:  
`CL-USER (load "~/path-to-quicklisp/quicklisp/setup.lisp")`

### 4. How to use 
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

### 5. Will My OpenGL setup work out of the box?
Yes if:
- If your OpenGL driver provides a compatibility profile, [which is likely for NVidia GPUs](https://stackoverflow.com/questions/4113989/why-were-display-lists-deprecated-in-opengl-3-1), for supporting display lists.

Otherwise:
For unsupported display lists, a few places would need minor edits to switch from display lists to vertex arrays for creation, deletion, rendering, and iterating, which requires an ability to refactor Common Lisp code.
- If display lists are not supported, the code must be refactored to use vertex arrays. If you are having trouble with this please [open an issue](https://github.com/terminal625/sucle/issues). The transition is straightforward as the display lists are used as nothing but interleaved float data. Display lists were chosen over vertex arrays because they were faster on my particular NVidia GPUs.
