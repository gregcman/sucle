# cl-glfw3
Bindings for the [GLFW library](http://glfw.org/), version 3.x, for Common Lisp. These bindings have been enhanced with several conveniences, as described in `cl-glfw3.lisp`.

Direct bindings to all GLFW functions can be found in the system `%glfw` (`glfw-bindings.lisp`).

cl-glfw3 is Quicklisp installable! Just type `(ql:quickload :cl-glfw3)`. Running the resulting system requires GLFW version 3.x and [libffi](http://sourceware.org/libffi/) to be installed on your computer.

Examples can be found in the `examples` directory. These can be loaded through Quicklisp with `(ql:quickload :cl-glfw3-examples)`. The examples rely on cl-opengl (which Quicklisp will take care of) and require that OpenGL be installed on your computer.

At the moment these bindings are largely untested, aside from what can be seen in the examples directory. Contributions are most welcome!

The best place to find documentation for GLFW3 is [their official site](http://www.glfw.org/docs/3.0/index.html). 
