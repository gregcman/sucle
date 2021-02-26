sucle.lisp:
	- The entry point of the application
	- The main loop
	- Spaghetti controls [TODO] cleanup

physics.lisp:
	- [TODO] mess, clean up

render.lisp:
	- Deals with OpenGL
	- sucle + world -> OpenGL
	- Render cubes, set up shaders, clear the canvas, etc...

voxel-chunks:
	- A 3d data structure that allows access by (x y z) -> lisp object

world.lisp:
	- serialize chunks to a file
	- save and load new chunks based on player position
	- block format specification
	- types of blocks and how to mesh them

extra.lisp:
	- Miscellaneous drawing facilities

queue.lisp:
	- A queue with unique items that can be sorted.

mesher.lisp:
	- World -> sequence of vertices -> OpenGL
	- Convert from the World to a sequence of vertices
