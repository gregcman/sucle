## So What Is This Repository?
- a game engine: graphics, sound, some physics
- bindings for various c libraries
- a few end-user[me] applications
- personal utility libraries
- This repository could probably be broken into multiple repositories, but I currently do not see the need.

## Personal Use Cases
![voxel game](https://user-images.githubusercontent.com/14166099/39225064-57b43bfa-4818-11e8-9f33-4737ae6f18b7.png)  
I've spent many hours building and jumping around in this Minecraft Clone. I really liked the old minecraft. I'm quite certain this project infringes on Minecraft's Copyrights. It can load and save minecraft worlds. It was quite fun to build worlds using interactive development with Common Lisp. Here's a [clip](https://www.youtube.com/watch?v=DJLquOyreQQ) of me jumping around randomly. 

![terminal emulator](https://user-images.githubusercontent.com/14166099/39225409-3571051c-481a-11e8-8160-422a7052e605.png)  
The graphics and input for [3b's terminal emulator](https://github.com/3b/3bst). I don't use it, but it works as a dumb terminal emulator.

## Requirements
- Glfw3
- FFmpeg
- OpenAL
- OpenGL
- Common Lisp
- Some quicklisp libraries

### Platforms
Tested on:
- Ubuntu 16.04 sbcl x86_64

## Filesystem Layout
- application/
    - Interactive applications
    - Subsystems	
		
- component/
    - Convenience macros
    - Utility libraries
    - Math
    - Data structures

- foreign/
    - Foreign library bindings
    - Binaries
    - Unmodified forks
    - Convenience wrappers around standard APIs and libraries

- unused/
    - random code which is not necessarily maintained or functional
