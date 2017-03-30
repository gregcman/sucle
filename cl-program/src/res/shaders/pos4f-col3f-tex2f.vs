#version 120
attribute vec4 pos;
attribute vec2 tex;
attribute vec3 col;

varying vec2 ftex;
varying vec3 fcol;

uniform mat4 pmv;

void main()
{
gl_Position = 

pmv * pos;
ftex = tex;
fcol = col;

} 
