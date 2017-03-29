#version 120
attribute vec4 pos;
attribute vec2 tex;
attribute vec4 col;

varying vec2 ftex;
varying vec4 fcol;

uniform mat4 pmv;

void main()
{
gl_Position = pmv * pos;
ftex = tex;
fcol = col;

} 
