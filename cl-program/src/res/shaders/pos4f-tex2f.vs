#version 120
attribute vec4 pos;
attribute vec2 tex;

varying vec2 ftex;

uniform mat4 pmv;

void main()
{
gl_Position = 

pmv * pos;
ftex = tex;

} 
