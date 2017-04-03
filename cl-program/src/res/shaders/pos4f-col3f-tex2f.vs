#version 120
attribute vec4 POS;
attribute vec2 TEX;
attribute vec3 COL;

varying vec2 FTEX;
varying vec3 FCOL;

uniform mat4 PMV;

void main()
{
gl_Position = 

PMV * POS;
FTEX = TEX;
FCOL = COL;

} 
