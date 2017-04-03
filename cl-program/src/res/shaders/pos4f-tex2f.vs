#version 120
attribute vec4 POS;
attribute vec2 TEX;

varying vec2 FTEX;

uniform mat4 PMV;

void main()
{
gl_Position = 

PMV * POS;
FTEX = TEX;

} 
