#version 120
attribute vec4 POS;
attribute vec2 TEX;
attribute vec3 BGCOL;
attribute vec3 FGCOL;

varying vec2 FTEX;
varying vec3 BG;
varying vec3 FG;

uniform mat4 PMV;

void main()
{
gl_Position = 

PMV * POS;
FTEX = TEX;
BG = BGCOL;
FG = FGCOL;

} 
