#version 120
attribute vec4 POS;
attribute vec2 TEX;
attribute vec4 BGCOL;
attribute vec4 FGCOL;

varying vec2 FTEX;
varying vec4 BG;
varying vec4 FG;

uniform mat4 PMV;

void main()
{
gl_Position = 

PMV * POS;
FTEX = TEX;
BG = BGCOL;
FG = FGCOL;

} 
