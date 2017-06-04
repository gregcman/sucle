#version 120
attribute vec4 POS;
attribute vec2 TEX;
attribute vec2 INDIRECT;

varying vec2 FTEX;
varying vec4 BG;
varying vec4 FG;

uniform mat4 PMV;
uniform sampler2D indirection;
uniform sampler2D fgindirection;
uniform sampler2D bgindirection;

void main()
{
gl_Position = PMV * POS;

vec4 chardata = texture2D(indirection, INDIRECT);

FTEX = mix(chardata.rg, chardata.ba, TEX);
BG = texture2D(fgindirection, INDIRECT);
FG = texture2D(bgindirection, INDIRECT);

} 
