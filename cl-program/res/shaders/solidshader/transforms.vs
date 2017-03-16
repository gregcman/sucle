#version 120
attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;

varying vec2 TexCoord;
varying float mycolor;

uniform mat4 projectionmodelview;

void main()
{
///position as fast as its going to get
gl_Position = projectionmodelview * position;

///nothing to say here...
TexCoord = texCoord;

///max is some amount of cycles, timeday multiply is one, dot is one
mycolor = darkness;

} 
