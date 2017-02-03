#version 120
attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;

varying vec2 TexCoord;
varying float mycolor;
varying float fogratio;


uniform mat4 projectionmodelview;
uniform sampler2D ourTexture;

uniform float foglet = -1.0/(96.0);
uniform float aratio = 4.0/3.0;

void main()
{
///position as fast as its going to get
gl_Position = projectionmodelview * position;

///nothing to say here...
TexCoord = texCoord;

///multiply then add is one instruction. then minimum
fogratio = min(gl_Position.z*foglet+aratio, 1.0);

///max is some amount of cycles, timeday multiply is one, dot is one
mycolor = darkness;


} 
