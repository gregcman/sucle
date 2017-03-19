#version 120
attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;

varying vec2 TexCoord;
varying float mycolor;

uniform mat4 projectionmodelview;

void main()
{
gl_Position = projectionmodelview * position;
TexCoord = texCoord;
mycolor = darkness;

} 
