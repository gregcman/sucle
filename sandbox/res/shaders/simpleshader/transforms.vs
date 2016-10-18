#version 120
attribute vec4 position;
attribute vec2 texCoord;
attribute vec4 color;

varying vec2 TexCoord;
varying vec4 mycolor;
varying float fogratio;
  
uniform mat4 modelview;
uniform mat4 projection;

void main()
{
	
	vec4 viewspace = modelview * position;
	gl_Position = projection * viewspace;

	TexCoord = texCoord;
	mycolor = color;
	
} 
