#version 120
attribute vec4 position;
attribute vec2 texCoord;
attribute vec4 color;
attribute vec4 skyLight;
attribute vec4 blockLight;

varying vec2 TexCoord;
varying vec4 mycolor;
varying float fogratio;
  
uniform mat4 modelview;
uniform mat4 projection;
uniform float timeday;
uniform float foglet = -1 / 96;
uniform float aratio = 4 / 3;

void main()
{
	
	vec4 viewspace = modelview * position;
	gl_Position = projection * viewspace;
	TexCoord = texCoord;
	
	fogratio = clamp(length(viewspace)*foglet+aratio, 0.0, 1.0);
	mycolor = color * dot(max(blockLight,timeday*skyLight), vec4(0.25));
	
} 
