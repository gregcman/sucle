#version 130
in vec4 position;
in vec2 texCoord;
in vec4 color;

out lowp vec2 TexCoord;
out lowp vec4 mycolor;
out float fogratio;
  
uniform mat4 modelview;
uniform mat4 projection;

void main()
{
	
	vec4 viewspace = modelview * position;
	gl_Position = projection * viewspace;

	TexCoord = texCoord;
	mycolor = color;
	
} 
