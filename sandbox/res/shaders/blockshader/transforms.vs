#version 130
in vec4 position;
in vec2 texCoord;
in vec4 color;
in vec4 skyLight;
in vec4 blockLight;

out lowp vec2 TexCoord;
out lowp vec4 mycolor;
out float fogratio;
  
uniform mat4 modelview;
uniform mat4 projection;
uniform lowp float timeday;
uniform lowp float foglet = 1.0f/(-96.0f);
uniform lowp float aratio = 4.0f/3.0f;

void main()
{
	
	vec4 viewspace = modelview * position;
	gl_Position = projection * viewspace;
	TexCoord = texCoord;
	
	fogratio = clamp(length(viewspace)*foglet+aratio, 0.0, 1.0);
	mycolor = color * dot(max(blockLight,timeday*skyLight), vec4(0.25));
	
} 
