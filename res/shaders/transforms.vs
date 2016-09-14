
#version 330 core
layout (location = 0) in vec3 position;
layout (location = 4) in vec4 color;
layout (location = 2) in vec2 texCoord;

out vec2 TexCoord;
out vec4 mycolor;

out float dist;
  
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
	
	vec4 cameraspaceposition = view * model * vec4(position, 1.0f);
    gl_Position = projection * cameraspaceposition;
    TexCoord = texCoord;
	mycolor = color;
	dist = cameraspaceposition.z;
} 
