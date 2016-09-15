
#version 130
in vec3 position;
in vec2 texCoord;
in vec4 color;

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
