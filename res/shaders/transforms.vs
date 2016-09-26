
#version 130
in vec3 position;
in vec2 texCoord;
in vec4 color;

in float skyLight;
in float blockLight;

out vec2 TexCoord;
out vec4 mycolor;

out float fogratio;
  
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

uniform float timeday;

float fogfunc(in float ledistance, out float daratio)
{
	return daratio = clamp((128 - ledistance)/96, 0.0 ,1.0);
}

void main()
{
	float wow = max(blockLight,timeday*skyLight);	

	vec4 viewspace = view * model * vec4(position, 1.0f);
	gl_Position = projection * viewspace;
	TexCoord = texCoord;

	fogfunc(distance(viewspace.xyz, vec3(0)), fogratio);	
	mycolor = vec4(wow,wow,wow,1.0)*color;
	
} 
