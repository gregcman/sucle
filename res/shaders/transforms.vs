
#version 130
in vec3 position;
in vec2 texCoord;
in vec4 color;

in vec4 skyLight;
in vec4 blockLight;

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
	vec4 brightness = max(blockLight,timeday*skyLight);
	
	float avg = (brightness.x+brightness.y+brightness.z+brightness.w)/4.0;
	float wow = pow(0.8, (15.0 - avg));

	vec4 viewspace = view * model * vec4(position, 1.0f);
	gl_Position = projection * viewspace;
	TexCoord = texCoord;

	fogfunc(distance(vec3(viewspace.x, viewspace.y, viewspace.z), vec3(0, 0, 0)), fogratio);	
	mycolor = vec4(wow,wow,wow,1.0)*color;
	
} 
