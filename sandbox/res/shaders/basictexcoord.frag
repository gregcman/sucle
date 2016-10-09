 #version 130
in vec2 TexCoord;
in vec4 mycolor;


out vec4 color;

in float fogratio;

uniform sampler2D ourTexture;

uniform vec3 fogcolor;

void main()
{

 
	color= texture(ourTexture, TexCoord) * mycolor;
	if(color.a < 0.1)
			discard;
	color= vec4(mix(fogcolor, color.rgb, fogratio), color.a);
	
}

