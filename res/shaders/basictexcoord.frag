 #version 130
in vec2 TexCoord;
in vec4 mycolor;


out vec4 color;

in float fogratio;

uniform sampler2D ourTexture;

void main()
{

    color = texture(ourTexture, TexCoord) * mycolor;
   color= mix(vec4(0.68, 0.8, 1.0, color.a), color, fogratio);
	if(color.a < 0.1)
			discard;
	
	
}

