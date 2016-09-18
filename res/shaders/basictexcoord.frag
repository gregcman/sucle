 #version 130
in vec2 TexCoord;
in vec4 mycolor;

in float dist;

out vec4 color;

uniform sampler2D ourTexture;

void main()
{	
    color = texture(ourTexture, TexCoord) * mycolor;
	if(color.a < 0.1)
			discard;
	
	
}
