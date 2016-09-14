 #version 330 core
in vec2 TexCoord;
in vec4 mycolor;

in float dist;

out vec4 color;

uniform sampler2D ourTexture;

void main()
{	
    color = mycolor * texture(ourTexture, TexCoord);
	if(color.a < 0.1)
			discard;
	
	color = mix(color, vec4(1.0, 1.0, 1.0, 1.0), dist/50);
}
