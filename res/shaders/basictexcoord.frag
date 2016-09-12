 #version 330 core
in vec2 TexCoord;
in vec4 mycolor;

out vec4 color;

uniform sampler2D ourTexture;

void main()
{	
    color = mycolor * texture(ourTexture, TexCoord);
}
