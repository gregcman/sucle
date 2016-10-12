#version 130
in lowp vec2 TexCoord;
in lowp vec4 mycolor;
in lowp float fogratio;

out lowp vec4 color;

uniform sampler2D ourTexture;
uniform lowp vec4 fogcolor;

void main()
{
	vec4 imagecolor = texture(ourTexture, TexCoord);
	if(imagecolor.a < 0.1){discard;}
	color= mix(fogcolor, imagecolor * mycolor, fogratio);
}

