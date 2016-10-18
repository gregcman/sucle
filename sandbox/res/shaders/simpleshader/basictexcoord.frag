#version 120
varying vec2 TexCoord;
varying vec4 mycolor;

uniform sampler2D ourTexture;

void main()
{
	vec4 imagecolor = texture2D(ourTexture, TexCoord);
	if(imagecolor.a < 0.1){discard;}
	gl_FragColor= imagecolor * mycolor;
}

