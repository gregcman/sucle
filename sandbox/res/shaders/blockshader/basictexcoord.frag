#version 120
varying vec2 TexCoord;
varying vec4 mycolor;
varying float fogratio;

uniform sampler2D ourTexture;
uniform vec4 fogcolor;

void main()
{
	vec4 imagecolor = texture2D(ourTexture, TexCoord);
	if(imagecolor.a < 0.1){discard;}
	gl_FragColor= mix(fogcolor, imagecolor * mycolor, fogratio);
}

