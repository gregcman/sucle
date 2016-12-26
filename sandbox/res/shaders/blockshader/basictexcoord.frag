#version 120
varying vec2 TexCoord;
varying vec4 mycolor;
varying float fogratio;

uniform sampler2D ourTexture;
uniform vec4 fogcolor;

void main()
{

	gl_FragColor= mix(fogcolor, mycolor *  texture2D(ourTexture, TexCoord), fogratio);
}

