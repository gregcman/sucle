#version 120
varying vec2 TexCoord;
varying float mycolor;

uniform sampler2D ourTexture;
uniform vec3 fogcolor;

void main()
{

vec4 texcolor = texture2D(ourTexture, TexCoord);
if(0.0 == texcolor.a){discard;}
gl_FragColor = vec4(mycolor *  texcolor.xyz, texcolor.a);
}

