#version 120
varying vec2 FTEX;

varying vec3 BG;
varying vec3 FG;

uniform sampler2D samptwodee;
void main()
{

vec4 texcolor = texture2D(samptwodee, FTEX);
if(0.0 == texcolor.a){gl_FragColor.xyz = BG;}
else {gl_FragColor.xyz = FG;}
}

