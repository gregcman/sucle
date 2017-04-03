#version 120
varying vec2 ftex;
varying vec3 fcol;

uniform sampler2D samptwodee;
uniform vec3 bg;
void main()
{

vec4 texcolor = texture2D(samptwodee, ftex);
if(0.0 == texcolor.a){gl_FragColor.xyz = bg;}
else {gl_FragColor.xyz = texcolor.xyz * fcol;}
}

