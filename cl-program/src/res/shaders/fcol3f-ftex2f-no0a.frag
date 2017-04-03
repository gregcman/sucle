#version 120
varying vec2 FTEX;
varying vec3 FCOL;

uniform sampler2D SAMPTWODEE;
void main()
{

vec4 texcolor = texture2D(SAMPTWODEE, FTEX);
if(0.0 == texcolor.a){discard;}
else {gl_FragColor.xyz = texcolor.xyz * FCOL;}
}

