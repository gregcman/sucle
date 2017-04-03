#version 120
varying vec2 FTEX;

uniform vec3 BG = vec3(0.0, 0.0, 0.0);
uniform vec3 FG = vec3(1.0, 1.0, 1.0);

uniform sampler2D samptwodee;
void main()
{

vec4 texcolor = texture2D(samptwodee, FTEX);
if(0.0 == texcolor.a){gl_FragColor.xyz = BG;}
else {gl_FragColor.xyz = FG;}
}

