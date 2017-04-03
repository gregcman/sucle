#version 120
varying vec2 ftex;

uniform vec3 bg = vec3(0.0, 0.0, 0.0);
uniform vec3 fg = vec3(1.0, 1.0, 1.0);

uniform sampler2D samptwodee;
void main()
{

vec4 texcolor = texture2D(samptwodee, ftex);
if(0.0 == texcolor.a){gl_FragColor.xyz = bg;}
else {gl_FragColor.xyz = fg;}
}

