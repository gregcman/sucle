#version 120
varying vec2 FTEX;

varying vec4 BG;
varying vec4 FG;

uniform sampler2D samptwodee;
void main()
{

vec4 texcolor = texture2D(samptwodee, FTEX);

gl_FragColor = mix(BG, FG, texcolor);
}

