#version 120
varying vec2 TexCoord;
varying float mycolor;
//varying float fogratio;

uniform sampler2D ourTexture;
//uniform vec3 fogcolor;

void main()
{

vec4 texcolor = texture2D(ourTexture, TexCoord);
//vec3 ans = mix(fogcolor, mycolor *  texcolor.rgb, fogratio);
//gl_FragColor.rgb =  ans;

gl_FragColor.rgb = mycolor * texcolor.rgb;

}

