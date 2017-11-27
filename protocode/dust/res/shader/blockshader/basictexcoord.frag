#version 100
varying lowp vec2 TexCoord;
varying lowp float mycolor;
varying lowp float fogratio;

uniform sampler2D ourTexture;
uniform lowp vec3 fogcolor;

void main()
{

lowp vec4 texcolor = texture2D(ourTexture, TexCoord);
gl_FragColor.rgb =  mix(fogcolor, mycolor *  texcolor.rgb, fogratio);


}

