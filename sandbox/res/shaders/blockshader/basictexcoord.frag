#version 100
varying highp vec2 TexCoord;
varying highp float mycolor;
varying highp float fogratio;

uniform sampler2D ourTexture;
uniform highp vec3 fogcolor;

void main()
{

highp vec4 texcolor = texture2D(ourTexture, TexCoord);
gl_FragColor= vec4(mix(fogcolor, mycolor *  texcolor.xyz, fogratio), texcolor.a);

//gl_FragColor=texture2D(ourTexture, TexCoord);

}

