#version 120
attribute vec4 POS;
attribute vec2 TEX;
attribute vec2 INDIRECT;

varying vec2 FTEX;
varying vec4 BG;
varying vec4 FG;

uniform sampler2D indirection;

uniform vec4 texcoords[256];
uniform vec4 fgcolor[256];
uniform vec4 bgcolor[256];


void main()
{


vec4 chardata = texture2D(indirection, INDIRECT);



if (chardata.a == 0.0)
{
gl_Position = POS;
int codeindex = int(255.0 * chardata.r);
int fgindex = int(255.0 * chardata.g);
int bgindex = int(255.0 * chardata.b);
vec4 boxdata = texcoords[codeindex];
FTEX = mix(boxdata.rg, boxdata.ba, TEX);
BG = bgcolor[bgindex];
FG = fgcolor[fgindex];
} else {
gl_Position = vec4(10000.0);
}

} 
