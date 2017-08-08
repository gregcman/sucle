#version 120

attribute vec4 position;
attribute vec2 texCoord;
attribute float darkness;

uniform mat4 projectionmodelview;
uniform sampler2D ourTexture;
uniform vec3 cameraPos;
uniform float foglet;
uniform float aratio;

varying vec2 TexCoord;
varying float mycolor;
varying float fogratio;

void main()
{
gl_Position = projectionmodelview * position;
mycolor = darkness;
TexCoord = texCoord;
fogratio = min(1.0, distance(cameraPos, position.xyz) * foglet + aratio);
}
