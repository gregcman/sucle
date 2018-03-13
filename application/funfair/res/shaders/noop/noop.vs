#version 120

attribute vec4 position;
attribute vec4 color;

varying vec4 col;

void main()
{
gl_Position = position;
col = color;
}
