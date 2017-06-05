varying vec3 colour;
void main()
{
  colour = gl_Color.rgb;
  gl_Position = ftransform();
}
