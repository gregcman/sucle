uniform float time;
varying vec3 colour;
const float pi2=2.0*3.14159265;
void main()
{
  gl_FragColor = vec4(pow(sin(colour.r*pi2*4.0+mod(time*8.0,pi2)),2.0),
	              pow(sin(colour.g*pi2*4.0+mod(time*8.0,pi2)),2.0),
	              pow(sin(colour.b*pi2*4.0+mod(time*8.0,pi2)),2.0),
		      1.0); 
}
