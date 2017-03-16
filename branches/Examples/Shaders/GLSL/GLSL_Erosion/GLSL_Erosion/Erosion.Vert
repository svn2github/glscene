// Vertex shader for erosion effect
uniform vec3 LightPosition;
uniform float Scale;

varying float LightIntensity;
varying vec3 Position;

void main(void) {
   vec4 pos       = gl_ModelViewMatrix * gl_Vertex;
   Position       = vec3(gl_Vertex) * Scale;
   vec3 tnorm     = normalize(gl_NormalMatrix * gl_Normal);
   float dotval   = abs(dot(normalize(LightPosition - vec3(pos)), tnorm));
   LightIntensity = dotval * 1.5;
   gl_Position    = ftransform();
}