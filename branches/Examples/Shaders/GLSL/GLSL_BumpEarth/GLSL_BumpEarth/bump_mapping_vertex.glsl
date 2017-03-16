attribute mat3 tangentBasis;

uniform vec3 light;
uniform mat4 modelViewI;

varying vec2 uv;
varying vec3 lightVec;
varying vec3 eyeVec;

void main()
{
    // output vertex position
    gl_Position = ftransform();

    // output texture coordinates for decal and normal maps
    uv = gl_MultiTexCoord0.xy;

    // transform light and half angle vectors by tangent basis
    lightVec = light * tangentBasis;

    eyeVec = modelViewI[3].xyz - gl_Vertex.xyz;
    eyeVec = eyeVec * tangentBasis;
}
