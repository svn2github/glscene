#ifndef __GLSL_CG_DATA_TYPES
# define half float
# define half3x3 mat3
# define half2 vec2
# define half3 vec3
# define half4 vec4
#endif

uniform float Time;
uniform vec4  EyePos;

varying half3   EyeVec;
varying half3x3 ObjToTangentSpace;

const half cTexScale = 0.15;
const half cBumpScale = 0.2;
const half cBumpSpeed = 0.4;

// Waves parameters

#define NWAVES 3
struct Wave {
  half freq;  // 2*PI / wavelength
  half amp;   // amplitude
  half phase; // speed * 2*PI / wavelength
  half2 dir;
};
Wave wave[NWAVES] = {
	{ 0.2, 0.9, 12, half2(1, 0) },
	{ 0.3, 0.7, 9,  half2(0.98, 0.2) },
	{ 0.4, 0.5, 8,  half2(0.99, -0.15) }
};

const int k = 2;

half2 evaluateWave(Wave w, vec2 pos, float t)
{
    vec2 result;

    float wavePos = dot(w.dir, pos)*w.freq + t*w.phase;
    half waveAmp = sin(wavePos)*0.5 + 0.5;

    result.x = w.amp * pow(waveAmp, k);
    result.y = k*w.freq*w.amp * pow(waveAmp, k-1) * cos(wavePos);

    return result;
}

void main()
{
    vec4 P = gl_Vertex;

    // sum waves
	vec2 dd = 0.0;
	for(int i=0; i<NWAVES; i++) {
    	vec2 waveEval = evaluateWave(wave[i], P.xy, Time);
    	P.z += waveEval.x;
        dd += waveEval.y * wave[i].dir;
    }

    gl_Position = gl_ModelViewProjectionMatrix * P;

	// compute tangent basis
    half3 B = half3(1.0, 0.0, dd.x);
    half3 T = half3(0.0, 1.0, dd.y);
    half3 N = half3(-dd.x, -dd.y, 1);

	// compute the 3x3 tranform from tangent space to object space
	// first rows are the tangent and binormal scaled by the bump scale
	ObjToTangentSpace[0] = cBumpScale * normalize(T);
	ObjToTangentSpace[1] = cBumpScale * normalize(B);
	ObjToTangentSpace[2] = normalize(N);

	float texTime = Time*cBumpSpeed;
    gl_TexCoord[0].xy = gl_Vertex.xy*cTexScale + texTime;
    gl_TexCoord[1].xy = gl_Vertex.xy*(2*cTexScale) - texTime;

    EyeVec = normalize(gl_Vertex.xyz - EyePos.xyz);
}
