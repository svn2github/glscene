#ifndef __GLSL_CG_DATA_TYPES
# define half float
# define half3x3 mat3
# define half3 vec3
# define half4 vec4
#endif

uniform sampler2D NormalMap;
uniform samplerCube EnvironmentMap;

varying half3 EyeVec;
varying half3x3 ObjToTangentSpace;

const half  cFresnelBias = 0.1;
const half4 cDeepColor = vec4(0, 0.1, 0.2, 1);
const half4 cShallowColor = vec4(0, 0.3, 0.4, 1);

const half3 cNormalCorrection = vec3(-1, -1, 0);

void main()
{
	// sum normal maps
    half3 t0 = texture2D(NormalMap, gl_TexCoord[0].xy).rgb;
    half3 t1 = texture2D(NormalMap, gl_TexCoord[1].xy).rgb;
    half3 normal = t0 + t1 + cNormalCorrection;

    half3 nW = normalize(ObjToTangentSpace * normal);

    half3 r = reflect(EyeVec, nW);
    half4 rColor = textureCube(EnvironmentMap, r);
    rColor.a = 1.0+5.0*rColor.a;

    half facing = 1.0-max(dot(normalize(-EyeVec), nW), 0.0);
    half4 waterColor = mix(cDeepColor, cShallowColor, facing);

    half fresnel = cFresnelBias + (1-cFresnelBias)*pow(facing, 4.0);

    gl_FragColor = vec4(waterColor.rgb + (fresnel*rColor.a)*rColor.rgb, 1.0); 
}
