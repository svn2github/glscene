uniform sampler2DShadow ShadowMap;
uniform sampler2D TextureMap: TEXUNIT0;
uniform sampler2D projtex;

uniform bool Softly;
uniform float SCALEE;


void main()
{
// use manual comparision (GL_TEXTURE_COMPARE_MODE = GL_NONE)
//  float depth = shadow2DProj(ShadowMap, gl_TexCoord[1]).r;
//  float R = gl_TexCoord[1].z / gl_TexCoord[1].w;
//
//  float shadow = R < depth;
// use texture comparision mode (GL_TEXTURE_COMPARE_MODE = GL_COMPARE_R_TO_TEXTURE)


float shadow = shadow2DProj(ShadowMap, gl_TexCoord[1]).r; 
vec4 proj  = texture2DProj(projtex, gl_TexCoord[1]);

if (Softly)
{
  float shadow1 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00048,0.0,0.0,0.0)*SCALEE).r; 
  float shadow2 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00048,0.0,0.0,0.0)*SCALEE).r;
  float shadow3 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00048,0.0,0.0)*SCALEE).r; 
  float shadow4 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00048,0.0,0.0)*SCALEE).r;

  float shadow5 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00096,0.0,0.0,0.0)*SCALEE).r; 
  float shadow6 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00096,0.0,0.0,0.0)*SCALEE).r;
  float shadow7 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00096,0.0,0.0)*SCALEE).r; 
  float shadow8 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00096,0.0,0.0)*SCALEE).r;

  float shadow9 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(-0.00144,0.0,0.0,0.0)*SCALEE).r; 
  float shadow10 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.00144,0.0,0.0,0.0)*SCALEE).r;
  float shadow11 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,0.00144,0.0,0.0)*SCALEE).r; 
  float shadow12 = shadow2DProj(ShadowMap, gl_TexCoord[1]+vec4(0.0,-0.00144,0.0,0.0)*SCALEE).r;

  shadow = shadow + shadow1 + shadow2 + shadow3 + shadow4;
  shadow = shadow + shadow5 + shadow6 + shadow7 + shadow8;
  shadow = shadow + shadow9 + shadow10 + shadow11 + shadow12;
  shadow = shadow * 0.08;
}
  shadow = shadow *proj;

  vec4 color = texture2D(TextureMap, gl_TexCoord[0]);

  gl_FragColor = color * mix(shadow, 1.0, 0.2);
  gl_FragColor.a = color.a;
}
