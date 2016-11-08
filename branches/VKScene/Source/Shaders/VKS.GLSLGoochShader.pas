//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Gooch shader : Gooch shading is used to substitute photorealistic
   rendering by rendering that focuses on structore and shape of the object.
   Instead of usage of light and shadow, Gooch shading uses concept of warm and cool colors.
   Standard Blinn-Phong shading only modulates base color of the object.
   In Gooch shading intensity of diffuse lighting is used to determine how to blend warm and cold colors together. 

   At this time only one light source is supported
     
}

unit VKS.GLSLGoochShader;

interface

//{$I VKScene.inc}

uses
  System.Classes,
  //VKS
  VKS.Scene, VKS.CrossPlatform, VKS.BaseClasses, VKS.State, Winapi.OpenGL, Winapi.OpenGLext,  VKS.OpenGL1x, 
  VKS.Context, VKS.RenderContextInfo, VKS.VectorGeometry, VKS.Coordinates,
  VKS.TextureFormat, VKS.Color, VKS.Texture, VKS.Material, GLSL.Shader, VKS.CustomShader;

//TVKCustomGLSLSimpleGoochShader
//
{ Custom class for GLSLSimpleGoochShader. }
type
  TVKCustomGLSLSimpleGoochShader = class(TVKCustomGLSLShader)
  private
    FDiffuseColor : TVKColor;
    FWarmColor : TVKColor;
    FCoolColor : TVKColor;
    FSpecularColor : TVKColor;
    FAmbientColor : TVKColor;
    FDiffuseWarm : Single;
    FDiffuseCool : Single;
    FAmbientFactor : Single;
    FDiffuseFactor : Single;
    FSpecularFactor : Single;

    FBlendingMode: TVKBlendingModeEx;

    procedure SetDiffuseColor(AValue: TVKColor);
    procedure SetAmbientColor(AValue: TVKColor);
    procedure SetSpecularColor(AValue: TVKColor);
    procedure SetWarmColor(AValue: TVKColor);
    procedure SetCoolColor(AValue: TVKColor);

  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property DiffuseColor : TVKColor read FDiffuseColor Write setDiffuseColor;
    property WarmColor : TVKColor read FWarmColor Write setWarmColor;
    property CoolColor : TVKColor Read FCoolColor Write setCoolColor;
    property SpecularColor : TVKColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TVKColor Read FAmbientColor Write setAmbientColor;
    property WarmFactor : Single Read FDiffuseWarm Write FDiffuseWarm;
    property CoolFactor : Single Read FDiffuseCool Write FDiffuseCool;
    property AmbientFactor : Single Read FAmbientFactor Write FAmbientFactor;
    property DiffuseFactor : Single Read FDiffuseFactor Write FDiffuseFactor;
    property SpecularFactor : Single Read FSpecularFactor Write FSpecularFactor;

    property BlendingMode: TVKBlendingModeEx read FBlendingMode write FBlendingMode default bmxOpaque;
  end;

type
  TVKSLSimpleGoochShader = class(TVKCustomGLSLSimpleGoochShader)
  published
    property DiffuseColor;
    property WarmColor;
    property CoolColor;
    property SpecularColor;
    property AmbientColor;
    property WarmFactor;
    property CoolFactor;
    property AmbientFactor;
    property DiffuseFactor;
    property SpecularFactor;
  end;

implementation


{ TVKCustomGLSLSimpleGoochShader }

constructor TVKCustomGLSLSimpleGoochShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;

    Add('varying vec3 vNormal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 ReflectVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec3 normal = gl_NormalMatrix * gl_Normal; ');
    Add('  vNormal  = normalize(normal); ');

    Add('  lightVec = vec3(lightPos - vert); ');
    Add('  ReflectVec    = normalize(reflect(-lightVec, vNormal)); ');
    Add('  viewVec = -vec3(vert); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('uniform vec4  SurfaceColor; ');
    Add('uniform vec4  WarmColor; ');
    Add('uniform vec4  CoolColor; ');
    Add('uniform vec4  SpecularColor; ');
    Add('uniform vec4  AmbientColor; ');
    Add('uniform float DiffuseWarm; ');
    Add('uniform float DiffuseCool; ');
    Add('uniform float AmbientFactor; ');
    Add('uniform float DiffuseFactor; ');
    Add('uniform float SpecularFactor; ');

    Add('varying vec3 vNormal; ');
    Add('varying vec3 lightVec; ');
    Add('varying vec3 viewVec; ');
    Add('varying vec3 ReflectVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');


    Add('vec3 L = normalize(lightVec); ');
    Add('vec3 V = normalize(viewVec); ');
    Add('vec3 halfAngle = normalize(L + V); ');
    Add('float NdotL   = (dot(L, vNormal) + 1.0) * 0.5; ');
    Add('float NdotH = clamp(dot(halfAngle, vNormal), 0.0, 1.0); ');
    Add('// "Half-Lambert" technique for more pleasing diffuse term ');
    Add('float diffuse = 0.5 * NdotL + 0.5; ');
    Add('vec3 nreflect = normalize(ReflectVec); ');
    Add('float specular    = max(dot(nreflect, V), 0.0); ');
    Add('specular          = pow(specular, 64.0); ');

    Add('vec4 kCool    = min(CoolColor + DiffuseCool * SurfaceColor, 1.0); ');
    Add('vec4 kWarm    = min(WarmColor + DiffuseWarm * SurfaceColor, 1.0); ');


    Add('vec4 Cgooch = mix(kWarm, kCool, diffuse); ');

    Add('vec3 result = AmbientFactor * AmbientColor.rgb + DiffuseFactor * Cgooch.rgb + SpecularColor.rgb * SpecularFactor *specular; ');

    Add('gl_FragColor = vec4(result,SurfaceColor.a); ');
    Add('} ');
  end;


  // Initial stuff.
  FDiffuseColor := TVKColor.Create(self);
  FDiffuseColor.SetColor(0.75,0.75,0.75,1.0);
  FWarmColor := TVKColor.Create(self);
  FWarmColor.SetColor(0.88,0.81,0.49,1.0);
  FCoolColor := TVKColor.Create(self);
  FCoolColor.SetColor(0.58,0.10,0.76,1.0);
  FAmbientColor := TVKColor.Create(self);
  FAmbientColor.SetColor(0.3,0.3,0.3,1.0);
  FSpecularColor := TVKColor.Create(self);
  FSpecularColor.SetColor(1.0,1.0,1.0,1.0);

  FDiffuseWarm    := 0.55;
  FDiffuseCool    := 0.30;
  FAmbientFactor  := 1.0;
  FDiffuseFactor  :=0.8;
  FSpecularFactor :=0.9;

  FBlendingMode:=bmxOpaque;
end;

destructor TVKCustomGLSLSimpleGoochShader.Destroy;
begin
  FDiffuseColor.Free;
  FWarmColor.Free;
  FCoolColor.Free;
  FSpecularColor.Free;
  FAmbientColor.Free;
  inherited;
end;

procedure TVKCustomGLSLSimpleGoochShader.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin


  GetGLSLProg.UseProgramObject;
  param['SurfaceColor'].AsVector4f := FDiffuseColor.Color;
  param['WarmColor'].AsVector4f := FWarmColor.Color;
  param['CoolColor'].AsVector4f := FCoolColor.Color;
  param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  param['SpecularColor'].AsVector4f := FSpecularColor.Color;
  param['DiffuseWarm'].AsVector1f := FDiffuseWarm;
  param['DiffuseCool'].AsVector1f := FDiffuseCool;
  param['AmbientFactor'].AsVector1f := FAmbientFactor;
  param['DiffuseFactor'].AsVector1f := FDiffuseFactor;
  param['SpecularFactor'].AsVector1f := FSpecularFactor;

 // glPushAttrib(GL_COLOR_BUFFER_BIT);
  ApplyBlendingModeEx(FBlendingMode);
//  glEnable(GL_BLEND);
//  gl.BlendFunc(cGLBlendFunctionToGLEnum[FBlendSrc],cGLBlendFunctionToGLEnum[FBlendDst]);
end;

function TVKCustomGLSLSimpleGoochShader.DoUnApply(var rci: TVKRenderContextInfo): Boolean;
begin

  gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
  UnApplyBlendingModeEx;
 // glPopAttrib;
  Result := False;
end;

procedure TVKCustomGLSLSimpleGoochShader.SetDiffuseColor(AValue: TVKColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSimpleGoochShader.SetAmbientColor(AValue: TVKColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSimpleGoochShader.SetSpecularColor(AValue: TVKColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSimpleGoochShader.SetWarmColor(AValue: TVKColor);
begin
  FWarmColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLSimpleGoochShader.SetCoolColor(AValue: TVKColor);
begin
  FCoolColor.DirectColor := AValue.Color;
end;

end.
