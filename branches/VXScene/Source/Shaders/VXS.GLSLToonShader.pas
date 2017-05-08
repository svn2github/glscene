//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Toon shader : Toon shading also called Cell Shading 
   At this time only one light source is supported

 }
unit VXS.GLSLToonShader;

interface

//{$I VXScene.inc}

uses
  System.Classes,
  //VXScene
  VXS.Scene, VXS.CrossPlatform, VXS.BaseClasses, VXS.State, Winapi.OpenGL, Winapi.OpenGLext,  VXS.OpenGL1x, 
  VXS.Context, VXS.RenderContextInfo, VXS.VectorGeometry, VXS.Coordinates,
  VXS.TextureFormat, VXS.Color, VXS.Texture, VXS.Material, GLSL.Shader, VXS.CustomShader;

//TVXCustomGLSLToonShader
//
{ Custom class for GLSLToonShader. }
type
  TVXCustomGLSLToonShader = class(TVXCustomGLSLShader)
  private
    FHighlightColor : TVXColor;
    FMidColor : TVXColor;
    FLightenShadowColor : TVXColor;
    FDarkenShadowColor : TVXColor;
    FOutlineColor : TVXColor;
    FHighlightSize : Single;
    FMidSize : Single;
    FShadowSize : Single;
    FOutlineWidth : Single;


    procedure SetHighLightColor(AValue: TVXColor);
    procedure SetMidColor(AValue: TVXColor);
    procedure SetLightenShadowColor(AValue: TVXColor);
    procedure SetDarkenShadowColor(AValue: TVXColor);
    procedure SetOutlineColor(AValue: TVXColor);

  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property HighlightColor : TVXColor read FHighlightColor Write setHighlightColor;
    property MidColor : TVXColor read FMidColor Write setMidColor;
    property LightenShadowColor : TVXColor Read FLightenShadowColor Write setLightenShadowColor;
    property DarkenShadowrColor : TVXColor Read FDarkenShadowColor Write setDarkenShadowColor;
    property OutlinetColor : TVXColor Read FOutlineColor Write setOutlineColor;

    property HighlightSize : Single read FHighlightSize write FHighlightSize;
    property MidSize : Single read FMidSize write FMidSize;
    property ShadowSize : Single read FShadowSize write FShadowSize;
    property OutlineWidth : Single read FOutlineWidth write FOutlineWidth;

  end;

type
  TVXSLToonShader = class(TVXCustomGLSLToonShader)
  published
    property HighlightColor;
    property MidColor;
    property LightenShadowColor;
    property DarkenShadowrColor;
    property OutlinetColor;

    property HighlightSize;
    property MidSize;
    property ShadowSize;
    property OutlineWidth;
  end;

implementation


{ TVXCustomGLSLToonShader }

constructor TVXCustomGLSLToonShader.Create(AOwner: TComponent);
begin
  inherited;

  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 vNormal; ');
    Add('varying vec3 LightVec; ');
    Add('varying vec3 ViewVec; ');
    Add(' ');
    Add('void main() ');
    Add('{ ');
    Add('  vec4 lightPos = gl_LightSource[0].position;');
    Add('  vec4 vert =  gl_ModelViewMatrix * gl_Vertex; ');
    Add('  vec3 normal = gl_NormalMatrix * gl_Normal; ');
    Add('  vNormal  = normalize(normal); ');
    Add('  LightVec = vec3(lightPos - vert); ');
    Add('  ViewVec = -vec3(vert); ');
    //Add('  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; ');
    Add('  gl_Position = ftransform(); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('uniform vec4 HighlightColor; ');
    Add('uniform vec4 MidColor; ');
    Add('uniform vec4 LightenShadowColor; ');
    Add('uniform vec4 DarkenShadowColor; ');
    Add('uniform vec4 OutlineColor; ');

    Add('uniform float HighlightSize; '); // 0.95
    Add('uniform float MidSize; ');       // 0.5
    Add('uniform float ShadowSize; ');    // 0.25

    Add('uniform float OutlineWidth; ');

    Add('varying vec3 vNormal; ');
    Add('varying vec3 LightVec; ');
    Add('varying vec3 ViewVec; ');

    Add('void main() ');
    Add('{ ');
    Add('  vec3 n = normalize(vNormal); ');
    Add('  vec3 l = normalize(LightVec); ');
    Add('  vec3 v = normalize(ViewVec); ');

    Add('    float lambert = dot(l,n); ');
    Add('    vec4 colour = MidColor; ');
    Add('    if (lambert>HighlightSize) colour = HighlightColor; ');
    Add('    else if (lambert>MidSize) colour = MidColor; ');
    Add('    else if (lambert>ShadowSize) colour = LightenShadowColor; ');
    Add('    else if (lambert<ShadowSize) colour = DarkenShadowColor; ');

    Add('    if (dot(n,v)<OutlineWidth) colour = OutlineColor; ');

    Add('    gl_FragColor = colour; ');
    Add('} ');
  end;


  // Initial stuff.
  FHighLightColor := TVXColor.Create(self);
  FHighLightColor.SetColor(0.9,0.9,0.9,1.0);
  FMidColor := TVXColor.Create(self);
  FMidColor.SetColor(0.75,0.75,0.75,1.0);
  FLightenShadowColor := TVXColor.Create(self);
  FLightenShadowColor.SetColor(0.5,0.5,0.5,1.0);
  FDarkenShadowColor := TVXColor.Create(self);
  FDarkenShadowColor.SetColor(0.3,0.3,0.3,1.0);
  FOutlineColor := TVXColor.Create(self);
  FOutlineColor.SetColor(0,0,0,1.0);

  FHighlightSize := 0.95;
  FMidSize       := 0.50;
  FShadowSize    := 0.25;
  FOutlineWidth  := 0.25;

end;

destructor TVXCustomGLSLToonShader.Destroy;
begin
  FHighLightColor.Free;
  FMidColor.Free;
  FLightenShadowColor.Free;
  FDarkenShadowColor.Free;
  FOutlineColor.Free;
  inherited;
end;

procedure TVXCustomGLSLToonShader.DoApply(var rci: TVXRenderContextInfo;Sender: TObject);
begin

  GetGLSLProg.UseProgramObject;
  param['HighlightColor'].AsVector4f := FHighlightColor.Color;
  param['MidColor'].AsVector4f := FMidColor.Color;
  param['LightenShadowColor'].AsVector4f := FLightenShadowColor.Color;
  param['DarkenShadowColor'].AsVector4f := FDarkenShadowColor.Color;
  param['OutlineColor'].AsVector4f := FOutlineColor.Color;

  param['HighlightSize'].AsVector1f := FHighlightSize;
  param['MidSize'].AsVector1f := FMidSize;
  param['ShadowSize'].AsVector1f := FShadowSize;
  param['OutlineWidth'].AsVector1f := FOutlineWidth;

end;

function TVXCustomGLSLToonShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TVXCustomGLSLToonShader.SetHighlightColor(AValue: TVXColor);
begin
  FHighlightColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLToonShader.SetMidColor(AValue: TVXColor);
begin
  FMidColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLToonShader.SetLightenShadowColor(AValue: TVXColor);
begin
  FLightenShadowColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLToonShader.SetDarkenShadowColor(AValue: TVXColor);
begin
  FDarkenShadowColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLToonShader.SetOutlineColor(AValue: TVXColor);
begin
  FOutlineColor.DirectColor := AValue.Color;
end;

end.
