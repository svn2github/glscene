//
// This unit is part of the GLScene Project   
//
{: GLSL.ToonShader <p>

   Toon shader : Toon shading also called Cell Shading<p>

   At this time only one light source is supported

   <b>History : </b><font size=-1><ul>
    <li>13/12/15 - J.Delauney - Creation

}
unit GLSLToonShader;

interface

//{$I GLScene.inc}

uses
  System.Classes,
  // GLScene
  VKS.Scene, VKS.CrossPlatform, VKS.BaseClasses, VKS.State, VKS.OpenGLTokens, VKS.OpenGL1x, 
  VKS.Context, VKS.RenderContextInfo, VKS.VectorGeometry, VKS.Coordinates,
  VKS.TextureFormat, VKS.Color, VKS.Texture, VKS.Material, GLSL.Shader, VKS.CustomShader;

//TVKCustomGLSLToonShader
//
{: Custom class for GLSLToonShader.<p>}
type
  TVKCustomGLSLToonShader = class(TVKCustomGLSLShader)
  private
    FHighlightColor : TVKColor;
    FMidColor : TVKColor;
    FLightenShadowColor : TVKColor;
    FDarkenShadowColor : TVKColor;
    FOutlineColor : TVKColor;
    FHighlightSize : Single;
    FMidSize : Single;
    FShadowSize : Single;
    FOutlineWidth : Single;


    procedure SetHighLightColor(AValue: TVKColor);
    procedure SetMidColor(AValue: TVKColor);
    procedure SetLightenShadowColor(AValue: TVKColor);
    procedure SetDarkenShadowColor(AValue: TVKColor);
    procedure SetOutlineColor(AValue: TVKColor);

  protected
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property HighlightColor : TVKColor read FHighlightColor Write setHighlightColor;
    property MidColor : TVKColor read FMidColor Write setMidColor;
    property LightenShadowColor : TVKColor Read FLightenShadowColor Write setLightenShadowColor;
    property DarkenShadowrColor : TVKColor Read FDarkenShadowColor Write setDarkenShadowColor;
    property OutlinetColor : TVKColor Read FOutlineColor Write setOutlineColor;

    property HighlightSize : Single read FHighlightSize write FHighlightSize;
    property MidSize : Single read FMidSize write FMidSize;
    property ShadowSize : Single read FShadowSize write FShadowSize;
    property OutlineWidth : Single read FOutlineWidth write FOutlineWidth;

  end;

type
  TVKSLToonShader = class(TVKCustomGLSLToonShader)
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


{ TVKCustomGLSLToonShader }

constructor TVKCustomGLSLToonShader.Create(AOwner: TComponent);
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
  FHighLightColor := TVKColor.Create(self);
  FHighLightColor.SetColor(0.9,0.9,0.9,1.0);
  FMidColor := TVKColor.Create(self);
  FMidColor.SetColor(0.75,0.75,0.75,1.0);
  FLightenShadowColor := TVKColor.Create(self);
  FLightenShadowColor.SetColor(0.5,0.5,0.5,1.0);
  FDarkenShadowColor := TVKColor.Create(self);
  FDarkenShadowColor.SetColor(0.3,0.3,0.3,1.0);
  FOutlineColor := TVKColor.Create(self);
  FOutlineColor.SetColor(0,0,0,1.0);

  FHighlightSize := 0.95;
  FMidSize       := 0.50;
  FShadowSize    := 0.25;
  FOutlineWidth  := 0.25;

end;

destructor TVKCustomGLSLToonShader.Destroy;
begin
  FHighLightColor.Free;
  FMidColor.Free;
  FLightenShadowColor.Free;
  FDarkenShadowColor.Free;
  FOutlineColor.Free;
  inherited;
end;

procedure TVKCustomGLSLToonShader.DoApply(var rci: TRenderContextInfo;Sender: TObject);
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

function TVKCustomGLSLToonShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  GetGLSLProg.EndUseProgramObject;
  Result := False;
end;

procedure TVKCustomGLSLToonShader.SetHighlightColor(AValue: TVKColor);
begin
  FHighlightColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLToonShader.SetMidColor(AValue: TVKColor);
begin
  FMidColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLToonShader.SetLightenShadowColor(AValue: TVKColor);
begin
  FLightenShadowColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLToonShader.SetDarkenShadowColor(AValue: TVKColor);
begin
  FDarkenShadowColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLToonShader.SetOutlineColor(AValue: TVKColor);
begin
  FOutlineColor.DirectColor := AValue.Color;
end;

end.
