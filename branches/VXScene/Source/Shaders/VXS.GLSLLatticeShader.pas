//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Lattice shader that simulate Lattice. 
   At this time only one light source is supported
           
}


unit VXS.GLSLLatticeShader;

interface

{$I VXScene.inc}

uses
  System.Classes,
  
  VXS.Scene, VXS.CrossPlatform, VXS.BaseClasses, VXS.State, Winapi.OpenGL, Winapi.OpenGLext,  VXS.OpenGL1x, 
  VXS.Context, VXS.RenderContextInfo, VXS.VectorGeometry, VXS.Coordinates,
  VXS.TextureFormat, VXS.Color, VXS.Texture, VXS.Material,
  GLSL.Shader, VXS.CustomShader;

//TVXCustomGLSLSimpleLatticeShader
//
{ Custom class for GLSLSimpleLatticeShader. 
 A shader that simulate Lattice }
type
  TVXCustomGLSLSimpleLatticeShader = class(TVXCustomGLSLShader)
  private
    FLatticeScale: TVXCoordinates2;
    FLatticeThreshold: TVXCoordinates2;
    procedure SetLatticeScale(const Value: TVXCoordinates2);
    procedure SetLatticeThreshold(const Value: TVXCoordinates2);
  protected
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVXRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property LatticeScale: TVXCoordinates2 read FLatticeScale write SetLatticeScale;
    property LatticeThreshold: TVXCoordinates2 read FLatticeThreshold write SetLatticeThreshold;
  end;


//TVXCustomGLSLLatticeShader
//
{ Custom class for GLSLLatticeShader. 
 A shader that simulate Lattice with Diffuse/Specular and support Texture }
  TVXCustomGLSLLatticeShader = class(TVXCustomGLSLSimpleLatticeShader)
  private
    FAmbientColor: TVXColor;
    FDiffuseColor: TVXColor;
    FSpecularColor: TVXColor;

    FMaterialLibrary: TVXAbstractMaterialLibrary;
    FMainTexture: TVXTexture;
    FMainTexName   : TVXLibMaterialName;

    FSpecularPower: Single;
    FLightPower: Single;

    function GetMaterialLibrary: TVXAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TVXTexture);
    function GetMainTexName: TVXLibMaterialName;
    procedure SetMainTexName(const Value: TVXLibMaterialName);

    procedure SetDiffuseColor(AValue: TVXColor);
    procedure SetAmbientColor(AValue: TVXColor);
    procedure SetSpecularColor(AValue: TVXColor);

  protected
    procedure DoInitialize(var rci : TVXRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci : TVXRenderContextInfo; Sender : TObject); override;

    procedure SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property DiffuseColor : TVXColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TVXColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TVXColor Read FAmbientColor Write setAmbientColor;

    property MaterialLibrary: TVXAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TVXTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TVXLibMaterialName read GetMainTexName write SetMainTexName;

    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property LightPower: Single read FLightPower write FLightPower;

  end;

  TVXSLSimpleLatticeShader = class(TVXCustomGLSLSimpleLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;
  end;

  TVXSLLatticeShader = class(TVXCustomGLSLLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;

    property AmbientColor;
    property DiffuseColor;
    property SpecularColor;

    property MainTexture;

    property SpecularPower;
    property LightPower;
  end;

implementation

{ TVXCustomGLSLSimpleLatticeShader }

constructor TVXCustomGLSLSimpleLatticeShader.Create(AOwner: TComponent);
begin
  inherited;
  with FragmentProgram.Code do
  begin
    Clear;
    Add('  uniform vec2  Scale; ');
    Add('  uniform vec2  Threshold; ');
    Add(' ');
    Add('  void main (void) ');
    Add('{ ');
    Add('    float ss = fract(gl_TexCoord[0].s * Scale.s); ');
    Add('    float tt = fract(gl_TexCoord[0].t * Scale.t); ');
    Add(' ');
    Add('    if ((ss > Threshold.s) && (tt > Threshold.t)) discard; ');
    Add('    gl_FragColor = gl_Color;');
    Add('} ');
  end;

  // Initial stuff.
  FLatticeScale := TVXCoordinates2.Create(Self);
  FLatticeThreshold := TVXCoordinates2.Create(Self);

  FLatticeScale.SetPoint2D(10, 40);
  FLatticeThreshold.SetPoint2D(0.15, 0.3);
end;

destructor TVXCustomGLSLSimpleLatticeShader.Destroy;
begin
  FLatticeScale.Destroy;
  FLatticeThreshold.Destroy;
  inherited;
end;

procedure TVXCustomGLSLSimpleLatticeShader.DoApply(var rci: TVXRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['Scale'].AsVector2f := FLatticeScale.AsPoint2D;
  Param['Threshold'].AsVector2f := FLatticeThreshold.AsPoint2D;
end;

function TVXCustomGLSLSimpleLatticeShader.DoUnApply(var rci: TVXRenderContextInfo): Boolean;
begin
  Result := False;
  //gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
end;

procedure TVXCustomGLSLSimpleLatticeShader.SetLatticeScale(
  const Value: TVXCoordinates2);
begin
  FLatticeScale.Assign(Value);
end;

procedure TVXCustomGLSLSimpleLatticeShader.SetLatticeThreshold(
  const Value: TVXCoordinates2);
begin
  FLatticeThreshold.Assign(Value);
end;

{ TVXCustomGLSLLatticeShader }

constructor TVXCustomGLSLLatticeShader.Create(
  AOwner: TComponent);
begin
  inherited;
  FAmbientColor := TVXColor.Create(Self);
  FDiffuseColor := TVXColor.Create(Self);
  FSpecularColor := TVXColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);

  FSpecularPower  := 8;  //6
  FLightPower     := 1;
end;

destructor TVXCustomGLSLLatticeShader.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TVXCustomGLSLLatticeShader.DoApply(var rci: TVXRenderContextInfo; Sender: TObject);
begin

  inherited;

  Param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  Param['SpecularColor'].AsVector4f := FSpecularColor.Color;

  Param['SpecPower'].AsVector1f := FSpecularPower;
  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

procedure TVXCustomGLSLLatticeShader.DoInitialize(var rci : TVXRenderContextInfo; Sender : TObject);
begin
  with VertexProgram.Code do
  begin
    Clear;
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');
    Add('varying vec2 Texcoord; ');
    Add(' ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('  gl_Position = ftransform(); ');
    Add('  Texcoord = gl_MultiTexCoord0.xy; ');
    Add('  Normal = normalize(gl_NormalMatrix * gl_Normal); ');
    Add('  vec3 p = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add('  LightVector = normalize(gl_LightSource[0].position.xyz - p); ');
    Add('  CameraVector = normalize(p); ');
    Add('} ');
  end;

  with FragmentProgram.Code do
  begin
    Clear;
    Add('  uniform vec2  Scale; ');
    Add('  uniform vec2  Threshold; ');
    Add(' ');
    Add('uniform vec4 AmbientColor; ');
    Add('uniform vec4 DiffuseColor; ');
    Add('uniform vec4 SpecularColor; ');
    Add(' ');
    Add('uniform float LightIntensity; ');
    Add('uniform float SpecPower; ');
    Add('uniform sampler2D MainTexture; ');
    Add(' ');
    Add('varying vec3 Normal; ');
    Add('varying vec3 LightVector; ');
    Add('varying vec3 CameraVector; ');
    Add('varying vec2 Texcoord; ');
    Add(' ');
    Add('void main(void) ');
    Add('{ ');
    Add('    float ss = fract(Texcoord[0] * Scale.s); ');
    Add('    float tt = fract(Texcoord[1] * Scale.t); ');
    Add(' ');
    Add('    if ((ss > Threshold.s) && (tt > Threshold.t)) discard; ');
    Add(' ');
    Add('  vec4 TextureContrib = texture2D(MainTexture, Texcoord); ');
    Add('  vec4 DiffuseContrib = clamp(DiffuseColor * dot(LightVector, Normal), 0.0, 1.0); ');
    Add(' ');
    Add('  vec3 reflect_vec = reflect(CameraVector, -Normal); ');
    Add('  float Temp = dot(reflect_vec, LightVector); ');
    Add('  vec4 SpecContrib = SpecularColor * clamp(pow(Temp, SpecPower), 0.0, 0.95); ');
    Add(' ');
    Add('  gl_FragColor = TextureContrib * LightIntensity * (AmbientColor + DiffuseContrib) + LightIntensity * SpecContrib; ');
    Add('} ');
  end;
  inherited;
end;


function TVXCustomGLSLLatticeShader.GetMaterialLibrary: TVXAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVXCustomGLSLLatticeShader.SetMaterialLibrary(const Value: TVXAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TVXAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TVXCustomGLSLLatticeShader.SetMainTexTexture(const Value: TVXTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TVXCustomGLSLLatticeShader.GetMainTexName: TVXLibMaterialName;
begin
  Result := TVXMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TVXCustomGLSLLatticeShader.SetMainTexName(const Value: TVXLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TVXMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TVXCustomGLSLLatticeShader.SetDiffuseColor(AValue: TVXColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLLatticeShader.SetAmbientColor(AValue: TVXColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLLatticeShader.SetSpecularColor(AValue: TVXColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TVXCustomGLSLLatticeShader.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin

        if FMainTexture <> nil then
        begin
          Index := TVXMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

end.

