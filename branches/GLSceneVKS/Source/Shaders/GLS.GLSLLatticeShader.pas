//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   Lattice shader that simulate Lattice. 
   At this time only one light source is supported
           
}


unit GLS.GLSLLatticeShader;

interface

{$I GLScene.inc}

uses
  System.Classes,
  //GLS
  GLS.Scene, GLS.CrossPlatform, GLS.BaseClasses, GLS.State, GLS.OpenGLTokens, GLS.OpenGL1x, 
  GLS.Context, GLS.RenderContextInfo, GLS.VectorGeometry, GLS.Coordinates,
  GLS.TextureFormat, GLS.Color, GLS.Texture, GLS.Material,
  GLSL.Shader, GLS.CustomShader;

//TVKCustomGLSLSimpleLatticeShader
//
{ Custom class for GLSLSimpleLatticeShader. 
 A shader that simulate Lattice }
type
  TVKCustomGLSLSimpleLatticeShader = class(TVKCustomGLSLShader)
  private
    FLatticeScale: TVKCoordinates2;
    FLatticeThreshold: TVKCoordinates2;
    procedure SetLatticeScale(const Value: TVKCoordinates2);
    procedure SetLatticeThreshold(const Value: TVKCoordinates2);
  protected
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TRenderContextInfo): Boolean; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property LatticeScale: TVKCoordinates2 read FLatticeScale write SetLatticeScale;
    property LatticeThreshold: TVKCoordinates2 read FLatticeThreshold write SetLatticeThreshold;
  end;


//TVKCustomGLSLLatticeShader
//
{ Custom class for GLSLLatticeShader. 
 A shader that simulate Lattice with Diffuse/Specular and support Texture }
  TVKCustomGLSLLatticeShader = class(TVKCustomGLSLSimpleLatticeShader)
  private
    FAmbientColor: TVKColor;
    FDiffuseColor: TVKColor;
    FSpecularColor: TVKColor;

    FMaterialLibrary: TVKAbstractMaterialLibrary;
    FMainTexture: TVKTexture;
    FMainTexName   : TVKLibMaterialName;

    FSpecularPower: Single;
    FLightPower: Single;

    function GetMaterialLibrary: TVKAbstractMaterialLibrary;

    procedure SetMainTexTexture(const Value: TVKTexture);
    function GetMainTexName: TVKLibMaterialName;
    procedure SetMainTexName(const Value: TVKLibMaterialName);

    procedure SetDiffuseColor(AValue: TVKColor);
    procedure SetAmbientColor(AValue: TVKColor);
    procedure SetSpecularColor(AValue: TVKColor);

  protected
    procedure DoInitialize(var rci : TRenderContextInfo; Sender : TObject); override;
    procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;

    procedure SetMaterialLibrary(const Value: TVKAbstractMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property DiffuseColor : TVKColor read FDiffuseColor Write setDiffuseColor;
    property SpecularColor : TVKColor Read FSpecularColor Write setSpecularColor;
    property AmbientColor : TVKColor Read FAmbientColor Write setAmbientColor;

    property MaterialLibrary: TVKAbstractMaterialLibrary read getMaterialLibrary write SetMaterialLibrary;
    property MainTexture: TVKTexture read FMainTexture write SetMainTexTexture;
    property MainTextureName: TVKLibMaterialName read GetMainTexName write SetMainTexName;

    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property LightPower: Single read FLightPower write FLightPower;

  end;

  TVKSLSimpleLatticeShader = class(TVKCustomGLSLSimpleLatticeShader)
  published
    property LatticeScale;
    property LatticeThreshold;
  end;

  TVKSLLatticeShader = class(TVKCustomGLSLLatticeShader)
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

{ TVKCustomGLSLSimpleLatticeShader }

constructor TVKCustomGLSLSimpleLatticeShader.Create(AOwner: TComponent);
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
  FLatticeScale := TVKCoordinates2.Create(Self);
  FLatticeThreshold := TVKCoordinates2.Create(Self);

  FLatticeScale.SetPoint2D(10, 40);
  FLatticeThreshold.SetPoint2D(0.15, 0.3);
end;

destructor TVKCustomGLSLSimpleLatticeShader.Destroy;
begin
  FLatticeScale.Destroy;
  FLatticeThreshold.Destroy;
  inherited;
end;

procedure TVKCustomGLSLSimpleLatticeShader.DoApply(var rci: TRenderContextInfo;Sender: TObject);
begin
  GetGLSLProg.UseProgramObject;
  Param['Scale'].AsVector2f := FLatticeScale.AsPoint2D;
  Param['Threshold'].AsVector2f := FLatticeThreshold.AsPoint2D;
end;

function TVKCustomGLSLSimpleLatticeShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result := False;
  //gl.ActiveTexture(GL_TEXTURE0_ARB);
  GetGLSLProg.EndUseProgramObject;
end;

procedure TVKCustomGLSLSimpleLatticeShader.SetLatticeScale(
  const Value: TVKCoordinates2);
begin
  FLatticeScale.Assign(Value);
end;

procedure TVKCustomGLSLSimpleLatticeShader.SetLatticeThreshold(
  const Value: TVKCoordinates2);
begin
  FLatticeThreshold.Assign(Value);
end;

{ TVKCustomGLSLLatticeShader }

constructor TVKCustomGLSLLatticeShader.Create(
  AOwner: TComponent);
begin
  inherited;
  FAmbientColor := TVKColor.Create(Self);
  FDiffuseColor := TVKColor.Create(Self);
  FSpecularColor := TVKColor.Create(Self);

  //setup initial parameters
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);

  FSpecularPower  := 8;  //6
  FLightPower     := 1;
end;

destructor TVKCustomGLSLLatticeShader.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TVKCustomGLSLLatticeShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
begin

  inherited;

  Param['AmbientColor'].AsVector4f := FAmbientColor.Color;
  Param['DiffuseColor'].AsVector4f := FDiffuseColor.Color;
  Param['SpecularColor'].AsVector4f := FSpecularColor.Color;

  Param['SpecPower'].AsVector1f := FSpecularPower;
  Param['LightIntensity'].AsVector1f := FLightPower;

  Param['MainTexture'].AsTexture2D[0] := FMainTexture;

end;

procedure TVKCustomGLSLLatticeShader.DoInitialize(var rci : TRenderContextInfo; Sender : TObject);
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


function TVKCustomGLSLLatticeShader.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TVKCustomGLSLLatticeShader.SetMaterialLibrary(const Value: TVKAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> nil then FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;
  if (FMaterialLibrary <> nil)
    and (FMaterialLibrary is TVKAbstractMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
end;

procedure TVKCustomGLSLLatticeShader.SetMainTexTexture(const Value: TVKTexture);
begin
  if FMainTexture = Value then Exit;
  FMainTexture := Value;
  NotifyChange(Self)
end;

function TVKCustomGLSLLatticeShader.GetMainTexName: TVKLibMaterialName;
begin
  Result := TVKMaterialLibrary(FMaterialLibrary).GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTexName;
end;

procedure TVKCustomGLSLLatticeShader.SetMainTexName(const Value: TVKLibMaterialName);
begin
 // Assert(not(assigned(FMaterialLibrary)),'You must set Material Library Before');
  if FMainTexName = Value then Exit;
  FMainTexName  := Value;

  FMainTexture := TVKMaterialLibrary(FMaterialLibrary).TextureByName(FMainTexName);
  NotifyChange(Self);
end;


procedure TVKCustomGLSLLatticeShader.SetDiffuseColor(AValue: TVKColor);
begin
  FDiffuseColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLLatticeShader.SetAmbientColor(AValue: TVKColor);
begin
  FAmbientColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLLatticeShader.SetSpecularColor(AValue: TVKColor);
begin
  FSpecularColor.DirectColor := AValue.Color;
end;

procedure TVKCustomGLSLLatticeShader.Notification(AComponent: TComponent; Operation: TOperation);
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
          Index := TVKMaterialLibrary(FMaterialLibrary).Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            SetMainTexTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

end.

