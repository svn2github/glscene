//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   A GLSL shader that applies bump mapping. 


    Notes:
     1) Alpha is a synthetic property, in real life your should set each
      color's Alpha individualy
     2) TVKSLMLBumpShader takes all Light parameters directly
      from OpenGL (that includes TVKLightSource's)


    TODO:
      1) Implement IGLShaderDescription in all shaders.
}
unit GLS.GLSLBumpShader;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils,
  //GLS
  GLS.Texture, GLS.Scene, GLS.VectorGeometry, GLS.VectorTypes,
  GLS.Cadencer, GLS.Strings, GLS.OpenGLTokens, GLS.CustomShader, GLS.Color,
  GLS.RenderContextInfo, GLS.Material, GLS.GLSLShader;

type
  EGLSLBumpShaderException = class(EGLSLShaderException);

  // An abstract class.
  TVKBaseCustomGLSLBumpShader = class(TVKCustomGLSLShader, IGLMaterialLibrarySupported)
  private
    FBumpHeight: Single;
    FBumpSmoothness: Integer;

    FSpecularPower: Single;
    FSpecularSpread: Single;
    FLightPower: Single;

    FMaterialLibrary: TVKMaterialLibrary;

    FNormalTexture: TVKTexture;
    FSpecularTexture: TVKTexture;
    FNormalTextureName: TVKLibMaterialName;
    FSpecularTextureName: TVKLibMaterialName;

    function GetNormalTextureName: TVKLibMaterialName;
    function GetSpecularTextureName: TVKLibMaterialName;
    procedure SetNormalTextureName(const Value: TVKLibMaterialName);
    procedure SetSpecularTextureName(const Value: TVKLibMaterialName);
    procedure SetSpecularTexture(const Value: TVKTexture);
    procedure SetNormalTexture(const Value: TVKTexture);

    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TVKAbstractMaterialLibrary;
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    function DoUnApply(var rci: TVKRenderContextInfo): Boolean; override;
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;

    property BumpHeight: Single read FBumpHeight write FBumpHeight;
    property BumpSmoothness: Integer read FBumpSmoothness write FBumpSmoothness;

    property SpecularPower: Single read FSpecularPower write FSpecularPower;
    property SpecularSpread: Single read FSpecularSpread write FSpecularSpread;
    property LightPower: Single read FLightPower write FLightPower;

    property NormalTexture: TVKTexture read FNormalTexture write SetNormalTexture;
    property SpecularTexture: TVKTexture read FSpecularTexture write SetSpecularTexture;

    property NormalTextureName: TVKLibMaterialName read GetNormalTextureName write SetNormalTextureName;
    property SpecularTextureName: TVKLibMaterialName read GetSpecularTextureName write SetSpecularTextureName;

    property MaterialLibrary: TVKMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
  end;

  // An abstract class.
  TVKBaseCustomGLSLBumpShaderMT = class(TVKBaseCustomGLSLBumpShader)
  private
    FMainTexture: TVKTexture;
    FMainTextureName: TVKLibMaterialName;
    function GetMainTextureName: string;
    procedure SetMainTextureName(const Value: string);
  protected
    procedure SetMaterialLibrary(const Value: TVKMaterialLibrary); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property MainTexture: TVKTexture read FMainTexture write FMainTexture;
    property MainTextureName: TVKLibMaterialName read GetMainTextureName write SetMainTextureName;
  end;

  // One Light shaders.
  TVKCustomGLSLBumpShaderAM = class(TVKBaseCustomGLSLBumpShaderMT)
  private
    FAmbientColor: TVKColor;
    FDiffuseColor: TVKColor;
    FSpecularColor: TVKColor;

    function GetAlpha: Single;
    procedure SetAlpha(const Value: Single);
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    procedure DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property AmbientColor: TVKColor read FAmbientColor;
    property DiffuseColor: TVKColor read FDiffuseColor;
    property SpecularColor: TVKColor read FSpecularColor;

    property Alpha: Single read GetAlpha write SetAlpha;
  end;

  TVKCustomGLSLBumpShaderMT = class(TVKBaseCustomGLSLBumpShaderMT)
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    procedure DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject); override;
  end;

  TVKCustomGLSLBumpShader = class(TVKBaseCustomGLSLBumpShader, IGLShaderDescription)
  private
    // Implementing IGLShaderDescription.
    procedure SetShaderTextures(const Textures: array of TVKTexture);
    procedure GetShaderTextures(var Textures: array of TVKTexture);

    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);

    procedure SetShaderMiscParameters(const ACadencer: TVKCadencer; const AMatLib: TVKMaterialLibrary; const ALightSources: TVKLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TVKCadencer; var AMatLib: TVKMaterialLibrary; var ALightSources: TVKLightSourceSet);

    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);

    function GetShaderDescription: string;
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    procedure DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject); override;
  end;

  // MultiLight shaders.
  TVKCustomGLSLMLBumpShader = class(TVKBaseCustomGLSLBumpShader, IGLShaderDescription)
  private
    FLightSources: TVKLightSourceSet;
    FLightCompensation: Single;
    procedure SetLightSources(const Value: TVKLightSourceSet);
    procedure SetLightCompensation(const Value: Single);

    // Implementing IGLShaderDescription.
    procedure SetShaderTextures(const Textures: array of TVKTexture);
    procedure GetShaderTextures(var Textures: array of TVKTexture);

    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);

    procedure SetShaderMiscParameters(const ACadencer: TVKCadencer; const AMatLib: TVKMaterialLibrary; const ALightSources: TVKLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TVKCadencer; var AMatLib: TVKMaterialLibrary; var ALightSources: TVKLightSourceSet);

    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);

    function GetShaderDescription: string;
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    procedure DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
    property LightSources: TVKLightSourceSet read FLightSources write SetLightSources default [1];
    { Setting LightCompensation to a value less than 1 decreeses individual
       light intensity when using multiple lights }
    property LightCompensation: Single read FLightCompensation write SetLightCompensation;
  end;

  TVKCustomGLSLMLBumpShaderMT = class(TVKBaseCustomGLSLBumpShaderMT)
  private
    FLightSources: TVKLightSourceSet;
    FLightCompensation: Single;
    procedure SetLightSources(const Value: TVKLightSourceSet);
    procedure SetLightCompensation(const Value: Single);
  protected
    procedure DoApply(var rci : TVKRenderContextInfo; Sender : TObject); override;
    procedure DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject); override;
  public
    constructor Create(AOwner : TComponent); override;
    property LightSources: TVKLightSourceSet read FLightSources write SetLightSources default [1];
    { Setting LightCompensation to a value less than 1 decreeses individual
       light intensity when using multiple lights }
    property LightCompensation: Single read FLightCompensation write SetLightCompensation;
  end;

                {************** Published **************}

  // One light shaders.
  TVKSLBumpShaderMT = class(TVKCustomGLSLBumpShaderMT)
  published
    property MainTextureName;
    property NormalTextureName;
    property SpecularTextureName;
    property MaterialLibrary;

    property BumpHeight;
    property BumpSmoothness;

    property SpecularPower;
    property SpecularSpread;
    property LightPower;
  end;

  TVKSLBumpShader = class(TVKCustomGLSLBumpShader)
  published
    property NormalTextureName;
    property SpecularTextureName;
    property MaterialLibrary;

    property BumpHeight;
    property BumpSmoothness;

    property SpecularPower;
    property SpecularSpread;
    property LightPower;
  end;

  TVKSLBumpShaderAM = class(TVKCustomGLSLBumpShaderAM)
  published
    property AmbientColor;
    property DiffuseColor;
    property SpecularColor;
    property Alpha stored False;

    property MainTextureName;
    property NormalTextureName;
    property SpecularTextureName;
    property MaterialLibrary;

    property BumpHeight;
    property BumpSmoothness;

    property SpecularPower;
    property SpecularSpread;
    property LightPower;
  end;

  // Multi light shaders.
  TVKSLMLBumpShader = class(TVKCustomGLSLMLBumpShader)
  published
    property NormalTextureName;
    property SpecularTextureName;
    property MaterialLibrary;

    property BumpHeight;
    property BumpSmoothness;

    property SpecularPower;
    property SpecularSpread;
    property LightPower;
    property LightSources;
    property LightCompensation;
  end;

  TVKSLMLBumpShaderMT = class(TVKCustomGLSLMLBumpShaderMT)
  published
    property MainTextureName;
    property NormalTextureName;
    property SpecularTextureName;
    property MaterialLibrary;

    property BumpHeight;
    property BumpSmoothness;

    property SpecularPower;
    property SpecularSpread;
    property LightPower;
    property LightSources;
    property LightCompensation;
  end;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
procedure GetVertexProgramCode(const Code: TStrings);
begin
  with Code do
  begin
    Clear;
    Add('varying vec2 Texcoord; ');
    Add('varying vec3 ViewDirection; ');
    Add('varying vec3 LightDirection; ');
    Add(' ');
    Add('void main( void ) ');
    Add('{ ');
    Add('   gl_Position = ftransform(); ');
    Add('   Texcoord    = gl_MultiTexCoord0.xy; ');
    Add(' ');
    Add('   vec3 fvViewDirection   = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add('   vec3 fvLightDirection  =  gl_LightSource[0].position.xyz - fvViewDirection; ');
    Add(' ');
    Add('   vec3 fvNormal         = gl_NormalMatrix * gl_Normal; ');
    Add('   vec3 fvBinormal       = gl_NormalMatrix * gl_MultiTexCoord2.xyz; ');
    Add('   vec3 fvTangent        = gl_NormalMatrix * gl_MultiTexCoord1.xyz; ');
    Add(' ');
    Add('   ViewDirection.x  = dot( fvTangent, fvViewDirection ); ');
    Add('   ViewDirection.y  = dot( fvBinormal, fvViewDirection ); ');
    Add('   ViewDirection.z  = dot( fvNormal, fvViewDirection ); ');
    Add(' ');
    Add('   LightDirection.x  = dot( fvTangent, fvLightDirection ); ');
    Add('   LightDirection.y  = dot( fvBinormal, fvLightDirection ); ');
    Add('   LightDirection.z  = dot( fvNormal, fvLightDirection ); ');
    Add(' ');
    Add('   LightDirection = normalize(LightDirection); ');
    Add('   ViewDirection  = normalize(ViewDirection); ');
    Add('} ');
  end;
end;

procedure GetFragmentProgramCodeMP(const Code: TStrings; const UseSpecularMap: Boolean; const UseNormalMap: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform vec4 fvAmbient; ');
    Add('uniform vec4 fvSpecular; ');
    Add('uniform vec4 fvDiffuse; ');
    Add(' ');
    Add('uniform float fLightPower; ');
    Add('uniform float fSpecularPower; ');
    Add('uniform float fSpecularSpread; ');
    if UseNormalMap then
    begin
      Add('uniform sampler2D bumpMap; ');
      Add('uniform float fBumpHeight; ');
      Add('uniform float fBumpSmoothness; ');
    end;
    Add(' ');
    Add('uniform sampler2D baseMap; ');
    if UseSpecularMap then
      Add('uniform sampler2D specMap; ');
    Add(' ');
    Add('varying vec2 Texcoord; ');
    Add('varying vec3 ViewDirection; ');
    Add('varying vec3 LightDirection; ');
    Add(' ');
    Add('void main( void ) ');
    Add('{ ');
    if UseNormalMap then
      Add('   vec3  fvNormal = normalize( ( texture2D( bumpMap, Texcoord ).xyz * fBumpSmoothness) - fBumpHeight * fBumpSmoothness); ')
    else
      Add('   vec3  fvNormal = vec3(0.0, 0.0, 1);');
    Add(' ');
    Add('   float fNDotL           = dot( fvNormal, LightDirection ); ');
    Add('   vec3  fvReflection     = normalize( ( (fSpecularSpread * fvNormal ) * fNDotL ) - LightDirection ); ');
    Add(' ');
    Add('   float fRDotV           = max( dot( fvReflection, -ViewDirection ), 0.0 ); ');
    Add(' ');
    Add('   vec4  fvBaseColor      = texture2D( baseMap, Texcoord ); ');
    if UseSpecularMap then
      Add('   vec4  fvSpecColor      = texture2D( specMap, Texcoord ) * fvSpecular; ')
    else
      Add('   vec4  fvSpecColor      =                                  fvSpecular; ');
    Add(' ');
    Add('   vec4  fvTotalDiffuse   = clamp(fvDiffuse * fNDotL, 0.0, 1.0); ');
    Add(' ');
    Add('    //  (fvTotalDiffuse + 0.2) / 1.2 is used for removing artefacts on the non-lit side ');
    Add('   vec4  fvTotalSpecular  = clamp((pow(fRDotV, fSpecularPower ) ) * (fvTotalDiffuse + 0.2) / 1.2 * fvSpecColor, 0.0, 1.0); ');
    Add(' ');
    Add('   gl_FragColor = fLightPower * (fvBaseColor * ( fvAmbient + fvTotalDiffuse ) + fvTotalSpecular); ');
    Add('} ');
  end;
end;

procedure GetFragmentProgramCode(const Code: TStrings; const UseSpecularMap: Boolean; const UseNormalMap: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform float fLightPower; ');
    Add('uniform float fSpecularPower; ');
    Add('uniform float fSpecularSpread; ');
    if UseNormalMap then
    begin
      Add('uniform sampler2D bumpMap; ');
      Add('uniform float fBumpHeight; ');
      Add('uniform float fBumpSmoothness; ');
    end;
    Add(' ');
    Add('uniform sampler2D baseMap; ');
    if UseSpecularMap then
      Add('uniform sampler2D specMap; ');
    Add(' ');
    Add('varying vec2 Texcoord; ');
    Add('varying vec3 ViewDirection; ');
    Add('varying vec3 LightDirection; ');
    Add(' ');
    Add('void main( void ) ');
    Add('{ ');
    if UseNormalMap then
      Add('   vec3  fvNormal = normalize( ( texture2D( bumpMap, Texcoord ).xyz * fBumpSmoothness) - fBumpHeight * fBumpSmoothness); ')
    else
      Add('   vec3  fvNormal = vec3(0.0, 0.0, 1.0);');
    Add(' ');
    Add('   float fNDotL           = dot( fvNormal, LightDirection ); ');
    Add('   vec3  fvReflection     = normalize( ( (fSpecularSpread * fvNormal ) * fNDotL ) - LightDirection ); ');
    Add(' ');
    Add('   float fRDotV           = max(dot( fvReflection, -ViewDirection ), 0.0); ');
    Add(' '); 
    Add('   vec4  fvBaseColor      = texture2D( baseMap, Texcoord ); ');
    if UseSpecularMap then
      Add('   vec4  fvSpecColor      = texture2D( specMap, Texcoord ) * gl_LightSource[0].specular; ')
    else
      Add('   vec4  fvSpecColor      =                                  gl_LightSource[0].specular; ');
    Add(' ');
    Add('   vec4  fvTotalDiffuse   = clamp(gl_LightSource[0].diffuse * fNDotL, 0.0, 1.0); ');
    Add(' ');
    Add('    //  (fvTotalDiffuse + 0.2) / 1.2 is used for removing artefacts on the non-lit side ');
    Add('   vec4  fvTotalSpecular  = clamp((pow(fRDotV, fSpecularPower ) ) * (fvTotalDiffuse + 0.2) / 1.2 * fvSpecColor, 0.0, 1.0); ');
    Add(' ');
    Add('   gl_FragColor = fLightPower * (fvBaseColor * ( gl_LightSource[0].ambient + fvTotalDiffuse ) + fvTotalSpecular); ');
    Add('} ');
  end;
end;

procedure GetMLVertexProgramCode(const Code: TStrings);
begin
  with Code do
  begin
    Clear;
    Add('varying vec2 Texcoord; ');
    Add('varying vec3 ViewDirection; ');
    Add(' ');
    Add('varying vec3 fvViewDirection; ');
    Add('varying vec3 fvNormal; ');
    Add('varying vec3 fvBinormal; ');
    Add('varying vec3 fvTangent; ');
    Add(' ');
    Add('void main( void ) ');
    Add('{ ');
    Add('   gl_Position = ftransform(); ');
    Add('   Texcoord    = gl_MultiTexCoord0.xy; ');
    Add(' ');
    Add('   fvViewDirection   = (gl_ModelViewMatrix * gl_Vertex).xyz; ');
    Add(' ');
    Add('   fvNormal         = gl_NormalMatrix * gl_Normal; ');
    Add('   fvBinormal       = gl_NormalMatrix * gl_MultiTexCoord2.xyz; ');
    Add('   fvTangent        = gl_NormalMatrix * gl_MultiTexCoord1.xyz; ');
    Add(' ');
    Add('   ViewDirection.x  = dot( fvTangent, fvViewDirection ); ');
    Add('   ViewDirection.y  = dot( fvBinormal, fvViewDirection ); ');
    Add('   ViewDirection.z  = dot( fvNormal, fvViewDirection ); ');
    Add(' ');
    Add('   ViewDirection  = normalize(ViewDirection); ');
    Add('} ');
  end;
end;

procedure GetMLFragmentProgramCodeBeg(const Code: TStrings; const UseSpecularMap: Boolean; const UseNormalMap: Boolean);
begin
  with Code do
  begin
    Clear;
    Add('uniform float fLightPower; ');
    Add('uniform float fSpecularPower; ');
    Add('uniform float fSpecularSpread; ');
    if UseNormalMap then
    begin
      Add('uniform sampler2D bumpMap; ');
      Add('uniform float fBumpHeight; ');
      Add('uniform float fBumpSmoothness; ');
    end;
    Add(' ');
    Add('uniform sampler2D baseMap; ');
    if UseSpecularMap then
      Add('uniform sampler2D specMap; ');
    Add(' ');
    Add('varying vec2 Texcoord; ');
    Add('varying vec3 ViewDirection; ');
    Add(' ');
    Add('varying vec3 fvViewDirection; ');
    Add('varying vec3 fvNormal; ');
    Add('varying vec3 fvBinormal; ');
    Add('varying vec3 fvTangent; ');
    Add(' ');
    Add('void main( void ) ');
    Add('{ ');
    Add('   vec3 LightDirection;');
    Add('   vec3 fvLightDirection; ');
    Add(' ');
    if UseNormalMap then
      Add('   vec3  fvBumpNormal = normalize( ( texture2D( bumpMap, Texcoord ).xyz * fBumpSmoothness) - fBumpHeight * fBumpSmoothness); ')
    else
      Add('   vec3  fvBumpNormal = vec3(0.0, 0.0, 1);');
    Add(' ');
    Add('   float fNDotL        ; ');
    Add('   vec3  fvReflection  ; ');
    Add('   float fRDotV        ; ');
    Add('   vec4  fvBaseColor = texture2D( baseMap, Texcoord ); ');
    if UseSpecularMap then
      Add('   vec4 fvSpecColor      = texture2D( specMap, Texcoord ); ')
    else
      Add('   vec4 fvSpecColor      = vec4(1.0, 1.0, 1.0, 1.0); ');
    Add('   vec4  fvNewDiffuse  ; ');
    Add('   vec4  fvTotalDiffuse  = vec4(0, 0, 0, 0); ');
    Add('   vec4  fvTotalAmbient  = vec4(0, 0, 0, 0); ');
    Add('   vec4  fvTotalSpecular = vec4(0, 0, 0, 0); ');
  end;
end;

procedure GetMLFragmentProgramCodeMid(const Code: TStrings; const CurrentLight: Integer);
begin
  with Code do
  begin
    Add('   fvLightDirection  = gl_LightSource[' + IntToStr(CurrentLight) + '].position.xyz - fvViewDirection; ');
    Add(' ');
    Add('   LightDirection.x  = dot( fvTangent, fvLightDirection ); ');
    Add('   LightDirection.y  = dot( fvBinormal, fvLightDirection ); ');
    Add('   LightDirection.z  = dot( fvNormal, fvLightDirection ); ');
    Add('   LightDirection = normalize(LightDirection); ');
    Add(' ');
    Add('   fNDotL           = dot( fvBumpNormal, LightDirection ); ');
    Add('   fvReflection     = normalize( ( (fSpecularSpread * fvBumpNormal ) * fNDotL ) - LightDirection ); ');
    Add('   fRDotV           = max( dot( fvReflection, -ViewDirection ), 0.0 ); ');
    Add('   fvNewDiffuse     = clamp(gl_LightSource[' + IntToStr(CurrentLight) + '].diffuse * fNDotL, 0.0, 1.0); ');
    Add('   fvTotalDiffuse   = min(fvTotalDiffuse + fvNewDiffuse, 1.0); ');
    Add('   fvTotalSpecular  = min(fvTotalSpecular + clamp((pow(fRDotV, fSpecularPower ) ) * (fvNewDiffuse + 0.2) / 1.2 * (fvSpecColor * gl_LightSource[' + IntToStr(CurrentLight) + '].specular), 0.0, 1.0), 1.0); ');
    Add('   fvTotalAmbient   = fvTotalAmbient + gl_LightSource[' + IntToStr(CurrentLight) + '].ambient; ');
  end;
end;

procedure GetMLFragmentProgramCodeEnd(const Code: TStrings; const FLightCount: Integer; const FLightCompensation: Single);
var
  Temp: AnsiString;
begin
  with Code do
  begin
    Str((1 + (FLightCount  - 1) * FLightCompensation) / FLightCount :1 :1, Temp);
    if (FLightCount = 1) or (FLightCompensation = 1) then
      Add('   gl_FragColor = fLightPower * (fvBaseColor * ( fvTotalAmbient + fvTotalDiffuse ) + fvTotalSpecular); ')
    else
      Add('   gl_FragColor = fLightPower * (fvBaseColor * ( fvTotalAmbient + fvTotalDiffuse ) + fvTotalSpecular) * ' + string(Temp) + '; ');
    Add('} ');
  end;
end;


{ TVKBaseCustomGLSLBumpShader }

constructor TVKBaseCustomGLSLBumpShader.Create(AOwner: TComponent);
begin
  inherited;
  FSpecularPower := 6;
  FSpecularSpread := 1.5;
  FLightPower := 1;

  FBumpHeight := 0.5;
  FBumpSmoothness := 300;
  TStringList(VertexProgram.Code).OnChange := nil;
  TStringList(FragmentProgram.Code).OnChange := nil;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
end;

procedure TVKBaseCustomGLSLBumpShader.DoApply(
  var rci: TVKRenderContextInfo; Sender: TObject);
begin
  // Don't inherit not to call the event.
  GetGLSLProg.UseProgramObject;

  Param['fSpecularPower'].AsVector1f := FSpecularPower;
  Param['fSpecularSpread'].AsVector1f := FSpecularSpread;
  Param['fLightPower'].AsVector1f := FLightPower;

  if FSpecularTexture <> nil then
    Param['specMap'].AsTexture2D[2] := FSpecularTexture;

{$IFNDEF GLS_OPTIMIZATIONS}
  if FNormalTexture <> nil then
{$ENDIF}
  begin
    Param['bumpMap'].AsTexture2D[1] := FNormalTexture;
    Param['fBumpHeight'].AsVector1f := FBumpHeight;
    Param['fBumpSmoothness'].AsVector1f := FBumpSmoothness;
  end;
end;

function TVKBaseCustomGLSLBumpShader.DoUnApply(
  var rci: TVKRenderContextInfo): Boolean;
begin
  //don't inherit not to call the event
  Result := False;
  GetGLSLProg.EndUseProgramObject;
end;

function TVKBaseCustomGLSLBumpShader.GetMaterialLibrary: TVKAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

function TVKBaseCustomGLSLBumpShader.GetNormalTextureName: TVKLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FNormalTexture);
  if Result = '' then Result := FNormalTextureName;
end;

function TVKBaseCustomGLSLBumpShader.GetSpecularTextureName: TVKLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FSpecularTexture);
  if Result = '' then Result := FSpecularTextureName;
end;

procedure TVKBaseCustomGLSLBumpShader.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        // Need to nil the textures that were ownned by it.
        if FNormalTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FNormalTexture);
          if Index <> -1 then
            SetNormalTexture(nil);
        end;

        if FSpecularTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FSpecularTexture);
          if Index <> -1 then
            SetSpecularTexture(nil);
        end;

        FMaterialLibrary := nil;
      end;
end;

procedure TVKBaseCustomGLSLBumpShader.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
begin
  if FMaterialLibrary <> nil then
    FMaterialLibrary.RemoveFreeNotification(Self);
  FMaterialLibrary := Value;

  if FMaterialLibrary <> nil then
  begin
    FMaterialLibrary.FreeNotification(Self);

    if FNormalTextureName <> '' then
      SetNormalTextureName(FNormalTextureName);

    if FSpecularTextureName <> '' then
      SetSpecularTextureName(FSpecularTextureName);
  end
  else
  begin
    FNormalTextureName := '';
    FSpecularTextureName := '';
  end;
end;

procedure TVKBaseCustomGLSLBumpShader.SetNormalTexture(
  const Value: TVKTexture);
begin
  FNormalTexture := Value;
  FinalizeShader;
end;

procedure TVKBaseCustomGLSLBumpShader.SetNormalTextureName(
  const Value: TVKLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FNormalTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EGLSLBumpShaderException.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    SetNormalTexture(FMaterialLibrary.TextureByName(Value));
    FNormalTextureName := '';
  end;
end;

procedure TVKBaseCustomGLSLBumpShader.SetSpecularTexture(
  const Value: TVKTexture);
begin
  FSpecularTexture := Value;
  FinalizeShader;
end;

procedure TVKBaseCustomGLSLBumpShader.SetSpecularTextureName(
  const Value: TVKLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FSpecularTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EGLSLBumpShaderException.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    SetSpecularTexture(FMaterialLibrary.TextureByName(Value));
    FSpecularTextureName := '';
  end;
end;

{ TVKBaseCustomGLSLBumpShaderMT }

function TVKBaseCustomGLSLBumpShaderMT.GetMainTextureName: TVKLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfTexture(FMainTexture);
  if Result = '' then Result := FMainTextureName;
end;

procedure TVKBaseCustomGLSLBumpShaderMT.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  if Operation = opRemove then
    if AComponent = FMaterialLibrary then
      if FMaterialLibrary <> nil then
      begin
        //need to nil the textures that were ownned by it
        if FMainTexture <> nil then
        begin
          Index := FMaterialLibrary.Materials.GetTextureIndex(FMainTexture);
          if Index <> -1 then
            FMainTexture := nil;
        end;
      end;
  inherited;
end;

procedure TVKBaseCustomGLSLBumpShaderMT.SetMainTextureName(
  const Value: TVKLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FMainTextureName := Value;
    if not (csLoading in ComponentState) then
      raise EGLSLBumpShaderException.Create(glsErrorEx + glsMatLibNotDefined);
  end
  else
  begin
    FMainTexture := FMaterialLibrary.TextureByName(Value);
    FMainTextureName := '';
  end;  
end;

procedure TVKBaseCustomGLSLBumpShaderMT.SetMaterialLibrary(
  const Value: TVKMaterialLibrary);
begin
  inherited;
  if FMaterialLibrary <> nil then
  begin
    if FMainTextureName <> '' then
      SetMainTextureName(FMainTextureName);
  end
  else
    FMainTextureName := '';
end;

{ TVKCustomGLSLBumpShaderAM }

constructor TVKCustomGLSLBumpShaderAM.Create(AOwner: TComponent);
begin
  inherited;

  FAmbientColor := TVKColor.Create(Self);
  FDiffuseColor := TVKColor.Create(Self);
  FSpecularColor := TVKColor.Create(Self);

  // Setup initial parameters.
  FAmbientColor.SetColor(0.15, 0.15, 0.15, 1);
  FDiffuseColor.SetColor(1, 1, 1, 1);
  FSpecularColor.SetColor(1, 1, 1, 1);
end;

destructor TVKCustomGLSLBumpShaderAM.Destroy;
begin
  FAmbientColor.Destroy;
  FDiffuseColor.Destroy;
  FSpecularColor.Destroy;

  inherited;
end;

procedure TVKCustomGLSLBumpShaderAM.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin
  inherited;

  Param['fvAmbient'].AsVector4f := FAmbientColor.Color;
  Param['fvDiffuse'].AsVector4f := FDiffuseColor.Color;
  Param['fvSpecular'].AsVector4f := FSpecularColor.Color;

  Param['baseMap'].AsTexture2D[0] := FMainTexture;
end;

procedure TVKCustomGLSLBumpShaderAM.DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject);
begin
  GetVertexProgramCode(VertexProgram.Code);
  GetFragmentProgramCodeMP(FragmentProgram.Code, FSpecularTexture <> nil, FNormalTexture <> nil);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

function TVKCustomGLSLBumpShaderAM.GetAlpha: Single;
begin
  Result := (FAmbientColor.Alpha + FDiffuseColor.Alpha + FSpecularColor.Alpha) / 3;
end;

procedure TVKCustomGLSLBumpShaderAM.SetAlpha(const Value: Single);
begin
  FAmbientColor.Alpha := Value;
  FDiffuseColor.Alpha := Value;
  FSpecularColor.Alpha := Value;
end;

{ TVKCustomGLSLMLBumpShaderMT }

constructor TVKCustomGLSLMLBumpShaderMT.Create(AOwner: TComponent);
begin
  inherited;

  FLightSources := [1];
  FLightCompensation := 1;
end;

procedure TVKCustomGLSLMLBumpShaderMT.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin
  inherited;
  Param['baseMap'].AsTexture2D[0] := FMainTexture;
end;

procedure TVKCustomGLSLMLBumpShaderMT.DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject);
var
  I: Integer;
  lLightCount: Integer;
begin
  GetMLVertexProgramCode(VertexProgram.Code);
  
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, FSpecularTexture <> nil, FNormalTexture <> nil);

    lLightCount := 0;
    // Repeat for all lights.
    for I := 0 to glsShaderMaxLightSources - 1 do
      if I + 1 in FLightSources then
      begin
        GetMLFragmentProgramCodeMid(FragmentProgram.Code, I);
        Inc(lLightCount);
      end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code, lLightCount, FLightCompensation);
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

procedure TVKCustomGLSLMLBumpShaderMT.SetLightCompensation(
  const Value: Single);
begin
  FLightCompensation := Value;
  FinalizeShader;
end;

procedure TVKCustomGLSLMLBumpShaderMT.SetLightSources(
  const Value: TVKLightSourceSet);
begin
  Assert(Value <> [], glsErrorEx + glsShaderNeedsAtLeastOneLightSource);
  FLightSources := Value;
  FinalizeShader;
end;

{ TVKCustomGLSLBumpShaderMT }

procedure TVKCustomGLSLBumpShaderMT.DoApply(
  var rci: TVKRenderContextInfo; Sender: TObject);
begin
  inherited;
  Param['baseMap'].AsTexture2D[0] := FMainTexture;
end;

procedure TVKCustomGLSLBumpShaderMT.DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject);
begin
  GetVertexProgramCode(VertexProgram.Code);
  GetFragmentProgramCode(FragmentProgram.Code, FSpecularTexture <> nil, FNormalTexture <> nil);
  inherited;
end;

{ TVKCustomGLSLBumpShader }

procedure TVKCustomGLSLBumpShader.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin
  inherited;
  Param['baseMap'].AsVector1i := 0;  // Use the current texture.
end;

procedure TVKCustomGLSLBumpShader.DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject);
begin
  GetVertexProgramCode(VertexProgram.Code);
  GetFragmentProgramCode(FragmentProgram.Code, FSpecularTexture <> nil, FNormalTexture <> nil);
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;


function TVKCustomGLSLBumpShader.GetShaderAlpha: Single;
begin
  //ignore
  Result := -1;
end;

procedure TVKCustomGLSLBumpShader.GetShaderColorParams(var AAmbientColor,
  ADiffuseColor, ASpecularcolor: TVector4f);
begin
  //ignore
  AAmbientColor := NullHmgVector;
  ADiffuseColor := NullHmgVector;
  ASpecularcolor := NullHmgVector;
end;

procedure TVKCustomGLSLBumpShader.GetShaderTextures(
  var Textures: array of TVKTexture);
begin
  Textures[0] := FNormalTexture;
  Textures[1] := FSpecularTexture;
end;

procedure TVKCustomGLSLBumpShader.GetShaderMiscParameters(var ACadencer: TVKCadencer;
  var AMatLib: TVKMaterialLibrary; var ALightSources: TVKLightSourceSet);
begin
  ACadencer := nil;
  AMatLib := FMaterialLibrary;
  ALightSources := [0];
end;

procedure TVKCustomGLSLBumpShader.SetShaderAlpha(const Value: Single);
begin
  //ignore
end;

procedure TVKCustomGLSLBumpShader.SetShaderColorParams(const AAmbientColor,
  ADiffuseColor, ASpecularcolor: TVector4f);
begin
  //ignore
end;

procedure TVKCustomGLSLBumpShader.SetShaderMiscParameters(
  const ACadencer: TVKCadencer; const AMatLib: TVKMaterialLibrary;
  const ALightSources: TVKLightSourceSet);
begin
  SetMaterialLibrary(AMatLib);
end;

procedure TVKCustomGLSLBumpShader.SetShaderTextures(
  const Textures: array of TVKTexture);
begin
  SetNormalTexture(Textures[0]);
  SetSpecularTexture(Textures[1]);
end;

function TVKCustomGLSLBumpShader.GetShaderDescription: string;
begin
  Result := 'ShaderTexture1 is NormalMap, ShaderTexture2 is SpecularMap'
end;


{ TVKCustomGLSLMLBumpShader }

constructor TVKCustomGLSLMLBumpShader.Create(AOwner: TComponent);
begin
  inherited;

  FLightSources := [1];
  FLightCompensation := 1;
end;

procedure TVKCustomGLSLMLBumpShader.DoApply(var rci: TVKRenderContextInfo;
  Sender: TObject);
begin
  inherited;
  Param['baseMap'].AsVector1i := 0;  // Use the current texture.
end;

procedure TVKCustomGLSLMLBumpShader.DoInitialize(var rci : TVKRenderContextInfo; Sender : TObject);
var
  I: Integer;
  lLightCount: Integer;
begin
  GetMLVertexProgramCode(VertexProgram.Code);
  
  with FragmentProgram.Code do
  begin
    GetMLFragmentProgramCodeBeg(FragmentProgram.Code, FSpecularTexture <> nil, FNormalTexture <> nil);

    lLightCount := 0;

    // Repeat for all lights.
    for I := 0 to glsShaderMaxLightSources - 1 do
      if I + 1 in FLightSources then
      begin
        GetMLFragmentProgramCodeMid(FragmentProgram.Code, I);
        Inc(lLightCount);
      end;

    GetMLFragmentProgramCodeEnd(FragmentProgram.Code, lLightCount, FLightCompensation);
  end;
  VertexProgram.Enabled := True;
  FragmentProgram.Enabled := True;
  inherited;
end;

procedure TVKCustomGLSLMLBumpShader.SetLightCompensation(
  const Value: Single);
begin
  FLightCompensation := Value;
  FinalizeShader;
end;

procedure TVKCustomGLSLMLBumpShader.SetLightSources(
  const Value: TVKLightSourceSet);
begin
  Assert(Value <> [], glsErrorEx + glsShaderNeedsAtLeastOneLightSource);
  FLightSources := Value;
  FinalizeShader;
end;

function TVKCustomGLSLMLBumpShader.GetShaderAlpha: Single;
begin
  //ignore
  Result := -1;
end;

procedure TVKCustomGLSLMLBumpShader.GetShaderColorParams(var AAmbientColor,
  ADiffuseColor, ASpecularcolor: TVector4f);
begin
  //ignore
  AAmbientColor := NullHmgVector;
  ADiffuseColor := NullHmgVector;
  ASpecularcolor := NullHmgVector;
end;

function TVKCustomGLSLMLBumpShader.GetShaderDescription: string;
begin
  Result := 'ShaderTexture1 is NormalMap, ShaderTexture2 is SpecularMap';
end;

procedure TVKCustomGLSLMLBumpShader.GetShaderMiscParameters(
  var ACadencer: TVKCadencer; var AMatLib: TVKMaterialLibrary;
  var ALightSources: TVKLightSourceSet);
begin
  ACadencer := nil;
  AMatLib := FMaterialLibrary;
  ALightSources := FLightSources;
end;

procedure TVKCustomGLSLMLBumpShader.GetShaderTextures(
  var Textures: array of TVKTexture);
begin
  Textures[0] := FNormalTexture;
  Textures[1] := FSpecularTexture;
end;

procedure TVKCustomGLSLMLBumpShader.SetShaderAlpha(const Value: Single);
begin
  //ignore
end;

procedure TVKCustomGLSLMLBumpShader.SetShaderColorParams(const AAmbientColor,
  ADiffuseColor, ASpecularcolor: TVector4f);
begin
  //ignore
end;

procedure TVKCustomGLSLMLBumpShader.SetShaderMiscParameters(
  const ACadencer: TVKCadencer; const AMatLib: TVKMaterialLibrary;
  const ALightSources: TVKLightSourceSet);
begin
  SetMaterialLibrary(AMatLib);
  SetLightSources(ALightSources);
end;

procedure TVKCustomGLSLMLBumpShader.SetShaderTextures(
  const Textures: array of TVKTexture);
begin
  SetNormalTexture(Textures[0]);
  SetSpecularTexture(Textures[1]);
end;

initialization
  RegisterClasses([TVKSLBumpShaderMT, TVKSLBumpShader, TVKSLBumpShaderAM,
                   TVKSLMLBumpShader, TVKSLMLBumpShaderMT]);

end.

