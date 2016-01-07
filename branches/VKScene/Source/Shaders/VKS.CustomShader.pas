// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
    A collection of pure abstract classes - descendants of TVKShader, which are
    used for purpose of not having to write the same stuff all over and over
    again in your own shader classes.
    It also contains a procedures and function that can be used in all shaders. 

      
}
unit VKS.CustomShader;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  //VKS
  VKS.VectorGeometry, VKS.VectorTypes, VKS.Texture, VKS.Cadencer, VKS.OpenGLTokens, VKS.Scene,
  VKS.Strings, VKS.CrossPlatform, VKS.Context, VKS.RenderContextInfo, VKS.Material,
  VKS.VectorLists, VKS.TextureFormat, VKS.GLSLParameter;

const
  glsShaderMaxLightSources = 8;

type
  TVKShaderFogSupport = (sfsEnabled, sfsDisabled, sfsAuto);
  TVKTransformFeedBackMode = (tfbmInterleaved, tfbmSeparate);

  EGLCustomShaderException = class(EGLShaderException);

  TVKCustomShader = class;
  TVKVertexProgram = class;
  TVKFragmentProgram = class;
  TVKGeometryProgram = class;

  TVKShaderEvent = procedure(Shader: TVKCustomShader) of object;
  TVKShaderUnAplyEvent = procedure(Shader: TVKCustomShader; var ThereAreMorePasses: Boolean) of object;

  TVKLightSourceEnum = 1..glsShaderMaxLightSources;
  TVKLightSourceSet = set of TVKLightSourceEnum;

  { This interface describes user shaders, in order to be able to access them
    via a unified interface. If user shader does not support some option, don't
    raise an axception, just ignore it.
  }
  IGLShaderDescription = interface
  ['{04089C64-60C2-43F5-AC9C-38ED46264812}']
    procedure SetShaderTextures(const Textures: array of TVKTexture);
    procedure GetShaderTextures(var Textures: array of TVKTexture);

    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);

    procedure SetShaderMiscParameters(const ACadencer: TVKCadencer; const AMatLib: TVKMaterialLibrary; const ALightSources: TVKLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TVKCadencer; var AMatLib: TVKMaterialLibrary; var ALightSources: TVKLightSourceSet);

    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);

    function GetShaderDescription: string;
  end;

  { Used in the TVKPostShaderHolder component. }
  IGLPostShader = interface
  ['{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}']
    { Called on every pass. }
    procedure DoUseTempTexture(const TempTexture: TVKTextureHandle;
      TextureTarget: TVKTextureTarget);
    { Called to determine if it is compatible. }
    function GetTextureTarget: TVKTextureTarget;
  end;

  { A pure abstract class, must be overriden. }
  TVKCustomShader = class(TVKShader)
  private
    FFragmentProgram: TVKFragmentProgram;
    FVertexProgram: TVKVertexProgram;
    FGeometryProgram: TVKGeometryProgram;

    FTagObject: TObject;
    procedure SetFragmentProgram(const Value: TVKFragmentProgram);
    procedure SetGeometryProgram(const Value: TVKGeometryProgram);
    procedure SetVertexProgram(const Value: TVKVertexProgram);
    function StoreFragmentProgram: Boolean;
    function StoreGeometryProgram: Boolean;
    function StoreVertexProgram: Boolean;
  protected
    FDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean); virtual;

    property FragmentProgram: TVKFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
    property VertexProgram: TVKVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    property GeometryProgram: TVKGeometryProgram read FGeometryProgram write SetGeometryProgram stored StoreGeometryProgram;

    { Treats warnings as errors and displays this error,
       instead of a general shader-not-supported message. }
    property DebugMode: Boolean read FDebugMode write SetDebugMode default False;
    property TagObject: TObject read FTagObject write FTagObject default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
  end;

  { A custom shader program. }
  TVKShaderProgram = class(TPersistent)
  private
    FParent: TVKCustomShader;
    FEnabled: Boolean;
    FCode: TStrings;
    procedure SetCode(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure OnChangeCode(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure Apply; virtual;
    constructor Create(const AParent: TVKCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Code: TStrings read FCode write SetCode;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  TVKVertexProgram = class(TVKShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TVKFragmentProgram = class(TVKShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TVKGeometryProgram = class(TVKShaderProgram)
  private
    FInputPrimitiveType: TVKgsInTypes;
    FOutputPrimitiveType: TVKgsOutTypes;
    FVerticesOut: TGLint;
    procedure SetInputPrimitiveType(const Value: TVKgsInTypes);
    procedure SetOutputPrimitiveType(const Value: TVKgsOutTypes);
    procedure SetVerticesOut(const Value: TGLint);
  public
    constructor Create(const AParent: TVKCustomShader); override;
  published
    property Code;
    property Enabled;

    property InputPrimitiveType: TVKgsInTypes read FInputPrimitiveType write SetInputPrimitiveType default gsInPoints;
    property OutputPrimitiveType: TVKgsOutTypes read FOutputPrimitiveType write SetOutputPrimitiveType default gsOutPoints;
    property VerticesOut: TGLint read FVerticesOut write SetVerticesOut default 0;
  end;

  { Wrapper around a parameter of the main program. }
  TVKCustomShaderParameter = class(TObject)
  private
    { Private Declarations }
  protected
    { Protected Declarations }
    function GetAsVector1f: Single; virtual; abstract;
    function GetAsVector2f: TVector2f; virtual; abstract;
    function GetAsVector3f: TVector3f; virtual; abstract;
    function GetAsVector4f: TVector; virtual; abstract;

    function GetAsVector1i: Integer; virtual; abstract;
    function GetAsVector2i: TVector2i; virtual; abstract;
    function GetAsVector3i: TVector3i; virtual; abstract;
    function GetAsVector4i: TVector4i; virtual; abstract;

    function GetAsVector1ui: GLuint; virtual; abstract;
    function GetAsVector2ui: TVector2ui; virtual; abstract;
    function GetAsVector3ui: TVector3ui; virtual; abstract;
    function GetAsVector4ui: TVector4ui; virtual; abstract;

    procedure SetAsVector1f(const Value: Single); virtual; abstract;
    procedure SetAsVector2f(const Value: TVector2f); virtual; abstract;
    procedure SetAsVector3f(const Value: TVector3f); virtual; abstract;
    procedure SetAsVector4f(const Value: TVector4f); virtual; abstract;

    procedure SetAsVector1i(const Value: Integer); virtual; abstract;
    procedure SetAsVector2i(const Value: TVector2i); virtual; abstract;
    procedure SetAsVector3i(const Value: TVector3i); virtual; abstract;
    procedure SetAsVector4i(const Value: TVector4i); virtual; abstract;

    procedure SetAsVector1ui(const Value: GLuint); virtual; abstract;
    procedure SetAsVector2ui(const Value: TVector2ui); virtual; abstract;
    procedure SetAsVector3ui(const Value: TVector3ui); virtual; abstract;
    procedure SetAsVector4ui(const Value: TVector4ui); virtual; abstract;

    function GetAsMatrix2f: TMatrix2f; virtual; abstract;
    function GetAsMatrix3f: TMatrix3f; virtual; abstract;
    function GetAsMatrix4f: TMatrix4f; virtual; abstract;
    procedure SetAsMatrix2f(const Value: TMatrix2f); virtual; abstract;
    procedure SetAsMatrix3f(const Value: TMatrix3f); virtual; abstract;
    procedure SetAsMatrix4f(const Value: TMatrix4f); virtual; abstract;

    procedure SetAsTexture(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TVKTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TVKTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVKTextureTarget): Cardinal; virtual; abstract;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVKTextureTarget; const Value: Cardinal); virtual; abstract;

    function GetAsUniformBuffer: GLenum; virtual; abstract;
    procedure SetAsUniformBuffer(UBO: GLenum); virtual; abstract;
  public
    { Public Declarations }

    { This overloaded SetAsVector accepts open array as input. e.g.
       SetAsVectorF([0.1, 0.2]). Array length must between 1-4. }
    procedure SetAsVectorF(const Values: array of Single); overload;
    procedure SetAsVectorI(const Values: array of Integer); overload;

    { SetToTextureOf determines texture type on-the-fly.}
    procedure SetToTextureOf(const LibMaterial: TVKLibMaterial; const TextureIndex: Integer); overload;
    procedure SetToTextureOf(const Texture: TVKTexture; const TextureIndex: Integer); overload;

    //: GLScene-friendly properties.
    property AsVector: TVector read GetAsVector4f write SetAsVector4f;
    property AsAffineVector: TAffineVector read GetAsVector3f write SetAsVector3f;

    //: Standard types.
    property AsFloat: Single read GetAsVector1f write SetAsVector1f;
    property AsInteger: Integer read GetAsVector1i write SetAsVector1i;

    //: Float vector types.
    property AsVector1f: Single    read GetAsVector1f write SetAsVector1f;
    property AsVector2f: TVector2f read GetAsVector2f write SetAsVector2f;
    property AsVector3f: TVector3f read GetAsVector3f write SetAsVector3f;
    property AsVector4f: TVector4f read GetAsVector4f write SetAsVector4f;

    //: Integer vector  types.
    property AsVector1i: Integer   read GetAsVector1i write SetAsVector1i;
    property AsVector2i: TVector2i read GetAsVector2i write SetAsVector2i;
    property AsVector3i: TVector3i read GetAsVector3i write SetAsVector3i;
    property AsVector4i: TVector4i read GetAsVector4i write SetAsVector4i;

    //: Unsigned integer vector  types.
    property AsVector1ui: GLuint   read GetAsVector1ui write SetAsVector1ui;
    property AsVector2ui: TVector2ui read GetAsVector2ui write SetAsVector2ui;
    property AsVector3ui: TVector3ui read GetAsVector3ui write SetAsVector3ui;
    property AsVector4ui: TVector4ui read GetAsVector4ui write SetAsVector4ui;

    //: Matrix Types.
    property AsMatrix2f: TMatrix2f read GetAsMatrix2f write SetAsMatrix2f;
    property AsMatrix3f: TMatrix3f read GetAsMatrix3f write SetAsMatrix3f;
    property AsMatrix4f: TMatrix4f read GetAsMatrix4f write SetAsMatrix4f;

    //: Texture Types.
    property AsTexture    [const TextureIndex: Integer]: TVKTexture write SetAsTexture;
    property AsTexture1D  [const TextureIndex: Integer]: TVKTexture write SetAsTexture1D;
    property AsTexture2D  [const TextureIndex: Integer]: TVKTexture write SetAsTexture2D;
    property AsTexture3D  [const TextureIndex: Integer]: TVKTexture write SetAsTexture3D;
    property AsTextureRect[const TextureIndex: Integer]: TVKTexture write SetAsTextureRect;
    property AsTextureCube[const TextureIndex: Integer]: TVKTexture write SetAsTextureCube;

    property AsCustomTexture[const TextureIndex: Integer; TextureTarget: TVKTextureTarget]: Cardinal read GetAsCustomTexture write SetAsCustomTexture;

    property AsUniformBuffer: GLenum read GetAsUniformBuffer write SetAsUniformBuffer;
  end;


  { Adds two more blending modes to standard ones.
    Not sure how to name them or if they should be included in TBlending mode,
    so I created a new type here. }
  TVKBlendingModeEx = (bmxOpaque, bmxTransparency, bmxAdditive,
    bmxAlphaTest50, bmxAlphaTest100, bmxModulate,
    bmxDestColorOne, bmxDestAlphaOne);

// Exported procedures.
procedure ApplyBlendingModeEx(const BlendingMode: TVKBlendingModeEx);
procedure UnApplyBlendingModeEx;
procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TVKSize;
  const TextureTarget: TVKTextureTarget = ttTexture2D);
// Probably need to give them proper names, instead of numbers... 
procedure DrawTexturedScreenQuad;
procedure DrawTexturedScreenQuad2(const ViewPortSize: TVKSize);
procedure DrawTexturedScreenQuad3;
procedure DrawTexturedScreenQuad4(const ViewPortSize: TVKSize);
procedure DrawTexturedScreenQuad5(const ViewPortSize: TVKSize);
procedure DrawTexturedScreenQuad6(const ViewPortSize: TVKSize);

procedure CopyScreentoTexture(const ViewPortSize: TVKSize; const TextureTarget: Word = GL_TEXTURE_2D);
procedure CopyScreentoTexture2(const ViewPortSize: TVKSize; const TextureTarget: Word = GL_TEXTURE_2D);

function IsFogEnabled(const AFogSupportMode: TVKShaderFogSupport; var rci: TRenderContextInfo): Boolean;
procedure GetActiveLightsList(const ALightIDs: TIntegerList);

implementation

uses
  VKS.State;

procedure GetActiveLightsList(const ALightIDs: TIntegerList);
var
  I: Integer;
begin
  ALightIDs.Clear;
  with CurrentGLContext.GLStates do
  begin
    for I := 0 to MaxLights - 1 do
    begin
      if LightEnabling[I] then
        ALightIDs.Add(I);
    end;
  end;
end;

function IsFogEnabled(const AFogSupportMode: TVKShaderFogSupport; var rci: TRenderContextInfo): Boolean;
begin
  case AFogSupportMode of
    sfsEnabled:  Result := True;
    sfsDisabled: Result := False;
    sfsAuto:     Result := TVKSceneBuffer(rci.buffer).FogEnable;
  else
    Result := False;
    Assert(False, vksUnknownType);
  end;
end;

procedure CopyScreentoTexture(const ViewPortSize: TVKSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  GL.CopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, ViewPortSize.cx, ViewPortSize.cy);
end;

procedure CopyScreentoTexture2(const ViewPortSize: TVKSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  GL.CopyTexImage2D(TextureTarget, 0, GL_RGB, 0, 0, ViewPortSize.cx, ViewPortSize.cy, 0);
end;

procedure ApplyBlendingModeEx(const BlendingMode: TVKBlendingModeEx);
begin
  with CurrentGLContext.GLStates do
  begin
    Enable(stBlend);

    case BlendingMode of
      bmxOpaque: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxTransparency: SetBlendFunc(bfSRCALPHA, bfONEMINUSSRCALPHA);
      bmxAdditive: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxAlphaTest50: SetGLAlphaFunction(cfGEQUAL, 0.5);
      bmxAlphaTest100: SetGLAlphaFunction(cfGEQUAL, 1.0);
      bmxModulate: SetBlendFunc(bfDSTCOLOR, bfZERO);
      bmxDestColorOne: SetBlendFunc(bfDSTCOLOR, bfONE);
      bmxDestAlphaOne: SetBlendFunc(bfDSTALPHA, bfONE);
      else
        Assert(False, vksErrorEx + vksUnknownType);
    end;
  end;
end;

procedure UnApplyBlendingModeEx;
begin
end;

procedure DrawTexturedScreenQuad;
begin
  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.LoadIdentity;
  GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;

    // drawing rectangle over screen
    GL.Disable(GL_DEPTH_TEST);
    DrawTexturedScreenQuad3;
    GL.Enable(GL_DEPTH_TEST);

  GL.PopMatrix;
  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;
end;

procedure DrawTexturedScreenQuad2(const ViewPortSize: TVKSize);
begin
  GL.PushMatrix;
  GL.MatrixMode(GL_PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    GL.Ortho(0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1);
    GL.Disable(GL_DEPTH_TEST);
    GL.DepthMask(False);
    GL.Begin_(GL_QUADS);
      GL.TexCoord2f(0.0, ViewPortSize.cy);             GL.Vertex2f(0, 0);
      GL.TexCoord2f(0.0, 0.0);                         GL.Vertex2f(0, ViewPortSize.cy);
      GL.TexCoord2f(ViewPortSize.cx, 0.0);             GL.Vertex2f(ViewPortSize.cx, ViewPortSize.cy);
      GL.TexCoord2f(ViewPortSize.cx, ViewPortSize.cy); GL.Vertex2f(ViewPortSize.cx, 0);
    GL.End_;
    GL.DepthMask(True);
    GL.Enable(GL_DEPTH_TEST);
    GL.MatrixMode(GL_PROJECTION);
    GL.PopMatrix;
  GL.MatrixMode(GL_MODELVIEW);
  GL.PopMatrix;
end;

procedure DrawTexturedScreenQuad4(const ViewPortSize: TVKSize);
begin
  GL.Begin_(GL_QUADS);
    GL.TexCoord2f(0, 0);                             GL.Vertex2f(-1, -1);
    GL.TexCoord2f(ViewPortSize.cx, 0);               GL.Vertex2f( 1, -1);
    GL.TexCoord2f(ViewPortSize.cx, ViewPortSize.cy); GL.Vertex2f( 1,  1);
    GL.TexCoord2f(0, ViewPortSize.cy);               GL.Vertex2f(-1,  1);
  GL.End_;
end;

procedure DrawTexturedScreenQuad5(const ViewPortSize: TVKSize);
begin
  GL.MatrixMode( GL_PROJECTION );
  GL.PushMatrix;
    GL.LoadIdentity;
    GL.Ortho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
      GL.LoadIdentity;
      GL.Disable(GL_DEPTH_TEST);
      GL.DepthMask( FALSE );
      DrawTexturedScreenQuad3;
      GL.DepthMask( TRUE );
      GL.Enable(GL_DEPTH_TEST);
    GL.PopMatrix;
    GL.MatrixMode( GL_PROJECTION );
  GL.PopMatrix;
  GL.MatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad6(const ViewPortSize: TVKSize);
begin
  GL.MatrixMode( GL_PROJECTION );
  GL.PushMatrix;
    GL.LoadIdentity;
    GL.Ortho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    GL.MatrixMode(GL_MODELVIEW);
    GL.PushMatrix;
      GL.LoadIdentity;
      GL.Disable(GL_DEPTH_TEST);
      GL.DepthMask( FALSE );
      DrawTexturedScreenQuad4(ViewPortSize);;
      GL.DepthMask( TRUE );
      GL.Enable(GL_DEPTH_TEST);
    GL.PopMatrix;
    GL.MatrixMode( GL_PROJECTION );
  GL.PopMatrix;
  GL.MatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad3;
begin
  GL.Begin_(GL_QUADS);
    GL.TexCoord2f(0, 0); GL.Vertex2f(-1, -1);
    GL.TexCoord2f(1, 0); GL.Vertex2f(1, -1);
    GL.TexCoord2f(1, 1); GL.Vertex2f(1, 1);
    GL.TexCoord2f(0, 1); GL.Vertex2f(-1, 1);
  GL.End_;
end;

procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TVKSize;
  const TextureTarget: TVKTextureTarget = ttTexture2D);
var
  glTarget: TGLenum;
begin
  with CurrentGLContext.GLStates do
  begin
    TextureBinding[ActiveTexture, TextureTarget] := TextureHandle;
  end;
  glTarget := DecodeGLTextureTarget(TextureTarget);
  GL.TexParameteri(glTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  GL.TexParameteri(glTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  GL.TexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  GL.TexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  GL.CopyTexImage2D(glTarget, 0, GL_RGBA8, 0, 0, TextureSize.cx, TextureSize.cy, 0);
end;

{ TVKShaderProgram }

procedure TVKShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;


procedure TVKShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;

  if (Source is TVKShaderProgram) then
  begin
    FEnabled := TVKShaderProgram(Source).FEnabled;
    FCode.Assign(TVKShaderProgram(Source).FCode);
  end
  else
    inherited; //die, die, die!!!
end;


constructor TVKShaderProgram.Create(const AParent: TVKCustomShader);
begin
  FParent := AParent;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
  FEnabled := False;
end;


destructor TVKShaderProgram.Destroy;
begin
  FCode.Destroy;
end;


function TVKShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TVKShaderProgram.LoadFromFile(const AFileName: string);
begin
  FCode.LoadFromFile(AFileName);
  FEnabled := True;
end;


procedure TVKShaderProgram.OnChangeCode(Sender: TObject);
begin
  FEnabled := True;
  FParent.NotifyChange(self);
end;


procedure TVKShaderProgram.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
  FParent.NotifyChange(self);
end;


procedure TVKShaderProgram.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;
  FEnabled := Value;
  if FEnabled then
    FParent.FinalizeShader;
end;


{ TVKCustomShader }

procedure TVKCustomShader.Assign(Source: TPersistent);
begin
  if Source is TVKCustomShader then
  begin
    FFragmentProgram.Assign(TVKCustomShader(Source).FFragmentProgram);
    FVertexProgram.Assign(TVKCustomShader(Source).FVertexProgram);
    FGeometryProgram.Assign(TVKCustomShader(Source).FGeometryProgram);
    FTagObject := TVKCustomShader(Source).FTagObject;
  end;
  inherited;
end;


constructor TVKCustomShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugMode := False;
  FFragmentProgram := TVKFragmentProgram.Create(Self);
  FVertexProgram := TVKVertexProgram.Create(Self);
  FGeometryProgram := TVKGeometryProgram.Create(Self);
end;


destructor TVKCustomShader.Destroy;
begin
  FFragmentProgram.Destroy;
  FVertexProgram.Destroy;
  FGeometryProgram.Destroy;

  inherited;
end;

procedure TVKCustomShader.LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
begin
  If VPFilename <> '' then VertexProgram.LoadFromFile(VPFilename);
  If FPFilename <> '' then FragmentProgram.LoadFromFile(FPFilename);
  If GPFilename <> '' then GeometryProgram.LoadFromFile(GPFilename);
end;

procedure TVKCustomShader.SetDebugMode(const Value: Boolean);
begin
  if FDebugMode <> Value then
  begin
    FDebugMode := Value;

    if FDebugMode then
      FailedInitAction := fiaReRaiseException
    else
      FailedInitAction := fiaRaiseStandardException;
  end;
end;

procedure TVKCustomShader.SetFragmentProgram(const Value: TVKFragmentProgram);
begin
  FFragmentProgram.Assign(Value);
end;

procedure TVKCustomShader.SetGeometryProgram(const Value: TVKGeometryProgram);
begin
  FGeometryProgram.Assign(Value);
end;

procedure TVKCustomShader.SetVertexProgram(const Value: TVKVertexProgram);
begin
  FVertexProgram.Assign(Value);
end;

function TVKCustomShader.StoreFragmentProgram: Boolean;
begin
  Result := FFragmentProgram.Enabled or (FFragmentProgram.Code.Text <> '')
end;

function TVKCustomShader.StoreGeometryProgram: Boolean;
begin
  Result := FGeometryProgram.Enabled or (FGeometryProgram.Code.Text <> '')
end;

function TVKCustomShader.StoreVertexProgram: Boolean;
begin
  Result := FVertexProgram.Enabled or (FVertexProgram.Code.Text <> '')
end;

{ TVKCustomShaderParameter }

procedure TVKCustomShaderParameter.SetAsTexture(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, Value.TextureHandle.Target, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsTexture1D(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture1D, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsTexture2D(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture2D, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsTexture3D(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture3D, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsTextureCube(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureCube, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsTextureRect(
  const TextureIndex: Integer; const Value: TVKTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureRect, Value.Handle);
end;

procedure TVKCustomShaderParameter.SetAsVectorF(const Values: array of Single);
begin
  case Length(Values) of
    1: SetAsVector1f(Values[0]);
    2: SetAsVector2f(Vector2fMake(Values[0], Values[1]));
    3: SetAsVector3f(Vector3fMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4f(Vector4fMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TVKCustomShaderParameter.SetAsVectorI(const Values: array of Integer);
begin
  case Length(Values) of
    1: SetAsVector1i(Values[0]);
    2: SetAsVector2i(Vector2iMake(Values[0], Values[1]));
    3: SetAsVector3i(Vector3iMake(Values[0], Values[1], Values[2]));
    4: SetAsVector4i(Vector4iMake(Values[0], Values[1], Values[2], Values[3]));
  else
    Assert(False, 'Vector length must be between 1 to 4');
  end;
end;

procedure TVKCustomShaderParameter.SetToTextureOf(
  const LibMaterial: TVKLibMaterial; const TextureIndex: Integer);
begin
  SetToTextureOf(LibMaterial.Material.Texture, TextureIndex);
end;

procedure TVKCustomShaderParameter.SetToTextureOf(
  const Texture: TVKTexture; const TextureIndex: Integer);
begin
  SetAsCustomTexture(TextureIndex, Texture.Image.NativeTextureTarget, Texture.Handle);
end;

constructor TVKGeometryProgram.Create(const AParent: TVKCustomShader);
begin
  inherited Create(AParent);
  FInputPrimitiveType := gsInPoints;
  FOutputPrimitiveType := gsOutPoints;
  FVerticesOut := 0;
end;

procedure TVKGeometryProgram.SetInputPrimitiveType(const Value: TVKgsInTypes);
begin
  if Value <> FInputPrimitiveType then
  begin
    FInputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TVKGeometryProgram.SetOutputPrimitiveType(const Value: TVKgsOutTypes);
begin
  if Value<>FOutputPrimitiveType then
  begin
    FOutputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TVKGeometryProgram.SetVerticesOut(const Value: TGLint);
begin
  if Value<>FVerticesOut then
  begin
    FVerticesOut := Value;
    FParent.NotifyChange(Self);
  end;
end;

initialization
  RegisterClasses([TVKCustomShader, TVKShaderProgram,
                   TVKVertexProgram, TVKFragmentProgram, TVKGeometryProgram]);

end.