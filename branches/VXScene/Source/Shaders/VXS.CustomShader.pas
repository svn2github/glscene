//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    A collection of pure abstract classes - descendants of TVXShader, which are
    used for purpose of not having to write the same stuff all over and over
    again in your own shader classes.
    It also contains a procedures and function that can be used in all shaders. 

      
}
unit VXS.CustomShader;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,
  System.Classes, 
  System.SysUtils,
  
  VXS.VectorGeometry, 
  VXS.VectorTypes, 
  VXS.Texture, 
  VXS.Cadencer, 
  VXS.Scene,
  VXS.Strings, 
  VXS.CrossPlatform, 
  VXS.Context, 
  VXS.RenderContextInfo, 
  VXS.Material,
  VXS.VectorLists, 
  VXS.TextureFormat, 
  VXS.GLSLParameter;

const
  glsShaderMaxLightSources = 8;

type
  TVXShaderFogSupport = (sfsEnabled, sfsDisabled, sfsAuto);
  TVXTransformFeedBackMode = (tfbmInterleaved, tfbmSeparate);

  EGLCustomShaderException = class(EGLShaderException);

  TVXCustomShader = class;
  TVXVertexProgram = class;
  TVXFragmentProgram = class;
  TVXGeometryProgram = class;

  TVXShaderEvent = procedure(Shader: TVXCustomShader) of object;
  TVXShaderUnAplyEvent = procedure(Shader: TVXCustomShader; var ThereAreMorePasses: Boolean) of object;

  TVXLightSourceEnum = 1..glsShaderMaxLightSources;
  TVXLightSourceSet = set of TVXLightSourceEnum;

  { This interface describes user shaders, in order to be able to access them
    via a unified interface. If user shader does not support some option, don't
    raise an axception, just ignore it.
  }
  IGLShaderDescription = interface
  ['{04089C64-60C2-43F5-AC9C-38ED46264812}']
    procedure SetShaderTextures(const Textures: array of TVXTexture);
    procedure GetShaderTextures(var Textures: array of TVXTexture);

    procedure SetShaderColorParams(const AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);
    procedure GetShaderColorParams(var AAmbientColor, ADiffuseColor, ASpecularcolor: TVector4f);

    procedure SetShaderMiscParameters(const ACadencer: TVXCadencer; const AMatLib: TVXMaterialLibrary; const ALightSources: TVXLightSourceSet);
    procedure GetShaderMiscParameters(var ACadencer: TVXCadencer; var AMatLib: TVXMaterialLibrary; var ALightSources: TVXLightSourceSet);

    function GetShaderAlpha: Single;
    procedure SetShaderAlpha(const Value: Single);

    function GetShaderDescription: string;
  end;

  { Used in the TVXPostShaderHolder component. }
  IGLPostShader = interface
  ['{68A62362-AF0A-4CE8-A9E1-714FE02AFA4A}']
    { Called on every pass. }
    procedure DoUseTempTexture(const TempTexture: TVXTextureHandle;
      TextureTarget: TVXTextureTarget);
    { Called to determine if it is compatible. }
    function GetTextureTarget: TVXTextureTarget;
  end;

  { A pure abstract class, must be overriden. }
  TVXCustomShader = class(TVXShader)
  private
    FFragmentProgram: TVXFragmentProgram;
    FVertexProgram: TVXVertexProgram;
    FGeometryProgram: TVXGeometryProgram;

    FTagObject: TObject;
    procedure SetFragmentProgram(const Value: TVXFragmentProgram);
    procedure SetGeometryProgram(const Value: TVXGeometryProgram);
    procedure SetVertexProgram(const Value: TVXVertexProgram);
    function StoreFragmentProgram: Boolean;
    function StoreGeometryProgram: Boolean;
    function StoreVertexProgram: Boolean;
  protected
    FDebugMode: Boolean;
    procedure SetDebugMode(const Value: Boolean); virtual;

    property FragmentProgram: TVXFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
    property VertexProgram: TVXVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    property GeometryProgram: TVXGeometryProgram read FGeometryProgram write SetGeometryProgram stored StoreGeometryProgram;

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
  TVXShaderProgram = class(TPersistent)
  private
    FParent: TVXCustomShader;
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
    constructor Create(const AParent: TVXCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Code: TStrings read FCode write SetCode;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  TVXVertexProgram = class(TVXShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TVXFragmentProgram = class(TVXShaderProgram)
  published
    property Code;
    property Enabled;
  end;

  TVXGeometryProgram = class(TVXShaderProgram)
  private
    FInputPrimitiveType: TVXgsInTypes;
    FOutputPrimitiveType: TVXgsOutTypes;
    FVerticesOut: GLint;
    procedure SetInputPrimitiveType(const Value: TVXgsInTypes);
    procedure SetOutputPrimitiveType(const Value: TVXgsOutTypes);
    procedure SetVerticesOut(const Value: GLint);
  public
    constructor Create(const AParent: TVXCustomShader); override;
  published
    property Code;
    property Enabled;

    property InputPrimitiveType: TVXgsInTypes read FInputPrimitiveType write SetInputPrimitiveType default gsInPoints;
    property OutputPrimitiveType: TVXgsOutTypes read FOutputPrimitiveType write SetOutputPrimitiveType default gsOutPoints;
    property VerticesOut: GLint read FVerticesOut write SetVerticesOut default 0;
  end;

  { Wrapper around a parameter of the main program. }
  TVXCustomShaderParameter = class(TObject)
  private
    
  protected
    
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
      const Value: TVXTexture);
    procedure SetAsTexture1D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTexture2D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTexture3D(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTextureCube(const TextureIndex: Integer;
      const Value: TVXTexture);
    procedure SetAsTextureRect(const TextureIndex: Integer;
      const Value: TVXTexture);

    function GetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVXTextureTarget): Cardinal; virtual; abstract;
    procedure SetAsCustomTexture(const TextureIndex: Integer;
      TextureTarget: TVXTextureTarget; const Value: Cardinal); virtual; abstract;

    function GetAsUniformBuffer: GLenum; virtual; abstract;
    procedure SetAsUniformBuffer(UBO: GLenum); virtual; abstract;
  public
    

    { This overloaded SetAsVector accepts open array as input. e.g.
       SetAsVectorF([0.1, 0.2]). Array length must between 1-4. }
    procedure SetAsVectorF(const Values: array of Single); overload;
    procedure SetAsVectorI(const Values: array of Integer); overload;

    { SetToTextureOf determines texture type on-the-fly.}
    procedure SetToTextureOf(const LibMaterial: TVXLibMaterial; const TextureIndex: Integer); overload;
    procedure SetToTextureOf(const Texture: TVXTexture; const TextureIndex: Integer); overload;

    // friendly properties.
    property AsVector: TVector read GetAsVector4f write SetAsVector4f;
    property AsAffineVector: TAffineVector read GetAsVector3f write SetAsVector3f;

    // Standard types.
    property AsFloat: Single read GetAsVector1f write SetAsVector1f;
    property AsInteger: Integer read GetAsVector1i write SetAsVector1i;

    // Float vector types.
    property AsVector1f: Single    read GetAsVector1f write SetAsVector1f;
    property AsVector2f: TVector2f read GetAsVector2f write SetAsVector2f;
    property AsVector3f: TVector3f read GetAsVector3f write SetAsVector3f;
    property AsVector4f: TVector4f read GetAsVector4f write SetAsVector4f;

    // Integer vector  types.
    property AsVector1i: Integer   read GetAsVector1i write SetAsVector1i;
    property AsVector2i: TVector2i read GetAsVector2i write SetAsVector2i;
    property AsVector3i: TVector3i read GetAsVector3i write SetAsVector3i;
    property AsVector4i: TVector4i read GetAsVector4i write SetAsVector4i;

    // Unsigned integer vector  types.
    property AsVector1ui: GLuint   read GetAsVector1ui write SetAsVector1ui;
    property AsVector2ui: TVector2ui read GetAsVector2ui write SetAsVector2ui;
    property AsVector3ui: TVector3ui read GetAsVector3ui write SetAsVector3ui;
    property AsVector4ui: TVector4ui read GetAsVector4ui write SetAsVector4ui;

    // Matrix Types.
    property AsMatrix2f: TMatrix2f read GetAsMatrix2f write SetAsMatrix2f;
    property AsMatrix3f: TMatrix3f read GetAsMatrix3f write SetAsMatrix3f;
    property AsMatrix4f: TMatrix4f read GetAsMatrix4f write SetAsMatrix4f;

    // Texture Types.
    property AsTexture    [const TextureIndex: Integer]: TVXTexture write SetAsTexture;
    property AsTexture1D  [const TextureIndex: Integer]: TVXTexture write SetAsTexture1D;
    property AsTexture2D  [const TextureIndex: Integer]: TVXTexture write SetAsTexture2D;
    property AsTexture3D  [const TextureIndex: Integer]: TVXTexture write SetAsTexture3D;
    property AsTextureRect[const TextureIndex: Integer]: TVXTexture write SetAsTextureRect;
    property AsTextureCube[const TextureIndex: Integer]: TVXTexture write SetAsTextureCube;

    property AsCustomTexture[const TextureIndex: Integer; TextureTarget: TVXTextureTarget]: Cardinal read GetAsCustomTexture write SetAsCustomTexture;

    property AsUniformBuffer: GLenum read GetAsUniformBuffer write SetAsUniformBuffer;
  end;


  { Adds two more blending modes to standard ones.
    Not sure how to name them or if they should be included in TBlending mode,
    so I created a new type here. }
  TVXBlendingModeEx = (bmxOpaque, bmxTransparency, bmxAdditive,
    bmxAlphaTest50, bmxAlphaTest100, bmxModulate,
    bmxDestColorOne, bmxDestAlphaOne);

// Exported procedures.
procedure ApplyBlendingModeEx(const BlendingMode: TVXBlendingModeEx);
procedure UnApplyBlendingModeEx;
procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TVXSize;
  const TextureTarget: TVXTextureTarget = ttTexture2D);
// Probably need to give them proper names, instead of numbers... 
procedure DrawTexturedScreenQuad;
procedure DrawTexturedScreenQuad2(const ViewPortSize: TVXSize);
procedure DrawTexturedScreenQuad3;
procedure DrawTexturedScreenQuad4(const ViewPortSize: TVXSize);
procedure DrawTexturedScreenQuad5(const ViewPortSize: TVXSize);
procedure DrawTexturedScreenQuad6(const ViewPortSize: TVXSize);

procedure CopyScreentoTexture(const ViewPortSize: TVXSize; const TextureTarget: Word = GL_TEXTURE_2D);
procedure CopyScreentoTexture2(const ViewPortSize: TVXSize; const TextureTarget: Word = GL_TEXTURE_2D);

function IsFogEnabled(const AFogSupportMode: TVXShaderFogSupport; var rci: TVXRenderContextInfo): Boolean;
procedure GetActiveLightsList(const ALightIDs: TIntegerList);

implementation

uses
  VXS.State;

procedure GetActiveLightsList(const ALightIDs: TIntegerList);
var
  I: Integer;
begin
  ALightIDs.Clear;
  with CurrentVXContext.VxStates do
  begin
    for I := 0 to MaxLights - 1 do
    begin
      if LightEnabling[I] then
        ALightIDs.Add(I);
    end;
  end;
end;

function IsFogEnabled(const AFogSupportMode: TVXShaderFogSupport; var rci: TVXRenderContextInfo): Boolean;
begin
  case AFogSupportMode of
    sfsEnabled:  Result := True;
    sfsDisabled: Result := False;
    sfsAuto:     Result := TVXSceneBuffer(rci.buffer).FogEnable;
  else
    Result := False;
    Assert(False, strUnknownType);
  end;
end;

procedure CopyScreentoTexture(const ViewPortSize: TVXSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexSubImage2D(TextureTarget, 0, 0, 0, 0, 0, ViewPortSize.cx, ViewPortSize.cy);
end;

procedure CopyScreentoTexture2(const ViewPortSize: TVXSize; const TextureTarget: Word = GL_TEXTURE_2D);
begin
  glCopyTexImage2D(TextureTarget, 0, GL_RGB, 0, 0, ViewPortSize.cx, ViewPortSize.cy, 0);
end;

procedure ApplyBlendingModeEx(const BlendingMode: TVXBlendingModeEx);
begin
  with CurrentVXContext.VxStates do
  begin
    Enable(stBlend);

    case BlendingMode of
      bmxOpaque: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxTransparency: SetBlendFunc(bfSRCALPHA, bfONEMINUSSRCALPHA);
      bmxAdditive: SetBlendFunc(bfSRCALPHA, bfONE);
      bmxAlphaTest50: SetAlphaFunction(cfGEQUAL, 0.5);
      bmxAlphaTest100: SetAlphaFunction(cfGEQUAL, 1.0);
      bmxModulate: SetBlendFunc(bfDSTCOLOR, bfZERO);
      bmxDestColorOne: SetBlendFunc(bfDSTCOLOR, bfONE);
      bmxDestAlphaOne: SetBlendFunc(bfDSTALPHA, bfONE);
      else
        Assert(False, strErrorEx + strUnknownType);
    end;
  end;
end;

procedure UnApplyBlendingModeEx;
begin
end;

procedure DrawTexturedScreenQuad;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;

    // drawing rectangle over screen
    glDisable(GL_DEPTH_TEST);
    DrawTexturedScreenQuad3;
    glEnable(GL_DEPTH_TEST);

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad2(const ViewPortSize: TVXSize);
begin
  glPushMatrix;
  glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glOrtho(0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(GLboolean(False));
    glBegin(GL_QUADS);
      glTexCoord2f(0.0, ViewPortSize.cy);             glVertex2f(0, 0);
      glTexCoord2f(0.0, 0.0);                         glVertex2f(0, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, 0.0);             glVertex2f(ViewPortSize.cx, ViewPortSize.cy);
      glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f(ViewPortSize.cx, 0);
    glEnd;
    glDepthMask(GLboolean(True));
    glEnable(GL_DEPTH_TEST);
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

procedure DrawTexturedScreenQuad4(const ViewPortSize: TVXSize);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);                             glVertex2f(-1, -1);
    glTexCoord2f(ViewPortSize.cx, 0);               glVertex2f( 1, -1);
    glTexCoord2f(ViewPortSize.cx, ViewPortSize.cy); glVertex2f( 1,  1);
    glTexCoord2f(0, ViewPortSize.cy);               glVertex2f(-1,  1);
  glEnd;
end;

procedure DrawTexturedScreenQuad5(const ViewPortSize: TVXSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask(GLboolean(False));
      DrawTexturedScreenQuad3;
      glDepthMask(GLboolean(True));
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glMatrixMode( GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad6(const ViewPortSize: TVXSize);
begin
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
    glLoadIdentity;
    glOrtho( 0, ViewPortSize.cx, ViewPortSize.cy, 0, 0, 1 );
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glDepthMask(GLboolean(FALSE));
      DrawTexturedScreenQuad4(ViewPortSize);;
      glDepthMask(GLboolean(True));
      glEnable(GL_DEPTH_TEST);
    glPopMatrix;
    glMatrixMode(GL_PROJECTION );
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW );
end;

procedure DrawTexturedScreenQuad3;
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(-1, -1);
    glTexCoord2f(1, 0); glVertex2f(1, -1);
    glTexCoord2f(1, 1); glVertex2f(1, 1);
    glTexCoord2f(0, 1); glVertex2f(-1, 1);
  glEnd;
end;

procedure InitTexture(
  const TextureHandle: Cardinal;
  const TextureSize: TVXSize;
  const TextureTarget: TVXTextureTarget = ttTexture2D);
var
  glTarget: GLEnum;
begin
  with CurrentVXContext.VxStates do
  begin
    TextureBinding[ActiveTexture, TextureTarget] := TextureHandle;
  end;
  glTarget := DecodeTextureTarget(TextureTarget);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(glTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(glTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glCopyTexImage2D(glTarget, 0, GL_RGBA8, 0, 0, TextureSize.cx, TextureSize.cy, 0);
end;

{ TVXShaderProgram }

procedure TVXShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;


procedure TVXShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then
    Exit;

  if (Source is TVXShaderProgram) then
  begin
    FEnabled := TVXShaderProgram(Source).FEnabled;
    FCode.Assign(TVXShaderProgram(Source).FCode);
  end
  else
    inherited; //die, die, die!!!
end;


constructor TVXShaderProgram.Create(const AParent: TVXCustomShader);
begin
  FParent := AParent;
  FCode := TStringList.Create;
  TStringList(FCode).OnChange := OnChangeCode;
  FEnabled := False;
end;


destructor TVXShaderProgram.Destroy;
begin
  FCode.Destroy;
end;


function TVXShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TVXShaderProgram.LoadFromFile(const AFileName: string);
begin
  FCode.LoadFromFile(AFileName);
  FEnabled := True;
end;


procedure TVXShaderProgram.OnChangeCode(Sender: TObject);
begin
  FEnabled := True;
  FParent.NotifyChange(self);
end;


procedure TVXShaderProgram.SetCode(const Value: TStrings);
begin
  FCode.Assign(Value);
  FParent.NotifyChange(self);
end;


procedure TVXShaderProgram.SetEnabled(const Value: Boolean);
begin
  if Value = FEnabled then
    Exit;
  FEnabled := Value;
  if FEnabled then
    FParent.FinalizeShader;
end;


{ TVXCustomShader }

procedure TVXCustomShader.Assign(Source: TPersistent);
begin
  if Source is TVXCustomShader then
  begin
    FFragmentProgram.Assign(TVXCustomShader(Source).FFragmentProgram);
    FVertexProgram.Assign(TVXCustomShader(Source).FVertexProgram);
    FGeometryProgram.Assign(TVXCustomShader(Source).FGeometryProgram);
    FTagObject := TVXCustomShader(Source).FTagObject;
  end;
  inherited;
end;


constructor TVXCustomShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDebugMode := False;
  FFragmentProgram := TVXFragmentProgram.Create(Self);
  FVertexProgram := TVXVertexProgram.Create(Self);
  FGeometryProgram := TVXGeometryProgram.Create(Self);
end;


destructor TVXCustomShader.Destroy;
begin
  FFragmentProgram.Destroy;
  FVertexProgram.Destroy;
  FGeometryProgram.Destroy;

  inherited;
end;

procedure TVXCustomShader.LoadShaderPrograms(const VPFilename, FPFilename: string; GPFilename: string = '');
begin
  If VPFilename <> '' then VertexProgram.LoadFromFile(VPFilename);
  If FPFilename <> '' then FragmentProgram.LoadFromFile(FPFilename);
  If GPFilename <> '' then GeometryProgram.LoadFromFile(GPFilename);
end;

procedure TVXCustomShader.SetDebugMode(const Value: Boolean);
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

procedure TVXCustomShader.SetFragmentProgram(const Value: TVXFragmentProgram);
begin
  FFragmentProgram.Assign(Value);
end;

procedure TVXCustomShader.SetGeometryProgram(const Value: TVXGeometryProgram);
begin
  FGeometryProgram.Assign(Value);
end;

procedure TVXCustomShader.SetVertexProgram(const Value: TVXVertexProgram);
begin
  FVertexProgram.Assign(Value);
end;

function TVXCustomShader.StoreFragmentProgram: Boolean;
begin
  Result := FFragmentProgram.Enabled or (FFragmentProgram.Code.Text <> '')
end;

function TVXCustomShader.StoreGeometryProgram: Boolean;
begin
  Result := FGeometryProgram.Enabled or (FGeometryProgram.Code.Text <> '')
end;

function TVXCustomShader.StoreVertexProgram: Boolean;
begin
  Result := FVertexProgram.Enabled or (FVertexProgram.Code.Text <> '')
end;

{ TVXCustomShaderParameter }

procedure TVXCustomShaderParameter.SetAsTexture(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, Value.TextureHandle.Target, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsTexture1D(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture1D, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsTexture2D(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture2D, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsTexture3D(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTexture3D, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsTextureCube(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureCube, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsTextureRect(
  const TextureIndex: Integer; const Value: TVXTexture);
begin
  SetAsCustomTexture(TextureIndex, ttTextureRect, Value.Handle);
end;

procedure TVXCustomShaderParameter.SetAsVectorF(const Values: array of Single);
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

procedure TVXCustomShaderParameter.SetAsVectorI(const Values: array of Integer);
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

procedure TVXCustomShaderParameter.SetToTextureOf(
  const LibMaterial: TVXLibMaterial; const TextureIndex: Integer);
begin
  SetToTextureOf(LibMaterial.Material.Texture, TextureIndex);
end;

procedure TVXCustomShaderParameter.SetToTextureOf(
  const Texture: TVXTexture; const TextureIndex: Integer);
begin
  SetAsCustomTexture(TextureIndex, Texture.Image.NativeTextureTarget, Texture.Handle);
end;

constructor TVXGeometryProgram.Create(const AParent: TVXCustomShader);
begin
  inherited Create(AParent);
  FInputPrimitiveType := gsInPoints;
  FOutputPrimitiveType := gsOutPoints;
  FVerticesOut := 0;
end;

procedure TVXGeometryProgram.SetInputPrimitiveType(const Value: TVXgsInTypes);
begin
  if Value <> FInputPrimitiveType then
  begin
    FInputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TVXGeometryProgram.SetOutputPrimitiveType(const Value: TVXgsOutTypes);
begin
  if Value<>FOutputPrimitiveType then
  begin
    FOutputPrimitiveType := Value;
    FParent.NotifyChange(Self);
  end;
end;

procedure TVXGeometryProgram.SetVerticesOut(const Value: GLint);
begin
  if Value<>FVerticesOut then
  begin
    FVerticesOut := Value;
    FParent.NotifyChange(Self);
  end;
end;

initialization
  RegisterClasses([TVXCustomShader, TVXShaderProgram,
                   TVXVertexProgram, TVXFragmentProgram, TVXGeometryProgram]);

end.