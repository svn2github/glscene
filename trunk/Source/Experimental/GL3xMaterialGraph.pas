//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xMaterialGraph<p>

   Material Shader Graph Editor <p>

   <b>History : </b><font size=-1><ul>

    <li>24/01/10 - Yar - Creation
 </ul></font>
}

{ TODO: Material custom lighting model}
{ TODO: Material distorsion input}
{ DONE: Add Index to texture coordinates node}
{ DONE: Fix reflection vector node}
{ DONE: Complex Load/Save Material into XML-document}
{ DONE: Basic Load/Save Material into XML-document}
{ DONE: 1 Build Material}
{ TODO: 5 Togle Curved Conections}
{ DONE: Vector Swizzling}
{ DONE: Highligth Conections}
{ DONE: Fix Conections Cooling}
{ DONE: Fix Texture Coords Artifact}

unit GL3xMaterialGraph;

interface

{$I GLScene.inc}

uses
  Classes,
  Graphics,
  Dialogs,
{$IFDEF FPC}
  FileUtil,
  DOM,
  XMLRead,
{$ENDIF}
  // GLScene
  GLCrossPlatform,
  GLSCrossXML,
  OpenGLTokens,
  GLContext,
  GLState,
  GLShaderManager,
  GLVBOManager,
  GLRenderContextInfo,
  VectorTypes,
  VectorGeometry,
  VectorGeometryEXT,
  VectorLists,
  GLTexture,
  GL3xTexture,
  GL3xMaterial,
  GL3xMaterialConst,
  GLSGraphStructure,
  GLStrings;

type

  TCustomMaterialGraphNode = class;
  TCustomMaterialGraphNodeClass = class of TCustomMaterialGraphNode;

  TGL3xEdinableMaterial = class(TGL3xMaterial)
  public
    property Document;
    property SampleList;
    property BlendingMode;
    property FaceCulling;
    property PolygonMode;
    property TextureUnits;
  end;

  TTextureRec = record
    Name: TGL3xTextureName;
    UseCount: Integer;
  end;

  TMaterialGraph = class(TBaseGraphStructure)
  private
    FRenderContextInfo: TRenderContextInfo;

    FGraphicInitialized: Boolean;
    FBackTexture: TGLTextureHandle;
    FFontTexture: TGLTextureHandle;
    FFBO: TGLFramebufferHandle;

    FMaxCharWidth, FMaxCharHeight: Integer;
    FJointPositionList: TSingleList;
    FJointColorList: TSingleList;
    FLinksCoordsList: TSingleList;
    FLinksColorList: TSingleList;

    FNodeCounter: Integer;

    FErrorMessage: string;

    FShaderCodePad: TStrings;
    FVertexProgram: TStringList;
    FFragmentProgram: TStringList;
    FGraphChanged: Boolean;
    FMaterial: TGL3xEdinableMaterial;
    FTextures: array[0..MAX_HARDWARE_TEXTURE_UNIT - 1] of TTextureRec;

    function InitGraphics: Boolean;
    procedure CreateFontTexture;
    procedure CreateBackgroundTexture;
    procedure SetErrorMessage(const AMsg: string);
    procedure SetShaderCodePad(AList: TStrings);
    procedure RefreshMaterial;
    procedure ClearTextureRegistry;
    procedure RegisterTexture(const AName: TGL3xTextureName);
    procedure UnRegisterTexture(const AName: TGL3xTextureName);
    function GetTextureUnitIndex(const AName: TGL3xTextureName): Integer;
  protected
    function GetMaterialNode(I: Integer): TCustomMaterialGraphNode; overload;
    function GetSelectedJointNode1: TCustomMaterialGraphNode; inline;
    function GetSelectedJointNode2: TCustomMaterialGraphNode; inline;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddNode(const VertexClass: TCustomMaterialGraphNodeClass; const x, y, w, h:
      Integer): TCustomMaterialGraphNode; overload;

    procedure Save; override;
    procedure Load; override;
    procedure Render; override;
    procedure Refresh;

    property Node[I: Integer]: TCustomMaterialGraphNode read GetMaterialNode;

    property Material: TGL3xEdinableMaterial read FMaterial;
    property ShaderCodePad: TStrings read FShaderCodePad write SetShaderCodePad;
  end;

  TNodeSize = (sideInput, sideOutput);

  PNodeJoint = ^TNodeJoint;
  TNodeJoint = {$IFNDEF FPC}record {$ELSE}object{$ENDIF}
    Step: SmallInt;
    Side: TNodeSize;
    Mask: TGLColorComponentMask;
    Caption: AnsiString;
    GivingNode: TCustomMaterialGraphNode;
    GivingNodeJointIndex: Integer;
    SampleCache: PShaderSample;
    procedure SetAtOnce(AStep: SmallInt; ASide: TNodeSize;
      AMask: TGLColorComponentMask; const ACaption: AnsiString); inline;
    function Color: TVector;
  end;

  TSampleGatherInfo = record
    SampleList: TList;
    PassTexCoord: Boolean;
    LightingInTextureSpace: Boolean;
  end;

  TCustomMaterialGraphNode = class(TBaseGraphVertex)
  private
    { Private declarations }
    FJoints: array of TNodeJoint;
    FViewingTexture: TGLTextureHandle;
    FViewingObject: string;
    FViewingProgram: string;
    FViewingProgLinked: Boolean;
    FPrimarySampleChache: PShaderSample;
    FGatherLock: Boolean;
    FGetOuptupSampleLock: Boolean;
    function GetGraph: TMaterialGraph; inline;
    function GetJoints(I: Integer): PNodeJoint;
    procedure SetJoints(I: Integer; Value: PNodeJoint);
    procedure ClearSampleCache;
  protected
    { Protected declarations }
    class function ValueToHex(const Val): string;
    function GetJointCoords(I: Integer): TVector2s;
    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
    function GetCount: Integer; override;

    procedure RenderFrame;
    procedure RenderText;
    procedure RenderJoint;
    procedure RenderViewing;
    procedure CheckNodeInput(JointIndex: SmallInt; const AMessage: string; var Flag: Boolean);
    function CheckInput: Boolean; virtual; abstract;
    procedure CheckOutput(Flag: Boolean);
    function Caption: string; virtual; abstract;
    function GetViewingCode: AnsiString; virtual;
    procedure SetViewingUniforms; virtual;
    function GetGivingNodeTextureID(JointIndex: SmallInt): TGLuint;
    function GetGivingNodeMask(JointIndex: SmallInt): TGLColorComponentMask;
    function GetGivingNodeDataType(JointIndex: SmallInt): TGLSLDataType;
    function GetOutputSample(out ASample: TShaderSample): Boolean; virtual; abstract;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; virtual;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; virtual;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); virtual;

    procedure SetLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer); override;
    function IsJointLinkedWith(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean; override;
    function IsJointCanLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean; override;
    function IsStartLinkNode(jointIndex: Integer): Boolean; override;
  public
    { Public declarations }
    constructor Create; override;
    destructor Destroy; override;
    procedure NotifyChange; override;
    procedure UpdateViewing;
    function GatherSamples(var Info: TSampleGatherInfo; JointIndex: Integer): Boolean;

    property Owner: TMaterialGraph read GetGraph;
    property Left;
    property Top;
    property Width;
    property Height;
    property Joints[Index: Integer]: PNodeJoint read GetJoints write SetJoints;
    property JointCoords[Index: Integer]: TVector2s read GetJointCoords;

    property ViewingObjectName: string read FViewingObject;
    property GivingNodeTextureID[JointIndex: SmallInt]: TGLuint read GetGivingNodeTextureID;
    property GivingNodeMask[JointIndex: SmallInt]: TGLColorComponentMask read GetGivingNodeMask;
    property GivingNodeDataType[JointIndex: SmallInt]: TGLSLDataType read GetGivingNodeDataType;
  end;

  TLightingModel = (lmPhong);

  TMaterialNode = class(TCustomMaterialGraphNode)
  private
    FLightingModel: TLightingModel;
    FOpacityMaskClip: TVectorEXT;
    function GetFaceCulling: TFaceCulling;
    procedure SetFaceCulling(Value: TFaceCulling);
    function GetPolygonMode: TPolygonMode;
    procedure SetPolygonMode(Value: TPolygonMode);
    function GetBlendingMode: TBlendingMode;
    procedure SetBlendingMode(Value: TBlendingMode);
    function GetOpacityMaskClip: Single;
    procedure SetOpacityMaskClip(Value: Single);
    procedure SetLightingModel(Value: TLightingModel);
  protected
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;

    procedure SetViewingUniforms; override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property FaceCulling: TFaceCulling read GetFaceCulling write SetFaceCulling
      default fcBufferDefault;
    property PolygonMode: TPolygonMode read GetPolygonMode write SetPolygonMode
      default pmFill;
    property BlendingMode: TBlendingMode read GetBlendingMode write SetBlendingMode
      default bmOpaque;
    property OpacityMaskClipValue: Single read GetOpacityMaskClip write SetOpacityMaskClip;
    property LightingModel: TLightingModel read FLightingModel write SetLightingModel
      default lmPhong;
  end;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Constant Nodes'}{$ENDIF}
  TCustomConstantNode = class(TCustomMaterialGraphNode)
  protected
    FValue: TVector4fEXT;
    procedure SetValue(Index: Integer; Value: Single);
    function GetValue(Index: Integer): Single;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TConstantNode = class(TCustomConstantNode)
  published
    property R: Single index 0 read GetValue write SetValue;
  end;

  TConstant2fNode = class(TCustomConstantNode)
  published
    property R: Single index 0 read GetValue write SetValue;
    property G: Single index 1 read GetValue write SetValue;
  end;

  TConstant3fNode = class(TCustomConstantNode)
  published
    property R: Single index 0 read GetValue write SetValue;
    property G: Single index 1 read GetValue write SetValue;
    property B: Single index 2 read GetValue write SetValue;
  end;

  TConstant4fNode = class(TCustomConstantNode)
  published
    property R: Single index 0 read GetValue write SetValue;
    property G: Single index 1 read GetValue write SetValue;
    property B: Single index 2 read GetValue write SetValue;
    property A: Single index 3 read GetValue write SetValue;
  end;

  TVertexColorNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Coordinate Nodes'}{$ENDIF}
  TTextureCoordinateNode = class(TCustomMaterialGraphNode)
  private
    FValue: TVector2fEXT;
    FIndex: Integer;
    procedure SetValue(Index: Integer; Value: Single);
    function GetValue(Index: Integer): Single;
    procedure SetIndex(Value: Integer);
  protected
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property ChannelIndex: Integer read FIndex write SetIndex;
    property TilingS: Single index 0 read GetValue write SetValue;
    property TilingT: Single index 1 read GetValue write SetValue;
  end;

  TPannerNode = class(TCustomMaterialGraphNode)
  private
    FValue: TVector2fEXT;
    procedure SetValue(Index: Integer; Value: Single);
    function GetValue(Index: Integer): Single;
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property SpeedS: Single index 0 read GetValue write SetValue;
    property SpeedT: Single index 1 read GetValue write SetValue;
  end;

  TRotatorNode = class(TCustomMaterialGraphNode)
  private
    FValue: TVector3fEXT;
    procedure SetValue(Index: Integer; Value: Single);
    function GetValue(Index: Integer): Single;
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property CenterX: Single index 0 read GetValue write SetValue;
    property CenterY: Single index 1 read GetValue write SetValue;
    property Speed: Single index 2 read GetValue write SetValue;
  end;

  TScreenPositionNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Math Nodes'}{$ENDIF}
  TUnarOpMathNode = class(TCustomMaterialGraphNode)
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
  end;

  TBinarOpMathNode = class(TCustomMaterialGraphNode)
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
  end;

  TAddNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TSubtractNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TMultiplyNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TDivideNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TNormalizeNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TPowerNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TDotProductNode = class(TBinarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TCustomTrigonNode = class(TCustomMaterialGraphNode)
  private
    FPeriod: Single;
    procedure SetPeriod(Value: Single);
  protected
    function CheckInput: Boolean; override;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
    procedure SetViewingUniforms; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
  public
    constructor Create; override;
  published
    property Period: Single read FPeriod write SetPeriod;
  end;

  TSineNode = class(TCustomTrigonNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TCosineNode = class(TCustomTrigonNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TFloorNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TAbsNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TFractNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TOneMinusNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

  TSquareRootNode = class(TUnarOpMathNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Texture Nodes'}{$ENDIF}
  TTextureSamplerNode = class(TCustomMaterialGraphNode)
  private
    FTexture: TGL3xTextureName;
    FSampler: TGLSamplerParam;
    procedure SetTexture(const AName: TGL3xTextureName);
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property Texture: TGL3xTextureName read FTexture write SetTexture stored False;
    property Sampler: TGLSamplerParam read FSampler;
  end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Utility Nodes'}{$ENDIF}
  TTimerNode = class(TCustomMaterialGraphNode)
  protected
    procedure SetViewingUniforms; override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
  end;

  TComponentMaskNode = class(TCustomMaterialGraphNode)
  private
    FMask: TGLColorComponentMask;
    function GetMask(Index: Integer): Boolean;
    procedure SetMask(Index: Integer; Value: Boolean);
  protected
    function CheckInput: Boolean; override;
    procedure SetViewingUniforms; override;
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
    function DoGatherSamples(var Info: TSampleGatherInfo): Boolean; override;
    function GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean; override;
    procedure SetProperty(Index: Integer; const APropName: string; const APropValue: string); override;
  public
    constructor Create; override;
    function Caption: string; override;
    function GetViewingCode: AnsiString; override;
  published
    property Red: Boolean index 0 read GetMask write SetMask default True;
    property Green: Boolean index 1 read GetMask write SetMask default True;
    property Blue: Boolean index 2 read GetMask write SetMask default False;
    property Alpha: Boolean index 3 read GetMask write SetMask default False;
  end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Vectors Nodes'}{$ENDIF}
  TWorldNormalNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
  published
  end;

  TLightVectorNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
  published
  end;

  TCameraVectorNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
  published
  end;

  TReflectionVectorNode = class(TCustomMaterialGraphNode)
  protected
    function GetOutputSample(out ASample: TShaderSample): Boolean; override;
  public
    constructor Create; override;
    function Caption: string; override;
  published
  end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

implementation

uses
{$IFDEF FPC}
  LResources,
  GraphType,
{$ENDIF}
  SysUtils,
  GLColor,
  GLCompositeImage,
  GLTextureFormat,
  GLSpecializedUniforms;

const

  cNodeCaptionHeigth = 24;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders source'}{$ENDIF}

  Background_vp150: AnsiString =
    '#version 150' + #13#10 +
    'uniform vec4 ScaleOffset;' + #13#10 +
    'in vec2 Position;' + #13#10 +
    'in vec2 TexCoord0;' + #13#10 +
    'out float mixFactor;' + #13#10 +
    'out vec2 fpTexCoord;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  vec2 screen_pos = 2.0*sign(Position)-1.0;' + #13#10 +
    '  mixFactor = sign(Position.y);' + #13#10 +
    '  fpTexCoord = TexCoord0*ScaleOffset.xy+ScaleOffset.zw;' + #13#10 +
    '  gl_Position = vec4(screen_pos, 0.0, 1.0);' + #13#10 +
    '}';

  Background_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'uniform sampler2D TexUnit0;' + #13#10 +
    'uniform vec4 GradientColor1;' + #13#10 +
    'uniform vec4 GradientColor2;' + #13#10 +
    'in float mixFactor;' + #13#10 +
    'in vec2 fpTexCoord;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  vec4 gradient = mix(GradientColor1, GradientColor2, mixFactor);' + #13#10
    +
    '  gradient = (gradient - 0.5)*0.5;' + #13#10 +
    '  FragColor = texture(TexUnit0, fpTexCoord).rgba + gradient;' + #13#10 +
    '}';

  Frame_vp150: AnsiString =
    '#version 150' + #13#10 +
    'in vec2 Position;' + #13#10 +
    'in vec2 TexCoord0;' + #13#10 +
    'uniform vec4 ScaleOffset;' + #13#10 +
    'out vec2 fpTexCoord;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  fpTexCoord = TexCoord0;' + #13#10 +
    '  vec2 screen_pos = Position*ScaleOffset.xy+ScaleOffset.zw;' + #13#10 +
    '  gl_Position = vec4(screen_pos, 0.0, 1.0);' + #13#10 +
    '}';

  Frame_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'uniform vec4 Diffuse;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  FragColor = Diffuse;' + #13#10 +
    '}';

  Viewing_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'in vec2 fpTexCoord;' + #13#10 +
    'uniform sampler2D TexUnit0;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  FragColor = texture(TexUnit0, fpTexCoord);' + #13#10 +
    '}';

  Joint_vp150: AnsiString =
    '#version 150' + #13#10 +
    'in vec2 Position;' + #13#10 +
    'in vec4 VertexColor;' + #13#10 +
    'out vec4 fpColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  fpColor = VertexColor;' + #13#10 +
    '  gl_Position = vec4(Position, 0.0, 1.0);' + #13#10 +
    '}';

  Joint_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'in vec4 fpColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  FragColor = fpColor;' + #13#10 +
    '}';

  Link_vp150: AnsiString =
    '#version 150' + #13#10 +
    'in vec2 Position;' + #13#10 +
    'in vec4 VertexColor;' + #13#10 +
    'out vec4 gpColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  gpColor = VertexColor;' + #13#10 +
    '  gl_Position = vec4(Position, 0.0, 1.0);' + #13#10 +
    '}';

  Link_gp150: AnsiString =
    '#version 150' + #13#10 +
    '#extension GL_EXT_gpu_shader4 : enable' + #13#10 +
    '#extension GL_EXT_geometry_shader4 : enable' + #13#10 +
    'layout(lines_adjacency) in;' + #13#10 +
    'layout(line_strip, max_vertices = 30) out;' + #13#10 +
    'precision highp float;' + #13#10 +
    'precision lowp int;' + #13#10 +
    'const int segments = 20;' + #13#10 +
    'uniform vec2 ArrowSize;' + #13#10 +
    'in vec4 gpColor[4];' + #13#10 +
    'out vec4 Diffuse;' + #13#10 +
    'vec4 evaluateBezierPosition(float t)' + #13#10 +
    '{' + #13#10 +
    '  float omt = 1.0 - t;' + #13#10 +
    '  vec4 b;' + #13#10 +
    '  b.x = omt*omt*omt;' + #13#10 +
    '  b.y = 3.0*t*omt*omt;' + #13#10 +
    '  b.z = 3.0*t*t*omt;' + #13#10 +
    '  b.w = t*t*t;' + #13#10 +
    '  vec2 node = gl_PositionIn[0].xy*b.x +' + #13#10 +
    '              gl_PositionIn[1].xy*b.y +' + #13#10 +
    '              gl_PositionIn[2].xy*b.z +' + #13#10 +
    '              gl_PositionIn[3].xy*b.w;' + #13#10 +
    '  return vec4(node, 0.0, 1.0);' + #13#10 +
    '}' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  vec4 p = vec4(0.0);' + #13#10 +
    '  Diffuse = gpColor[0];' + #13#10 +
    '  for(int i=0; i<segments; i++)' + #13#10 +
    '  {' + #13#10 +
    '    float t = i / float(segments-1);' + #13#10 +
    '    p = evaluateBezierPosition(t);' + #13#10 +
    '    gl_Position = p;' + #13#10 +
    '    EmitVertex();' + #13#10 +
    '  }' + #13#10 +
    '  EndPrimitive();' + #13#10 +
    '  gl_Position = p;' + #13#10 +
    '  EmitVertex();' + #13#10 +
    '  float dir = sign(gl_PositionIn[2].x - gl_PositionIn[3].x);' + #13#10 +
    '  gl_Position = vec4(p.xy+vec2(dir*ArrowSize.x, -ArrowSize.y), 0.0, 1.0);' + #13#10
    +
    '  EmitVertex();' + #13#10 +
    '  gl_Position = vec4(p.xy+vec2(dir*ArrowSize.x, ArrowSize.y), 0.0, 1.0);' + #13#10
    +
    '  EmitVertex();' + #13#10 +
    '  gl_Position = p;' + #13#10 +
    '  EmitVertex();' + #13#10 +
    '  EndPrimitive();' + #13#10 +
    '}';

  Link_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'in vec4 Diffuse;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main()' + #13#10 +
    '{' + #13#10 +
    '  FragColor = Diffuse;' + #13#10 +
    '}';

  Text_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'precision mediump int;' + #13#10 +
    '#extension GL_ARB_texture_rectangle : enable' + #13#10 +
    'uniform sampler2DRect TexUnit0;' + #13#10 +
    'uniform int[128] String;' + #13#10 +
    'uniform int Length;' + #13#10 +
    'uniform vec4 RectSize;' + #13#10 +
    'uniform ivec2 CharSize;' + #13#10 +
    'in vec2 fpTexCoord;' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main()' + #13#10 +
    '{' + #13#10 +
    '  ivec2 pos = ivec2(fpTexCoord * RectSize.xy + RectSize.zw);' + #13#10 +
    '  ivec2 texelPos = pos % CharSize;' + #13#10 +
    '  ivec2 delta = ivec2(step(vec2(pos), vec2(0.0)));' + #13#10 +
    '  ivec2 charPos  = pos / CharSize - delta;' + #13#10 +
    '  int CharNum = charPos.y*(int(RectSize.x) / CharSize.x)+charPos.x;' + #13#10
    +
    '  if ((CharNum < 0) || (CharNum >= Length) || (CharNum >= 128)) discard;' + #13#10
    +
    '  int Char = String[CharNum];' + #13#10 +
    '  charPos = ivec2((Char & 0x0F)*CharSize.x, ((Char & 0xF0) >> 4)*CharSize.y) + texelPos;' + #13#10
    +
    '  vec3 color = texture(TexUnit0, charPos).rgb;' + #13#10 +
    '  if (color.r+color.g+color.b == 0.0) discard;' + #13#10 +
    '  FragColor = vec4(color, 1.0);' + #13#10 +
    '}';

  ViewingMaker_vp150: AnsiString =
    '#version 150' + #13#10 +
    'in vec2 Position;' + #13#10 +
    'in vec2 TexCoord0;' + #13#10 +
    'out vec2 fpTexCoord;' + #13#10 +
    'void main()' + #13#10 +
    '{' + #13#10 +
    '  fpTexCoord = TexCoord0;' + #13#10 +
    '  gl_Position = vec4(Position, 0.0, 1.0);' + #13#10 +
    '}';

  UniformSampler_line: AnsiString =
    'uniform sampler2D TexUnit0;' + #13#10 +
    'uniform sampler2D TexUnit1;' + #13#10;
  GetTexCoord_line: AnsiString =
    'vec2 GetTexCoord(void);' + #13#10;
  GetWorkResult_line: AnsiString =
    'vec4 GetWorkResult(void)' + #13#10;
  GetValue0_line: AnsiString =
    ' vec4 value0 = texture(TexUnit0, GetTexCoord());' + #10#13;
  GetValue1_line: AnsiString =
    ' vec4 value1 = texture(TexUnit1, GetTexCoord());' + #10#13;
  LBracket_line: AnsiString =
    '{' + #10#13;
  RBracket_line: AnsiString =
    '}' + #10#13;

  ViewingMaker_fp150: AnsiString =
    '#version 150' + #13#10 +
    'precision highp float;' + #13#10 +
    'in vec2 fpTexCoord;' + #13#10 +
    'vec4 GetWorkResult(void);' + #13#10 +
    'vec2 GetTexCoord(void) { return (fpTexCoord); }' + #13#10 +
    'out vec4 FragColor;' + #13#10 +
    'void main(void)' + #13#10 +
    '{' + #13#10 +
    '  FragColor = GetWorkResult();' + #13#10 +
    '}';

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

var
  uniformGradientColor1,
    uniformGradientColor2,
    uniformScaleOffset,
    uniformArrowSize,
    uniformLength,
    uniformRectSize,
    uniformCharSize,
    uniformString,
    uniformTime: TGLSLUniform;

  gvarSample: PShaderSample;
  gvarProgType: TGLSLProgramTypes;

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TMaterialGraph'}{$ENDIF}

constructor TMaterialGraph.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodeList := TList.Create;
  FMaterial := TGL3xEdinableMaterial.Create(Self);
  FNeedPackList := false;
  FGraphicInitialized := false;
  FPulling := false;
  FGraphChanged := True;
  FVertexProgram := TStringList.Create;
  FFragmentProgram := TStringList.Create;

  FScreenCenter[0] := 0;
  FScreenCenter[1] := 0;
  FScreenZoom := 1;

  FNodeCounter := 0;

  FJointPositionList := TSingleList.Create;
  FJointColorList := TSingleList.Create;
  FLinksCoordsList := TSingleList.Create;
  FLinksColorList := TSingleList.Create;

  FillChar(FRenderContextInfo, SizeOf(FRenderContextInfo), 0);
end;

destructor TMaterialGraph.Destroy;
var
  n: Integer;
begin
  PackLists;
  for n := 0 to FNodeList.Count - 1 do
    Node[n].Free;

  FNodeList.Free;
  FJointPositionList.Free;
  FJointColorList.Free;
  FLinksCoordsList.Free;
  FLinksColorList.Free;
  FMaterial.Free;
  ClearTextureRegistry;

  // Release GPU resources
  if FGraphicInitialized then
  begin
    with ShaderManager do
    begin
      try
        BeginWork;
        ClearShaderObject;
        ClearShaderPrograms;
      finally
        EndWork;
      end;
      FFBO.Destroy;
    end;
    FBackTexture.Destroy;
    FFontTexture.Destroy;
  end;
  inherited;
end;

procedure TMaterialGraph.ClearTextureRegistry;
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
  begin
    FTextures[I].Name := '';
    FTextures[I].UseCount := 0;
  end;
end;

function TMaterialGraph.GetMaterialNode(I: Integer): TCustomMaterialGraphNode;
begin
  Result := TCustomMaterialGraphNode(GetNode(i));
end;

function TMaterialGraph.GetSelectedJointNode1: TCustomMaterialGraphNode;
begin
  Result := TCustomMaterialGraphNode(FSelectedJointNode1);
end;

function TMaterialGraph.GetSelectedJointNode2: TCustomMaterialGraphNode;
begin
  Result := TCustomMaterialGraphNode(FSelectedJointNode2);
end;

procedure TMaterialGraph.Refresh;
var
  n: Integer;
begin
  for n := 0 to FNodeList.Count - 1 do
    Node[n].NotifyChange;
end;

procedure TMaterialGraph.Save;
var
  XMLMaterial, XMLGraph, XMLNode, XMLNodeProp, XMLJoint: GLSDOMNode;
  vNode: TCustomMaterialGraphNode;
  i, j: Integer;
  bDone: Boolean;
  vName, vValue: string;
begin
  FMaterial.Document := GLSNewXMLDocument;
{$IFDEF FPC}
  XMLMaterial := FMaterial.Document.CreateElement('TGL3xMaterial');
  FMaterial.Document.AppendChild(XMLMaterial);
{$ELSE}
  XMLMaterial := FMaterial.Document.DOMDocument.CreateElement('TGL3xMaterial');
  FMaterial.Document.DOMDocument.AppendChild(XMLMaterial);
{$ENDIF}
  // Save textures
  vValue := '';
  for I := 0 to High(FTextures) do
    if FTextures[I].UseCount > 0 then
      vValue := vValue + FTextures[I].Name + ';'
    else
      break;
  SetXMLAttribute(XMLMaterial, 'TextureUnits', vValue);

  XMLGraph := CreateDOMNode(XMLMaterial, 'MaterialGraph');

  for i := 0 to FNodeList.Count - 1 do
  begin
    vNode := Node[i];
    XMLNode := CreateDOMNode(XMLGraph, 'MaterialNode');
    SetXMLAttribute(XMLNode, 'NodeClass', vNode.ClassName);
    SetXMLAttribute(XMLNode, 'Left', IntToStr(vNode.Left));
    SetXMLAttribute(XMLNode, 'Top', IntToStr(vNode.Top));
    SetXMLAttribute(XMLNode, 'Width', IntToStr(vNode.Width));
    SetXMLAttribute(XMLNode, 'Height', IntToStr(vNode.Height));

    // Node published properties
    XMLNodeProp := CreateDOMNode(XMLNode, 'Properties');
    j := 0;
    repeat
      bDone := vNode.GetProperty(j, vName, vValue);
      Inc(j);
      if bDone then
        SetXMLAttribute(XMLNodeProp, vName, vValue);
    until not bDone;

    for j := 0 to vNode.Count - 1 do
    begin
      if Assigned(vNode.Joints[j].GivingNode) then
      begin
        XMLJoint := CreateDOMNode(XMLNode, 'NodeJoint');
        SetXMLAttribute(XMLJoint, 'JointIndex', IntToStr(j));
        SetXMLAttribute(XMLJoint, 'GivingNodeIntex',
          IntToStr(FNodeList.IndexOf(vNode.Joints[j].GivingNode)));
        SetXMLAttribute(XMLJoint, 'GivingNodeJointIntex',
          IntToStr(vNode.Joints[j].GivingNodeJointIndex));
      end;
    end;
  end;
end;

procedure TMaterialGraph.Load;
var
  XMLMaterial, XMLGraph, XMLNode, XMLJoint, XMLNodeProp: GLSXMLNode;
  OldNodeList: TList;
  I, J, P: Integer;
  NodeClass: TCustomMaterialGraphNodeClass;
  vNode: TCustomMaterialGraphNode;
  Success: Boolean;
  x, y, w, h, jointIndex, givingNodeIndex, err: Integer;
  temp: string;
begin
  if FMaterial.Document = nil then
    FMaterial.Default;

  OldNodeList := FNodeList;
  FNodeList := TList.Create;
  Success := False;
  try
    XMLMaterial := FMaterial.Document.DocumentElement;
    ClearTextureRegistry;
    GetXMLAttribute(XMLMaterial, 'TextureUnits', temp);
    repeat
      P := Pos(';', temp);
      if P = 0 then
        break;
      RegisterTexture(Copy(temp, 1, P - 1));
      Delete(temp, 1, P);
    until Length(temp) = 0;

    if FindXMLNode(XMLMaterial, 'MaterialGraph', XMLGraph) then
    begin
      // Recreate node
      for i := 0 to XMLGraph.ChildNodes.Count - 1 do
      begin
        XMLNode := XMLGraph.ChildNodes[i];
        if CompareText(XMLNode.NodeName, 'MaterialNode') = 0 then
        begin
          GetXMLAttribute(XMLNode, 'NodeClass', temp);
          NodeClass := TCustomMaterialGraphNodeClass(FindClass(temp));
          if NodeClass <> nil then
          begin
            GetXMLAttribute(XMLNode, 'Left', temp);            Val(temp, x, err);
            GetXMLAttribute(XMLNode, 'Top', temp);             Val(temp, y, err);
            GetXMLAttribute(XMLNode, 'Width', temp);           Val(temp, w, err);
            GetXMLAttribute(XMLNode, 'Height', temp);          Val(temp, h, err);
            vNode := AddNode(NodeClass, x, y, w, h);
            if FindXMLNode(XMLNode, 'Properties', XMLNodeProp) then
              for j := 0 to GetXMLAttributeCount(XMLNodeProp) - 1 do
                with GetXMLAttribute(XMLNodeProp, j) do
                  vNode.SetProperty(j, NodeName, NodeValue);
          end;
        end;
      end;
      // Recreate node links
      for i := 0 to XMLGraph.ChildNodes.Count - 1 do
      begin
        XMLNode := XMLGraph.ChildNodes[i];
        if (CompareText(XMLNode.NodeName, 'MaterialNode') = 0)
          and XMLNode.HasChildNodes then
        begin
          for j := 0 to XMLNode.ChildNodes.Count - 1 do
          begin
            XMLJoint := XMLNode.ChildNodes[j];
            if CompareText(XMLJoint.NodeName, 'NodeJoint') = 0 then
            begin
              GetXMLAttribute(XMLJoint, 'JointIndex', temp);
              Val(temp, jointIndex, err);
              GetXMLAttribute(XMLJoint, 'GivingNodeIntex', temp);
              Val(temp, givingNodeIndex, err);
              Node[i].Joints[jointIndex].GivingNode := Node[givingNodeIndex];
              with Node[i] do
              begin
                Joints[jointIndex].GivingNode := Node[givingNodeIndex];
                GetXMLAttribute(XMLJoint, 'GivingNodeJointIntex', temp);
                Val(temp, givingNodeIndex, err);
                Joints[jointIndex].GivingNodeJointIndex := givingNodeIndex;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      {: Force-major - material is empty :O
         So create new }
      AddNode(TMaterialNode, FScreenCenter[0] - 50, FScreenCenter[1] - 50, 100, 150);
    end;
    Success := True;
  finally
    if Success then
    begin
      for i := 0 to OldNodeList.Count - 1 do
      begin
        vNode := TCustomMaterialGraphNode(OldNodeList[i]);
        vNode.Free;
      end;
      OldNodeList.Free;
    end
    else
    begin
      for i := 0 to FNodeList.Count - 1 do
        Node[i].Free;
      FNodeList.Free;
      FNodeList := OldNodeList;
    end;
  end;

  vNode := Node[0];
  // Checking, just in case
  if not (vNode is TMaterialNode) then
    for I := 1 to FNodeList.Count - 1 do
    begin
      vNode := Node[I];
      if vNode is TMaterialNode then
        break;
    end;
  FScreenCenter := Vector2sMake(vNode.Left + vNode.Width div 2, vNode.Top + vNode.Height div 2);
  FScreenZoom := 1;
  Refresh;
end;

procedure TMaterialGraph.CreateFontTexture;
var
  i, j, size: Integer;
  bmp: TGLBitmap;
{$IFNDEF FPC}
  data, ptrSrc, ptrDst: PByte;
{$ENDIF}
  vNode: TCustomMaterialGraphNode;
begin
  // Find max sizes
  FMaxCharWidth := 0;
  FMaxCharHeight := 0;
  bmp := TGLBitmap.Create;
  bmp.Canvas.Font.Size := 8;
  bmp.Canvas.Font.Name := 'Fixedsys';
  for i := 0 to 255 do
  begin
    size := bmp.Canvas.TextExtent(chr(i)).cy;
    if size > FMaxCharHeight then
      FMaxCharHeight := size;
    size := bmp.Canvas.TextExtent(chr(i)).cx;
    if size > FMaxCharWidth then
      FMaxCharWidth := size;
  end;
  FMaxCharWidth := FMaxCharWidth{$IFDEF LINUX} div 2{$ENDIF} + 2;
  FMaxCharHeight := FMaxCharHeight + 2;

  bmp.Width := FMaxCharWidth * 16;
  bmp.Height := FMaxCharHeight * 16;
  bmp.Canvas.Pen.Color := $00000000;
  bmp.Canvas.Brush.Color := $00000000;
  bmp.Canvas.Rectangle(bmp.Canvas.ClipRect);
  bmp.PixelFormat := glpf24bit;
  bmp.Canvas.Brush.Style := bsClear;

  // Draw font to bitmap
  bmp.Canvas.Font.Color := ConvertColorVector(clrDimGray);
  for i := 0 to 15 do
    for j := 0 to 15 do
      bmp.Canvas.TextOut(j * FMaxCharWidth + 1, i * FMaxCharHeight + 1,
        chr(i * 16 + j));
  bmp.Canvas.Font.Color := ConvertColorVector(clrYellow);
  for i := 0 to 15 do
    for j := 0 to 15 do
      bmp.Canvas.TextOut(j * FMaxCharWidth, i * FMaxCharHeight, chr(i * 16 + j));

{$IFNDEF FPC}
  GetMem(data, bmp.Height * bmp.Width * 3);
  ptrDst := data;
  for i := 0 to bmp.Height - 1 do
  begin
    ptrSrc := bmp.ScanLine[i];
    Move(ptrSrc^, ptrDst^, bmp.Width * 3);
    Inc(ptrDst, bmp.Width * 3);
  end;
{$ENDIF}

  // Create texture
  FFontTexture := TGLTextureHandle.CreateAndAllocate;
  with FRenderContextInfo.GLStates do
  begin
    TextureBinding[0, ttTextureRect] := FFontTexture.Handle;
    UnpackAlignment := 1;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;
  GL.TexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  GL.TexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  GL.TexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB5, bmp.Width, bmp.Height, 0,
{$IFDEF LINUX}GL_RGBA{$ELSE}GL_BGR{$ENDIF},
    GL_UNSIGNED_BYTE,
{$IFDEF FPC}bmp.RawImage.Data{$ELSE}data{$ENDIF});

{$IFNDEF FPC}
  FreeMem(data);
{$ENDIF}
  bmp.Free;

  for i := 0 to FNodeList.Count - 1 do
  begin
    vNode := Node[i];
    vNode.SetWidth(vNode.Width);
    vNode.SetHeight(vNode.Height);
  end;
end;

procedure TMaterialGraph.CreateBackgroundTexture;
var
  bmp: TGLbitmap;
{$IFNDEF FPC}
  i: Integer;
  data, ptrSrc, ptrDst: PByte;
{$ENDIF}
begin
  bmp := TGLBitmap.Create;
{$IFDEF FPC}
  bmp.LoadFromLazarusResource('Background');
{$ELSE}
  bmp.LoadFromResourceName(HInstance, 'Background');
{$ENDIF}

{$IFNDEF FPC}
  GetMem(data, bmp.Height * bmp.Width * 3);
  ptrDst := data;
  for i := 0 to bmp.Height - 1 do
  begin
    ptrSrc := bmp.ScanLine[i];
    Move(ptrSrc^, ptrDst^, bmp.Width * 3);
    Inc(ptrDst, bmp.Width * 3);
  end;
{$ENDIF}

  FBackTexture := TGLTextureHandle.CreateAndAllocate;
  with FRenderContextInfo.GLStates do
  begin
    TextureBinding[0, ttTexture2D] := FBackTexture.Handle;
    UnpackAlignment := 1;
    UnpackRowLength := 0;
    UnpackSkipRows := 0;
    UnpackSkipPixels := 0;
  end;
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_MIRRORED_REPEAT);
  GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_MIRRORED_REPEAT);
  GL.TexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, bmp.Width, bmp.Height, 0,
    GL_BGR,
    GL_UNSIGNED_BYTE,
{$IFDEF FPC}bmp.RawImage.Data{$ELSE}data{$ENDIF});

{$IFNDEF FPC}
  FreeMem(data);
{$ENDIF}
  bmp.Free;
end;

procedure TMaterialGraph.SetErrorMessage(const AMsg: string);
begin
  if Length(FErrorMessage) = 0 then
    FErrorMessage := AMsg;
end;

procedure TMaterialGraph.SetShaderCodePad(AList: TStrings);
begin
  if FShaderCodePad <> AList then
  begin
    FShaderCodePad := AList;
    FGraphChanged := True;
  end;
end;

function TMaterialGraph.AddNode(const VertexClass: TCustomMaterialGraphNodeClass;
  const x, y, w, h: Integer): TCustomMaterialGraphNode;
var
  vName: string;
begin
  Result := TCustomMaterialGraphNode(inherited AddNode(VertexClass, x, y, w, h));
  if Result is TCustomConstantNode then
    vName := 'Const'
  else
    vName := Result.Caption;
  if Result is TTextureSamplerNode then
    RegisterTexture(TTextureSamplerNode(Result).Texture);
  Result.FViewingObject := vName + 'NodeObj' + IntToStr(FNodeCounter);
  Result.FViewingProgram := vName + 'NodeProg' + IntToStr(FNodeCounter);
  Inc(FNodeCounter);
end;

function TMaterialGraph.InitGraphics: Boolean;
begin
  Result := True;

  FRenderContextInfo.GLStates := CurrentGLContext.GLStates;

  uniformGradientColor1 := TGLSLUniform.RegisterUniform('GradientColor1');
  uniformGradientColor2 := TGLSLUniform.RegisterUniform('GradientColor2');
  uniformScaleOffset := TGLSLUniform.RegisterUniform('ScaleOffset');
  uniformArrowSize := TGLSLUniform.RegisterUniform('ArrowSize');
  uniformLength := TGLSLUniform.RegisterUniform('Length');
  uniformRectSize := TGLSLUniform.RegisterUniform('RectSize');
  uniformCharSize := TGLSLUniform.RegisterUniform('CharSize');
  uniformString := TGLSLUniform.RegisterUniform('String');

  with ShaderManager do
  begin
    try
      BeginWork;
      DefineShaderProgram('Frame');
      DefineShaderObject('FrameVertex', Frame_vp150, [ptVertex]);
      DefineShaderObject('FrameFragment', Frame_fp150, [ptFragment]);
      AttachShaderObjectToProgram('FrameVertex', 'Frame');
      AttachShaderObjectToProgram('FrameFragment', 'Frame');
      Result := Result and LinkShaderProgram('Frame');

      DefineShaderProgram('Viewing');
      DefineShaderObject('ViewingFragment', Viewing_fp150, [ptFragment]);
      AttachShaderObjectToProgram('FrameVertex', 'Viewing');
      AttachShaderObjectToProgram('ViewingFragment', 'Viewing');
      Result := Result and LinkShaderProgram('Viewing');

      DefineShaderProgram('Text');
      DefineShaderObject('TextFragment', Text_fp150, [ptFragment]);
      AttachShaderObjectToProgram('FrameVertex', 'Text');
      AttachShaderObjectToProgram('TextFragment', 'Text');
      Result := Result and LinkShaderProgram('Text');

      DefineShaderProgram('Background');
      DefineShaderObject('BackgroundVertex', Background_vp150, [ptVertex]);
      DefineShaderObject('BackgroundFragment', Background_fp150, [ptFragment]);
      AttachShaderObjectToProgram('BackgroundVertex', 'Background');
      AttachShaderObjectToProgram('BackgroundFragment', 'Background');
      Result := Result and LinkShaderProgram('Background');

      DefineShaderProgram('Joint');
      DefineShaderObject('JointVertex', Joint_vp150, [ptVertex]);
      DefineShaderObject('JointFragment', Joint_fp150, [ptFragment]);
      AttachShaderObjectToProgram('JointVertex', 'Joint');
      AttachShaderObjectToProgram('JointFragment', 'Joint');
      Result := Result and LinkShaderProgram('Joint');

      DefineShaderProgram('Link', [ptVertex, ptFragment, ptGeometry]);
      DefineShaderObject('LinkVertex', Link_vp150, [ptVertex]);
      DefineShaderObject('LinkGeometry', Link_gp150, [ptGeometry]);
      DefineShaderObject('LinkFragment', Link_fp150, [ptFragment]);
      AttachShaderObjectToProgram('LinkVertex', 'Link');
      AttachShaderObjectToProgram('LinkGeometry', 'Link');
      AttachShaderObjectToProgram('LinkFragment', 'Link');
      Result := Result and LinkShaderProgram('Link');

      DefineShaderObject('ViewingMakerVertex', ViewingMaker_vp150, [ptVertex]);
      DefineShaderObject('ViewingMakerFragment', ViewingMaker_fp150,
        [ptFragment]);
    finally
      EndWork;
    end;
  end;

  if Result then
  begin
    CreateFontTexture;
    CreateBackgroundTexture;
    FFBO := TGLFramebufferHandle.CreateAndAllocate;
  end;
end;

procedure TMaterialGraph.Render;
var
  n: Integer;
  vNode: TCustomMaterialGraphNode;
  CharSize: TVector2i;
  TexSize: TVector4f;
  FScreenRect: TVector4s;
  AtLeastOneVisible: Boolean;
  startJoint: PNodeJoint;
  spline: array[0..7] of Single;
  jc: TVector2s;
  jcss: TVector2f;
  intStr: array[0..127] of Integer;

  procedure ScreenQuad;
  begin
    with DynamicVBOManager do
    begin
      BeginObject(nil);
      Attribute2f(attrPosition, 0, 0);
      Attribute2f(attrTexCoord0, 0, 0);
      BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
      EmitVertex;
      Attribute2f(attrPosition, 2, 0);
      Attribute2f(attrTexCoord0, 1, 0);
      EmitVertex;
      Attribute2f(attrPosition, 0, 2);
      Attribute2f(attrTexCoord0, 0, 1);
      EmitVertex;
      Attribute2f(attrPosition, 2, 2);
      Attribute2f(attrTexCoord0, 1, 1);
      EmitVertex;
      EndPrimitives;
      EndObject;
    end;
  end;

begin
  if not FGraphicInitialized then
  begin
    FGraphicInitialized := InitGraphics;
    if not FGraphicInitialized then
      exit;
  end;

  PackLists;
  if FGraphChanged then
  begin
    if Assigned(FShaderCodePad) then
      RefreshMaterial;
    FGraphChanged := False;
    FErrorMessage := '';
  end;

  FRenderContextInfo.viewPortSize.cx := FScreenSize[0];
  FRenderContextInfo.viewPortSize.cy := FScreenSize[1];

  // Screen rect culling
  FScreenRect[0] := FScreenCenter[0] - Round(FScreenZoom * FScreenSize[0] / 2);
  FScreenRect[1] := FScreenCenter[1] - Round(FScreenZoom * FScreenSize[1] / 2);
  FScreenRect[2] := FScreenCenter[0] + Round(FScreenZoom * FScreenSize[0] / 2);
  FScreenRect[3] := FScreenCenter[1] + Round(FScreenZoom * FScreenSize[1] / 2);
  AtLeastOneVisible := false;
  if not FPulling then
    FSelectedJointNode1 := nil;
  FSelectedJointNode2 := nil;

  for n := 0 to FNodeList.Count - 1 do
  begin
    vNode := Node[n];
    vNode.FInScreen :=
      (vNode.Top - cNodeCaptionHeigth < FScreenRect[3]) and
      (vNode.Top + vNode.Height > FScreenRect[1]) and
      (vNode.Left < FScreenRect[2]) and
      (vNode.Left + vNode.Width > FScreenRect[0]);
    AtLeastOneVisible := AtLeastOneVisible or vNode.FInScreen;
  end;

  FRenderContextInfo.GLStates.Disable(stBlend);

  with ShaderManager do
  begin
    // Draw background

    UseProgram('Background');
    // Set uniforms
    TexSize := VectorMake(
      FScreenZoom * FScreenSize[0] / 256,
      FScreenZoom * FScreenSize[1] / 128,
      2 * frac(FScreenCenter[0] / 512),
      2 * frac(FScreenCenter[1] / 256));

    Uniform4f(uniformScaleOffset, TexSize);
    Uniform4f(uniformGradientColor1, clrSkyBlue);
    Uniform4f(uniformGradientColor2, clrWhite);
    UniformSampler(uniformTexUnit0, FBackTexture.Handle, 0);
    ScreenQuad;

    if not AtLeastOneVisible then
      exit;

    // Draw nodes frame
    UseProgram('Frame');
    for n := 0 to FNodeList.Count - 1 do
    begin
      vNode := Node[n];
      if vNode.FInScreen then
        vNode.RenderFrame;
    end;

    // Update node's work viewing
    FRenderContextInfo.GLStates.ViewPort := Vector4iMake(0, 0, 96, 96);
    for n := 0 to FNodeList.Count - 1 do
      Node[n].UpdateViewing;

    FRenderContextInfo.GLStates.ViewPort :=
      Vector4iMake(0, 0, FRenderContextInfo.viewPortSize.cx, FRenderContextInfo.viewPortSize.cy);

    // Draw node's work viewing
    UseProgram('Viewing');
    for n := 0 to FNodeList.Count - 1 do
    begin
      vNode := Node[n];
      if vNode.FInScreen then
        vNode.RenderViewing;
    end;

    if FScreenZoom < 2 then
    begin
      // Draw Text of Nodes
      UseProgram('Text');
      CharSize[0] := FMaxCharWidth;
      CharSize[1] := FMaxCharHeight;
      Uniform2I(uniformCharSize, CharSize);
      UniformSampler(uniformTexUnit0, FFontTexture.Handle, 0);

      for n := 0 to FNodeList.Count - 1 do
      begin
        vNode := Node[n];
        if vNode.FInScreen then
          vNode.RenderText;
      end;

      // Draw main message
      if Length(FErrorMessage) > 0 then
      begin
        with ShaderManager do
        begin
          TexSize := VectorMake(1, -1, -1.0, 1.0);
          Uniform4f(uniformScaleOffset, TexSize);
          for n := 0 to Length(FErrorMessage) - 1 do
            intStr[n] := Integer(FErrorMessage[n + 1]);
          Uniform1I(uniformString, @intStr[0], Length(FErrorMessage));
          TexSize := VectorMake(FScreenSize[0], FScreenSize[1], -5, -5);
          Uniform4f(uniformRectSize, TexSize);
          Uniform1I(uniformLength, Length(FErrorMessage));
        end;
        ScreenQuad;
      end;

      // Draw Joints and Links of Nodes
      FJointPositionList.Flush;
      FJointColorList.Flush;
      FLinksCoordsList.Flush;
      FLinksColorList.Flush;
      UseProgram('Joint');
      // Collect vertices
      for n := 0 to FNodeList.Count - 1 do
        Node[n].RenderJoint;

      // Draw collected lists
      if FJointPositionList.Count > 0 then
      begin
        FRenderContextInfo.GLStates.PointSize := 12.0 / FScreenZoom;
        with DynamicVBOManager do
        begin
          // Joints backside
          GL.VertexAttrib4f(attrVertexColor.Location, 0.25, 0.25, 0.25, 1.0);
          BeginObject(nil);
          Attribute2f(attrPosition, 0, 0);
          BeginPrimitives(GLVBOM_POINTS);
          AttributeList(attrPosition, FJointPositionList);
          EmitVertex;
          EndPrimitives;
          EndObject;
          // Joints frontside
          FRenderContextInfo.GLStates.PointSize := 8.0 / FScreenZoom;
          BeginObject(nil);
          Attribute2f(attrPosition, 0, 0);
          Attribute4f(attrVertexColor, 0, 0, 0, 0);
          BeginPrimitives(GLVBOM_POINTS);
          AttributeList(attrPosition, FJointPositionList);
          AttributeList(attrVertexColor, FJointColorList);
          EmitVertex;
          EndPrimitives;
          EndObject;
        end;
      end;

      // Draw Link when state creation
      if FPulling and Assigned(FSelectedJointNode1) then
      begin
        startJoint := PNodeJoint(GetSelectedJointNode1.Joints[FSelectedJointIndex1]);
        jc := GetSelectedJointNode1.JointCoords[FSelectedJointIndex1];
        jcss := GetScreenSpaceCoords(jc);
        spline[0] := jcss[0];
        spline[1] := jcss[1];
        jc := GetSelectedJointNode1.JointCoords[FSelectedJointIndex1];
        case startJoint.Side of
          sideInput: jc[0] := jc[0] - 5 * FMaxCharWidth;
          sideOutput: jc[0] := jc[0] + 5 * FMaxCharWidth;
        end;
        jcss := GetScreenSpaceCoords(jc);
        spline[2] := jcss[0];
        spline[3] := jcss[1];
        jc := FCursorCoords;
        case startJoint.Side of
          sideInput: jc[0] := jc[0] + 5 * FMaxCharWidth;
          sideOutput: jc[0] := jc[0] - 5 * FMaxCharWidth;
        end;
        jcss := GetScreenSpaceCoords(jc);
        spline[4] := jcss[0];
        spline[5] := jcss[1];
        jcss := GetScreenSpaceCoords(FCursorCoords);
        spline[6] := jcss[0];
        spline[7] := jcss[1];
        FLinksCoordsList.AddSingles(spline);
        FLinksColorList.AddSingles(clrBlack);
        FLinksColorList.AddSingles(clrBlack);
        FLinksColorList.AddSingles(clrBlack);
        FLinksColorList.AddSingles(clrBlack);
      end;

      if FLinksCoordsList.Count > 0 then
      begin
        UseProgram('Link');
        jcss[0] := 24 / FScreenSize[0] / FScreenZoom;
        jcss[1] := 6 / FScreenSize[1] / FScreenZoom;
        Uniform2f(uniformArrowSize, jcss);

        with DynamicVBOManager do
        begin
          BeginObject(nil);
          Attribute2f(attrPosition, 0, 0);
          Attribute4f(attrVertexColor, 0, 0, 0, 0);
          BeginPrimitives(GLVBOM_LINES_ADJACENCY);
          AttributeList(attrPosition, FLinksCoordsList);
          AttributeList(attrVertexColor, FLinksColorList);
          EmitVertex;
          EndPrimitives;
          EndObject;
        end;
      end;
    end;

  end;
  GL.CheckError;
end;

procedure TMaterialGraph.RefreshMaterial;
var
  I: Integer;
  vNode: TCustomMaterialGraphNode;
  ok: Boolean;
  Info: TSampleGatherInfo;
begin
  vNode := Node[0];
  // Checking, just in case
  if not (vNode is TMaterialNode) then
    for I := 1 to FNodeList.Count - 1 do
    begin
      vNode := Node[I];
      if vNode is TMaterialNode then
        break;
    end;
  if vNode = nil then
    exit;

  gvarSample := nil;
  if Assigned(FShaderCodePad) then
    FShaderCodePad.Clear;
  FMaterial.ClearSamples;
  FMaterial.ClearUniforms;

  // Clear sample cache
  for I := 0 to FNodeList.Count - 1 do
    Node[I].ClearSampleCache;

  Info.SampleList := FMaterial.SampleList;
  Info.PassTexCoord := False;
  ok := False;

  try
    ok := vNode.GatherSamples(Info, 0);
  finally
    if ok then
      ok := MaterialManager.GenerateShaderCode(FMaterial.SampleList, FVertexProgram, FFragmentProgram);

    if ok then
    begin
      for I := 0 to High(FTextures) do
        if FTextures[I].UseCount > 0 then
          FMaterial.TextureUnits.Add(FTextures[I].Name);
      FMaterial.CreateUniforms;
      FMaterial.CreateShader(AnsiString(FVertexProgram.Text), AnsiString(FFragmentProgram.Text));
      if Assigned(FShaderCodePad) then
      begin
        FShaderCodePad.AddStrings(FVertexProgram);
        FShaderCodePad.Add('');
        FShaderCodePad.AddStrings(FFragmentProgram);
      end;
    end
    else if Assigned(FShaderCodePad) then
      FShaderCodePad.Add('Fail to build material shader!');
  end;
end;

procedure TMaterialGraph.RegisterTexture(const AName: TGL3xTextureName);
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
  begin
    if FTextures[I].Name = AName then
    begin
      Inc(FTextures[I].UseCount);
      exit;
    end;
  end;
  for I := 0 to High(FTextures) do
  begin
    if FTextures[I].UseCount = 0 then
    begin
      FTextures[I].Name := AName;
      Inc(FTextures[I].UseCount);
      exit;
    end;
  end;
end;

procedure TMaterialGraph.UnRegisterTexture(const AName: TGL3xTextureName);
var
  I: Integer;
begin
  for I := 0 to High(FTextures) do
  begin
    if (FTextures[I].UseCount <> 0) and (FTextures[I].Name = AName) then
    begin
      Dec(FTextures[I].UseCount);
      if Ftextures[I].UseCount = 0 then
        FTextures[I].Name := '';
      exit;
    end;
  end;
end;

function TMaterialGraph.GetTextureUnitIndex(const AName: TGL3xTextureName): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FTextures) do
  begin
    if (FTextures[I].UseCount <> 0) and (FTextures[I].Name = AName) then
      exit;
    Inc(Result);
  end;
  Assert(False);
  Result := 0;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TNodeJoint'}{$ENDIF}

procedure TNodeJoint.SetAtOnce(AStep: SmallInt; ASide: TNodeSize;
  AMask: TGLColorComponentMask; const ACaption: AnsiString);
begin
  Step := AStep;
  Side := ASide;
  Mask := AMask;
  Caption := ACaption;
  GivingNode := nil;
  GivingNodeJointIndex := 0;
end;

function TNodeJoint.Color: TVector;
begin
  if ccmRed in Mask then
    Result := clrRed
  else if ccmGreen in Mask then
    Result := clrGreen
  else if ccmBlue in Mask then
    Result := clrBlue
  else if ccmAlpha in Mask then
    Result := clrGray20
  else if ccmWhite in Mask then
    Result := clrWhite
  else
    Result := clrBlack;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCustomMaterialGraphNode'}{$ENDIF}

constructor TCustomMaterialGraphNode.Create;
begin
  FHighlight := false;
  Deletable := True;
  FViewingProgLinked := false;
  FViewingTexture := TGLTextureHandle.Create;
  FGatherLock := False;
  NotifyChange;
end;

procedure TCustomMaterialGraphNode.NotifyChange;
begin
  inherited;
  if Assigned(Owner) then
  begin
    Owner.FGraphChanged := True;
  end;
end;

destructor TCustomMaterialGraphNode.Destroy;
begin
  Owner.EraseNode(Self);
  with ShaderManager do
  begin
    try
      BeginWork;
      if Length(FViewingObject) > 0 then
        DeleteShaderObject(FViewingObject);
      if Length(FViewingProgram) > 0 then
        DeleteShaderProgram(FViewingProgram);
    finally
      EndWork;
    end;
  end;
  FViewingTexture.Free;
  inherited;
end;

function TCustomMaterialGraphNode.GetJoints(I: Integer): PNodeJoint;
begin
  Result := @FJoints[I];
end;

procedure TCustomMaterialGraphNode.SetJoints(I: Integer; Value: PNodeJoint);
begin
  FJoints[I] := Value^;
end;

function TCustomMaterialGraphNode.GetCount: Integer;
begin
  Result := Length(FJoints);
end;

procedure TCustomMaterialGraphNode.ClearSampleCache;
var
  I: Integer;
begin
  for I := 0 to High(FJoints) do
    FJoints[I].SampleCache := nil;
  FPrimarySampleChache := nil;
end;

function TCustomMaterialGraphNode.GetGraph: TMaterialGraph;
begin
  Result := TMaterialGraph(FGraph);
end;

procedure TCustomMaterialGraphNode.SetWidth(Value: Integer);
var
  maxWidth, jWidth: Integer;
  I: Integer;
begin
  maxWidth := Owner.FMaxCharWidth * Length(Caption);
  for I := 0 to High(FJoints) do
  begin
    jWidth := Owner.FMaxCharWidth * Length(Joints[I].Caption);
    if jWidth > maxWidth then
      maxWidth := jWidth;
  end;

  if maxWidth > Value then
    Value := 2 * Owner.FMaxCharWidth + maxWidth;
  FRect.Width := Value;
end;

procedure TCustomMaterialGraphNode.SetHeight(Value: Integer);
var
  maxHeight, jHeight: Integer;
begin
  maxHeight := 100;
  jHeight := Owner.FMaxCharHeight * (Joints[High(FJoints)].Step + 2) + cNodeCaptionHeigth;
  if jHeight > maxHeight then
    maxHeight := jHeight;

  if Value < maxHeight then
    Value := maxHeight;
  FRect.Height := Value;
end;

class function TCustomMaterialGraphNode.ValueToHex(const Val): string;
begin
  SetLength(Result, 32);
  BinToHex(PAnsiChar(@Val), PChar(Result), 16);
end;

function TCustomMaterialGraphNode.GetJointCoords(I: Integer): TVector2s;
begin
  Result[0] := FRect.Left - 5;
  if Joints[i].Side = sideOutput then
    Result[0] := Result[0] + Width + 9;
  Result[1] := FRect.Top + Owner.FMaxCharHeight * (Joints[i].Step + 1);
end;

procedure TCustomMaterialGraphNode.RenderFrame;
var
  w, h, h1, h2: Integer;
begin
  with Owner, ShaderManager, DynamicVBOManager do
  begin
    SetVector(FScaleOffset,
      2 / FScreenSize[0],
      2 / FScreenSize[1],
      2 * (FRect.Left - FScreenCenter[0]) / FScreenSize[0],
      2 * (FRect.Top - FScreenCenter[1]) / FScreenSize[1]);
    w := FRect.Width;
    h := FRect.Height;
    h1 := h + 2;
    h2 := h1 + cNodeCaptionHeigth;
    ScaleVector(FScaleOffset, 1 / FScreenZoom);

    // Node frame
    Uniform4f(uniformScaleOffset, FScaleOffset);
    Uniform4f(uniformDiffuse, clrGray);

    BeginObject(nil);
    Attribute2f(attrPosition, 0, 0);
    Attribute2f(attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    EmitVertex;
    Attribute2f(attrPosition, w, 0);
    Attribute2f(attrTexCoord0, 1, 0);
    EmitVertex;
    Attribute2f(attrPosition, 0, h);
    Attribute2f(attrTexCoord0, 0, 1);
    EmitVertex;
    Attribute2f(attrPosition, w, h);
    Attribute2f(attrTexCoord0, 1, 1);
    EmitVertex;
    RestartStrip;
    Attribute2f(attrPosition, 0, h1);
    Attribute2f(attrTexCoord0, 0, 0);
    EmitVertex;
    Attribute2f(attrPosition, w, h1);
    Attribute2f(attrTexCoord0, 1, 0);
    EmitVertex;
    Attribute2f(attrPosition, 0, h2);
    Attribute2f(attrTexCoord0, 0, 1);
    EmitVertex;
    Attribute2f(attrPosition, w, h2);
    Attribute2f(attrTexCoord0, 1, 1);
    EmitVertex;
    EndPrimitives;
    EndObject;

    if FHighlight then
      Uniform4f(uniformDiffuse, clrYellow)
    else
      Uniform4f(uniformDiffuse, clrBlack);

    BeginObject(nil);
    Attribute2f(attrPosition, 0, 0);
    BeginPrimitives(GLVBOM_LINE_LOOP);
    EmitVertex;
    Attribute2f(attrPosition, w, 0);
    EmitVertex;
    Attribute2f(attrPosition, w, h);
    EmitVertex;
    Attribute2f(attrPosition, 0, h);
    EmitVertex;
    EndPrimitives;

    BeginPrimitives(GLVBOM_LINE_LOOP);
    Attribute2f(attrPosition, 0, h1);
    EmitVertex;
    Attribute2f(attrPosition, w, h1);
    EmitVertex;
    Attribute2f(attrPosition, w, h2);
    EmitVertex;
    Attribute2f(attrPosition, 0, h2);
    EmitVertex;
    EndPrimitives;
    EndObject;
  end;
end;

procedure TCustomMaterialGraphNode.RenderText;
var
  i, j, len: Integer;
  RectSize: TVector4f;
  TextTop, TextBottom: Single;
  intStr: array[0..127] of Integer;

  procedure TextOut;
  begin
    with Owner, DynamicVBOManager do
    begin
      BeginObject(nil);
      Attribute2f(attrPosition, 0, TextBottom);
      Attribute2f(attrTexCoord0, 0, 1);
      BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
      EmitVertex;
      Attribute2f(attrPosition, 0, TextTop);
      Attribute2f(attrTexCoord0, 0, 0);
      EmitVertex;
      Attribute2f(attrPosition, FRect.Width, TextBottom);
      Attribute2f(attrTexCoord0, 1, 1);
      EmitVertex;
      Attribute2f(attrPosition, FRect.Width, TextTop);
      Attribute2f(attrTexCoord0, 1, 0);
      EmitVertex;
      EndPrimitives;
      EndObject;
    end;
  end;

begin
  with Owner, ShaderManager do
  begin
    len := Length(Caption);
    if len > 0 then
    begin
      // Node Caption Text
      Uniform4f(uniformScaleOffset, FScaleOffset);
      for i := 0 to len - 1 do
        intStr[i] := Integer(Caption[i + 1]);
      Uniform1I(uniformString, @intStr[0], len);
      RectSize := VectorMake(
        FRect.Width,
        cNodeCaptionHeigth,
        -5,
        (FMaxCharHeight - cNodeCaptionHeigth) div 2);
      Uniform4f(uniformRectSize, RectSize);
      Uniform1I(uniformLength, len);
      TextBottom := FRect.Height + 2;
      TextTop := TextBottom + cNodeCaptionHeigth;
      TextOut;
      // Joints Caption Text
      if Count > 0 then
      begin
        for i := 0 to Count - 1 do
        begin
          len := Length(Joints[i].Caption);
          if len > 0 then
          begin
            for j := 0 to len - 1 do
              intStr[j] := Integer(Joints[i].Caption[j + 1]);
            Uniform1I(uniformString, @intStr[0], len);
            TextBottom := FMaxCharHeight * (Joints[i].Step + 0.5);
            TextTop := FMaxCharHeight * (Joints[i].Step + 1.5);
            SetVector(RectSize, FRect.Width, TextTop - TextBottom, -5, 0);
            Uniform4f(uniformRectSize, RectSize);
            Uniform1I(uniformLength, len);
            TextOut;
          end;
        end;
      end;
    end
  end;
end;

procedure TCustomMaterialGraphNode.RenderJoint;
var
  i, temp: Integer;
  jc: TVector2s;
  jcss: TVector2f;
  spline: array[0..7] of Single;
  hl: Boolean;
  clr: TVector;
begin
  with Owner do
  begin
    if Count > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        hl := False;
        jc := GetJointCoords(i);
        jcss := GetScreenSpaceCoords(jc);
        FJointPositionList.AddSingles(@jcss[0], 2);
        // Self color or highlight joint
        if ((sqr(jc[0] - FCursorCoords[0]) + sqr(jc[1] - FCursorCoords[1]))
          < 64) then
        begin
          hl := True;
          FJointColorList.AddSingles(clrOrange);
          if not Assigned(FSelectedJointNode1) then
          begin
            FSelectedJointNode1 := Self;
            FSelectedJointIndex1 := i;
          end
          else if Self <> FSelectedJointNode1 then
          begin
            FSelectedJointNode2 := Self;
            FSelectedJointIndex2 := i;
          end;
        end
        else
        begin
          clr := Joints[i].Color;
          FJointColorList.AddSingles(clr);
        end;
        // Links
        if (Joints[i].Side = sideInput) and Assigned(Joints[i].GivingNode) then
        begin
          jc :=
            Joints[i].GivingNode.GetJointCoords(Joints[i].GivingNodeJointIndex);
          jcss := GetScreenSpaceCoords(jc);
          spline[0] := jcss[0];
          spline[1] := jcss[1];
          jc[0] := jc[0] + 5 * FMaxCharWidth;
          jcss := GetScreenSpaceCoords(jc);
          spline[2] := jcss[0];
          spline[3] := jcss[1];
          jc := GetJointCoords(i);
          temp := jc[0];
          jc[0] := jc[0] - 5 * FMaxCharWidth;
          jcss := GetScreenSpaceCoords(jc);
          spline[4] := jcss[0];
          spline[5] := jcss[1];
          jc[0] := temp;
          jcss := GetScreenSpaceCoords(jc);
          spline[6] := jcss[0];
          spline[7] := jcss[1];
          FLinksCoordsList.AddSingles(spline);
          if hl then
          begin
            FLinksColorList.AddSingles(clrOrange);
            FLinksColorList.AddSingles(clrOrange);
            FLinksColorList.AddSingles(clrOrange);
            FLinksColorList.AddSingles(clrOrange);
          end
          else
          begin
            FLinksColorList.AddSingles(clrBlack);
            FLinksColorList.AddSingles(clrBlack);
            FLinksColorList.AddSingles(clrBlack);
            FLinksColorList.AddSingles(clrBlack);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomMaterialGraphNode.RenderViewing;
begin
  with Owner, ShaderManager, DynamicVBOManager do
  begin
    // Node work viewing
    Uniform4f(uniformScaleOffset, FScaleOffset);
    UniformSampler(uniformTexUnit0, FViewingTexture.Handle, 0);

    BeginObject(nil);
    Attribute2f(attrPosition, 2, 2);
    Attribute2f(attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    EmitVertex;
    Attribute2f(attrPosition, 98, 2);
    Attribute2f(attrTexCoord0, 1, 0);
    EmitVertex;
    Attribute2f(attrPosition, 2, 98);
    Attribute2f(attrTexCoord0, 0, 1);
    EmitVertex;
    Attribute2f(attrPosition, 98, 98);
    Attribute2f(attrTexCoord0, 1, 1);
    EmitVertex;
    EndPrimitives;
    EndObject;
  end;
end;

procedure TCustomMaterialGraphNode.UpdateViewing;
var
  I: Integer;
begin

  if FChanged then
  begin
    with ShaderManager do
    begin
      try
        BeginWork;
        DefineShaderProgram(FViewingProgram);
        DefineShaderObject(FViewingObject, GetViewingCode, [ptFragment]);
        AttachShaderObjectToProgram('ViewingMakerVertex', FViewingProgram);
        AttachShaderObjectToProgram('ViewingMakerFragment', FViewingProgram);
        AttachShaderObjectToProgram(FViewingObject, FViewingProgram);
        FViewingProgLinked := LinkShaderProgram(FViewingProgram);
      finally
        EndWork;
      end;
    end;

    if FViewingTexture.Handle = 0 then
    begin
      FViewingTexture.AllocateHandle;
      with Owner.FRenderContextInfo.GLStates do
        TextureBinding[0, ttTexture2D] := FViewingTexture.Handle;
      GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      GL.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      GL.TexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, 96, 96, 0,
        GL_RGBA, GL_HALF_FLOAT, nil);
    end;
    FChanged := False;
    for I := 0 to Count - 1 do
      if (Joints[I].Side = sideOutput) and Assigned(Joints[I].GivingNode) then
        Joints[I].GivingNode.NotifyChange;
  end;

  if not FViewingProgLinked then
    exit;

  with Owner, DynamicVBOManager do
  begin
    FFBO.BindForDrawing;
    FFBO.Attach2DTexture(
      GL_DRAW_FRAMEBUFFER,
      GL_COLOR_ATTACHMENT0,
      GL_TEXTURE_2D,
      FViewingTexture.Handle,
      0);
    ShaderManager.UseProgram(FViewingProgram);
    SetViewingUniforms;
    BeginObject(nil);
    Attribute2f(attrPosition, -1.0, -1.0);
    Attribute2f(attrTexCoord0, 0.0, 0.0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    EmitVertex;
    Attribute2f(attrPosition, -1.0, 1.0);
    Attribute2f(attrTexCoord0, 0.0, 1.0);
    EmitVertex;
    Attribute2f(attrPosition, 1.0, -1.0);
    Attribute2f(attrTexCoord0, 1.0, 0.0);
    EmitVertex;
    Attribute2f(attrPosition, 1.0, 1.0);
    Attribute2f(attrTexCoord0, 1.0, 1.0);
    EmitVertex;
    EndPrimitives;
    EndObject;
    FFBO.UnBindForDrawing;
  end;
end;

procedure TCustomMaterialGraphNode.CheckNodeInput(JointIndex: SmallInt; const AMessage: string; var Flag: Boolean);
var
  bPresent: Boolean;
begin
  bPresent := Assigned(Joints[JointIndex].GivingNode);
  Flag := Flag and bPresent;
  if not bPresent then
    Owner.SetErrorMessage(AMessage + ' in node ' + Caption);
end;

procedure TCustomMaterialGraphNode.CheckOutput(Flag: Boolean);
begin
  if not Flag then
    Owner.SetErrorMessage('Bad combination of operands, impossible to perform operation in node ' + Caption);
end;

function TCustomMaterialGraphNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    GetWorkResult_line +
    '{' + #10#13 +
    '  return vec4(0.0, 0.0, 0.0, 0.0);' + #10#13 +
    '}';
end;

procedure TCustomMaterialGraphNode.SetViewingUniforms;
begin
end;

function TCustomMaterialGraphNode.GetGivingNodeTextureID(JointIndex: SmallInt): TGLuint;
begin
  if Assigned(Joints[JointIndex].GivingNode) then
    Result := Joints[JointIndex].GivingNode.FViewingTexture.Handle
  else
    Result := 0;
end;

function TCustomMaterialGraphNode.GetGivingNodeMask(JointIndex: SmallInt): TGLColorComponentMask;
var
  I: Integer;
begin
  if Assigned(Joints[JointIndex].GivingNode) then
  begin
    I := Joints[JointIndex].GivingNodeJointIndex;
    Result := Joints[JointIndex].GivingNode.Joints[I].Mask;
  end
  else
    Result := [];
end;

function TCustomMaterialGraphNode.GetGivingNodeDataType(JointIndex: SmallInt): TGLSLDataType;
var
  vSample: TShaderSample;
  vMask: TGLColorComponentMask;
  OK: Boolean;
begin
  if Assigned(Joints[JointIndex].GivingNode) and not FGetOuptupSampleLock then
  begin
    FGetOuptupSampleLock := True;
    try
      OK := Joints[JointIndex].GivingNode.GetOutputSample(vSample);
    finally
      FGetOuptupSampleLock := False;
    end;
    if OK then
    begin
      Result := vSample.Output;
      vMask := GivingNodeMask[JointIndex];
      if vMask <> [] then
        Result := MaskToGLSLType(vMask);
    end
    else
      Result := GLSLTypeUndefined;
  end
  else
  begin
    if FGetOuptupSampleLock then
      Owner.SetErrorMessage('Graph obsessed, recorded a second pass through node ' + Self.Caption);
    Result := GLSLTypeUndefined;
  end;
end;

function TCustomMaterialGraphNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := False;
end;

procedure TCustomMaterialGraphNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
begin
end;

function TCustomMaterialGraphNode.IsJointCanLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean;
begin
  Result := Joints[jointIndex].Side <> TCustomMaterialGraphNode(giver).Joints[giverJointIndex].Side;
end;

function TCustomMaterialGraphNode.IsStartLinkNode(jointIndex: Integer): Boolean;
begin
  Result := Joints[jointIndex].Side = sideOutput;
end;

procedure TCustomMaterialGraphNode.SetLink(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer);
begin
  Joints[jointIndex].GivingNode := TCustomMaterialGraphNode(giver);
  Joints[jointIndex].GivingNodeJointIndex := giverJointIndex;
  NotifyChange;
end;

function TCustomMaterialGraphNode.IsJointLinkedWith(jointIndex: Integer; giver: TBaseGraphVertex; giverJointIndex: Integer): Boolean;
begin
  Result := (Joints[jointIndex].GivingNode = giver)
    and (Joints[jointIndex].GivingNodeJointIndex = giverJointIndex);
end;

function TCustomMaterialGraphNode.GatherSamples(var Info: TSampleGatherInfo; JointIndex: Integer): Boolean;
begin
  if FGatherLock then
  begin
    Owner.SetErrorMessage('Graph obsessed, recorded a second pass through node ' + Self.Caption);
    Result := False;
    exit;
  end;

  if Joints[JointIndex].Mask = [] then
    gvarSample := FPrimarySampleChache
  else
    gvarSample := FJoints[JointIndex].SampleCache;

  if Assigned(gvarSample) then
  begin
    Result := True;
    exit;
  end;

  if not Assigned(FPrimarySampleChache) then
  begin
    FGatherLock := True;
    try
      Result := DoGatherSamples(Info);
    finally
      FGatherLock := False;
    end;
    if not Result then
      exit;
    gvarSample.Participate := gvarSample.Participate * gvarProgType;
    FPrimarySampleChache := gvarSample;
    if Joints[JointIndex].Mask = [] then
      exit;
  end;

  New(gvarSample);
  gvarSample.Clear;
  gvarSample.Category := MaterialSystem.Utility.Name;
  gvarSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
  gvarSample.Mask := Joints[JointIndex].Mask;
  gvarSample.InputRef[0] := FPrimarySampleChache;
  Result := MaterialManager.FindCompatibleSample(gvarSample^);
  gvarSample.Participate := gvarProgType;
  Info.SampleList.Add(gvarSample);
  FJoints[JointIndex].SampleCache := gvarSample;
end;

function TCustomMaterialGraphNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TMaterialNode'}{$ENDIF}

constructor TMaterialNode.Create;
begin
  inherited;
  SetLength(FJoints, 10);
  Joints[0].SetAtOnce(0, sideInput, [], 'Emissive');
  Joints[1].SetAtOnce(1, sideInput, [], 'Diffuse');
  Joints[2].SetAtOnce(2, sideInput, [], 'DiffusePower');
  Joints[3].SetAtOnce(3, sideInput, [], 'Specular');
  Joints[4].SetAtOnce(4, sideInput, [], 'SpecularPower');
  Joints[5].SetAtOnce(5, sideInput, [], 'Opacity');
  Joints[6].SetAtOnce(6, sideInput, [], 'OpacityMask');
  Joints[7].SetAtOnce(7, sideInput, [], 'Normal');
  Joints[8].SetAtOnce(8, sideInput, [], 'Distortion');
  Joints[9].SetAtOnce(9, sideInput, [], 'CustomLighting');
  Deletable := False;
  FOpacityMaskClip := VectorMakeEXT(0.5, 0.0, 0.0, 0.0);
end;

function TMaterialNode.Caption: string;
begin
  Result := 'Material';
end;

function TMaterialNode.GetViewingCode: AnsiString;
var
  vNode: TCustomMaterialGraphNode;
  vSample: TShaderSample;
  vType: TGLSLDataType;
begin
  vNode := Joints[1].GivingNode;
  if Assigned(vNode) and vNode.GetOutputSample(vSample) then
  begin
    vType := GivingNodeDataType[1];
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GLSLTypeToString(vType) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vType) + ';' + #10#13 +
      '  return ' + GetGLSLTypeCast('value01', vType, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

procedure TMaterialNode.SetViewingUniforms;
begin
  ShaderManager.UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
end;

function TMaterialNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Fragment.Name;
  ASample.Name := MaterialSystem.Fragment.PassFragmentColor;
  Result := True;
end;

function TMaterialNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
const
  defDiffPower: TVector = (1, 0, 0, 0);
  defSpecPower: TVector = (64, 0, 0, 0);
var
  vSample: TShaderSample;
  pVertexRecord, pWorldNorm,
    pLightVec, pReflVec, pCustomConst,
    pEmissive, pDiffuse, pSpecular, gpRGB: PShaderSample;

  procedure AddToList;
  begin
    if MaterialManager.FindCompatibleSample(vSample) then
    begin
      New(gvarSample);
      vSample.Participate := vSample.Participate * gvarProgType;
      gvarSample^ := vSample;
      Info.SampleList.Add(gvarSample);
    end
    else
      Assert(False);
  end;

begin
  {|-|-|-|-|-|-|-|-|-|-|PHONG LIGHTING MODEL|-|-|-|-|-|-|-|-|-|-|-|}

  /////////////////////////////////////////////////////////////////
  //--------------------FRAGMENT PROGRAM-------------------------//
  /////////////////////////////////////////////////////////////////
  gvarProgType := [ptFragment];

  if Owner.FMaterial.BlendingMode = bmMasked then
  begin
    // Opacity Mask Cliping Value constant
    vSample.Clear;
    vSample.Category := MaterialSystem.Constants.Name;
    vSample.Name := MaterialSystem.Constants.Constants_Scalar;
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(FOpacityMaskClip);
    AddToList;
    pCustomConst := gvarSample;
    // Opacity Mask
    if Assigned(Joints[6].GivingNode)
      and Joints[6].GivingNode.GatherSamples(Info, Joints[6].GivingNodeJointIndex) then
    begin
      if gvarSample.TotalOutput <> GLSLType1F then
      begin
        // Cast Opacity Mask to float
        vSample.Clear;
        vSample.Category := MaterialSystem.Utility.Name;
        vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
        vSample.InputRef[0] := gvarSample;
        vSample.Mask := [ccmRed];
        AddToList;
      end;
    end
    else
    begin
      // Default Opacity Mask
      vSample.Clear;
      vSample.Category := MaterialSystem.Constants.Name;
      vSample.Name := MaterialSystem.Constants.Constants_Scalar;
      vSample.Purpose := sspConstant;
      vSample.ConstantValue := ValueToHex(XHmgVector);
      AddToList;
    end;
    vSample.Clear;
    vSample.Category := MaterialSystem.Fragment.Name;
    vSample.Name := MaterialSystem.Fragment.AlphaTest;
    vSample.InputRef[0] := gvarSample;
    vSample.InputRef[1] := pCustomConst;
    AddToList;
  end;

  // World Normal
  Info.LightingInTextureSpace := False;
  if Assigned(Joints[7].GivingNode)
    and Joints[7].GivingNode.GatherSamples(Info,
    Joints[7].GivingNodeJointIndex) then
  begin
    Info.LightingInTextureSpace := gvarSample.TextureInIntput;
    if gvarSample.TotalOutput <> GLSLType3F then
    begin
      // Cast Normal to vec3
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmWhite];
      AddToList;
    end;
  end
  else
  begin
    // Default Normal
    vSample.Clear;
    vSample.Category := MaterialSystem.Vectors.Name;
    vSample.Name := MaterialSystem.Vectors.Vectors_WorldNormal;
    vSample.Input[0] := GLSLTypeVoid;
    AddToList;
  end;
  pWorldNorm := gvarSample;

  // Light Vector
  vSample.Clear;
  vSample.Category := (MaterialSystem.Vectors.Name);
  vSample.Name := (MaterialSystem.Vectors.Vectors_LightVector);
  vSample.Input[0] := GLSLTypeVoid;
  AddToList;
  pLightVec := gvarSample;

  // Camera Vector
  vSample.Clear;
  vSample.Category := (MaterialSystem.Vectors.Name);
  vSample.Name := (MaterialSystem.Vectors.Vectors_CameraVector);
  vSample.Input[0] := GLSLTypeVoid;
  AddToList;

  // Reflection Vector
  vSample.Clear;
  vSample.Category := (MaterialSystem.Vectors.Name);
  vSample.Name := (MaterialSystem.Vectors.Vectors_ReflectionVector);
  vSample.InputRef[0] := gvarSample;
  vSample.InputRef[1] := pWorldNorm;
  AddToList;
  pReflVec := gvarSample;

  // Emissive Color
  if Assigned(Joints[0].GivingNode)
    and Joints[0].GivingNode.GatherSamples(Info,
    Joints[0].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType3F then
    begin
      // Cast Emissive Color to vec3
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmWhite];
      AddToList;
    end;
  end
  else
  begin
    // Default value
    vSample.Clear;
    vSample.Category := MaterialSystem.Constants.Name;
    vSample.Name := MaterialSystem.Constants.Constants_Vector3;
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(clrGray20);
    AddToList;
  end;
  pEmissive := gvarSample;

  // Diffuse Color
  if Assigned(Joints[1].GivingNode)
    and Joints[1].GivingNode.GatherSamples(Info,
    Joints[1].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType3F then
    begin
      // Cast Diffuse Color to vec3
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmWhite];
      AddToList;
    end
    else
  end
  else
  begin
    // Default value
    vSample.Clear;
    vSample.Category := MaterialSystem.Constants.Name;
    vSample.Name := MaterialSystem.Constants.Constants_Vector3;
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(clrGray80);
    AddToList;
  end;
  pDiffuse := gvarSample;

  // Specular color
  if Assigned(Joints[3].GivingNode)
    and Joints[3].GivingNode.GatherSamples(Info,
    Joints[3].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType3F then
    begin
      // Cast Specular Color to vec3
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmWhite];
      AddToList;
    end;
  end
  else
  begin
    // Default specular
    vSample.Clear;
    vSample.Category := (MaterialSystem.Constants.Name);
    vSample.Name := (MaterialSystem.Constants.Constants_Vector3);
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(clrGray50);
    AddToList;
  end;
  pSpecular := gvarSample;

  // Specular power
  if Assigned(Joints[4].GivingNode)
    and Joints[4].GivingNode.GatherSamples(Info,
    Joints[4].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType1F then
    begin
      // Cast Specular power Color to float
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmRed];
      AddToList;
    end;
  end
  else
  begin
    // Default specular power
    vSample.Clear;
    vSample.Category := (MaterialSystem.Constants.Name);
    vSample.Name := (MaterialSystem.Constants.Constants_Scalar);
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(defSpecPower);
    AddToList;
  end;

  // Total specular
  vSample.Clear;
  vSample.Category := MaterialSystem.Math.Name;
  vSample.Name := MaterialSystem.Math.Math_Phong;
  vSample.InputRef[0] := pLightVec;
  vSample.InputRef[1] := pReflVec;
  vSample.InputRef[2] := gvarSample;
  AddToList;
  vSample.Clear;
  vSample.Category := (MaterialSystem.Math.Name);
  vSample.Name := (MaterialSystem.Math.Math_Mul);
  vSample.InputRef[0] := pSpecular;
  vSample.InputRef[1] := gvarSample;
  AddToList;
  pSpecular := gvarSample;

  // Diffuse power
  if Assigned(Joints[2].GivingNode)
    and Joints[2].GivingNode.GatherSamples(Info,
    Joints[2].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType1F then
    begin
      // Cast Diffuse power to float
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmRed];
      AddToList;
    end;
  end
  else
  begin
    // Default specular power
    vSample.Clear;
    vSample.Category := (MaterialSystem.Constants.Name);
    vSample.Name := (MaterialSystem.Constants.Constants_Scalar);
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(defDiffPower);
    AddToList;
  end;

  // Total diffuse
  vSample.Clear;
  vSample.Category := MaterialSystem.Math.Name;
  vSample.Name := MaterialSystem.Math.Math_Phong;
  vSample.InputRef[0] := pWorldNorm;
  vSample.InputRef[1] := pLightVec;
  vSample.InputRef[2] := gvarSample;
  AddToList;
  vSample.Clear;
  vSample.Category := (MaterialSystem.Math.Name);
  vSample.Name := (MaterialSystem.Math.Math_Mul);
  vSample.InputRef[0] := pDiffuse;
  vSample.InputRef[1] := gvarSample;
  AddToList;
  pDiffuse := gvarSample;

  // Summa
  vSample.Clear;
  vSample.Category := (MaterialSystem.Math.Name);
  vSample.Name := (MaterialSystem.Math.Math_Add);
  vSample.InputRef[0] := pEmissive;
  vSample.InputRef[1] := pDiffuse;
  vSample.InputRef[2] := pSpecular;
  AddToList;
  gpRGB := gvarSample;

  // Opacity
  if Assigned(Joints[5].GivingNode)
    and Joints[5].GivingNode.GatherSamples(Info,
    Joints[5].GivingNodeJointIndex) then
  begin
    if gvarSample.TotalOutput <> GLSLType1F then
    begin
      // Cast Opacity to float
      vSample.Clear;
      vSample.Category := MaterialSystem.Utility.Name;
      vSample.Name := MaterialSystem.Utility.Utility_ComponentMask;
      vSample.InputRef[0] := gvarSample;
      vSample.Mask := [ccmRed];
      AddToList;
    end;
  end
  else
  begin
    // Default Opacity
    vSample.Clear;
    vSample.Category := MaterialSystem.Constants.Name;
    vSample.Name := MaterialSystem.Constants.Constants_Scalar;
    vSample.Purpose := sspConstant;
    vSample.ConstantValue := ValueToHex(XHmgVector);
    AddToList;
  end;

  // Pass FragColor
  GetOutputSample(vSample);
  vSample.InputRef[0] := gpRGB;
  vSample.InputRef[1] := gvarSample;
  AddToList;
  gpRGB := gvarSample;

  /////////////////////////////////////////////////////////////////
  //----------------------VERTEX PROGRAM-------------------------//
  /////////////////////////////////////////////////////////////////
  gvarProgType := [ptVertex];

  vSample.Clear;
  vSample.Participate := gvarProgType;
  vSample.Purpose := sspVariable;
  vSample.Output := GLSLTypeVRec;
  AddToList;
  pVertexRecord := gvarSample;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.GetVertex;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.TransformVertex_O2W;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.TransformVertex_W2S;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.GetLight;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.GetCamera;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  // Transform Light and Camera vectors to texture space
  if Info.LightingInTextureSpace then
  begin
    vSample.Clear;
    vSample.Category := MaterialSystem.Vertex.Name;
    vSample.Name := MaterialSystem.Vertex.TransformLighting_W2T;
    vSample.InputRef[0] := pVertexRecord;
    AddToList;
  end;

  vSample.Clear;
  vSample.Category := MaterialSystem.Vertex.Name;
  vSample.Name := MaterialSystem.Vertex.PassVertex_V2F;
  vSample.InputRef[0] := pVertexRecord;
  AddToList;

  // Pass TexCoord
  if Info.PassTexCoord then
  begin
    vSample.Clear;
    vSample.Category := MaterialSystem.Vertex.Name;
    vSample.Name := MaterialSystem.Vertex.PassTexCoord_V2F;
    vSample.InputRef[0] := pVertexRecord;
    AddToList;
  end;

  gvarProgType := [ptFragment];
  gvarSample := gpRGB;
  Result := True;
end;

function TMaterialNode.GetFaceCulling: TFaceCulling;
begin
  Result := Owner.FMaterial.FaceCulling;
end;

procedure TMaterialNode.SetFaceCulling(Value: TFaceCulling);
begin
  if Owner.FMaterial.FaceCulling <> Value then
  begin
    Owner.FMaterial.FaceCulling := Value;
    NotifyChange;
  end;
end;

function TMaterialNode.GetPolygonMode: TPolygonMode;
begin
  Result := Owner.FMaterial.PolygonMode;
end;

procedure TMaterialNode.SetPolygonMode(Value: TPolygonMode);
begin
  if Owner.FMaterial.PolygonMode <> Value then
  begin
    Owner.FMaterial.PolygonMode := Value;
    NotifyChange;
  end;
end;

function TMaterialNode.GetBlendingMode: TBlendingMode;
begin
  Result := Owner.FMaterial.BlendingMode;
end;

procedure TMaterialNode.SetBlendingMode(Value: TBlendingMode);
begin
  if Owner.FMaterial.BlendingMode <> Value then
  begin
    Owner.FMaterial.BlendingMode := Value;
    NotifyChange;
  end;
end;

function TMaterialNode.GetOpacityMaskClip: Single;
begin
  Result := FOpacityMaskClip.X;
end;

procedure TMaterialNode.SetOpacityMaskClip(Value: Single);
begin
  FOpacityMaskClip.X := Value;
  NotifyChange;
end;

procedure TMaterialNode.SetLightingModel(Value: TLightingModel);
begin
  if FLightingModel <> Value then
  begin
    FLightingModel := Value;
    NotifyChange;
  end;
end;

function TMaterialNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'LightingModel';
        APropValue := 'Phong';
      end;
    1:
      begin
        APropName := 'OpacityMaskClip';
        APropValue := FloatToStr(FOpacityMaskClip.X);
      end;
  else
    Result := False;
  end;
end;

procedure TMaterialNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'LightingModel' then
  begin
    FLightingModel := lmPhong;
  end
  else if APropName = 'OpacityMaskClip' then
    Val(APropValue, FOpacityMaskClip.V[0], err)
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCustomConstantNode'}{$ENDIF}

constructor TCustomConstantNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  FValue := NullHmgVector;
end;

procedure TCustomConstantNode.SetValue(Index: Integer; Value: Single);
begin
  FValue.V[Index] := Value;
  SetWidth(Width);
  NotifyChange;
end;

function TCustomConstantNode.GetValue(Index: Integer): Single;
begin
  Result := FValue.V[Index];
end;

function TCustomConstantNode.Caption: string;
begin
  if Self is TConstantNode then
    Result := Format('%.6g', [FValue.X])
  else if Self is TConstant2fNode then
    Result := Format('%.6g,%.6g', [FValue.X, FValue.Y])
  else if Self is TConstant3fNode then
    Result := Format('%.6g,%.6g,%.6g', [FValue.X, FValue.Y, FValue.Z])
  else if Self is TConstant4fNode then
    Result := Format('%.6g,%.6g,%.6g,%.6g', [FValue.X, FValue.Y, FValue.Z, FValue.W])
  else
    Assert(False);
end;

function TCustomConstantNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    GetWorkResult_line +
    LBracket_line +
    AnsiString(Format('  return vec4(%g, %g, %g, %g);',
    [FValue.X, FValue.Y, FValue.Z, FValue.W])) + #10#13 +
    RBracket_line;
end;

function TCustomConstantNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'Red';
        APropValue := FloatToStr(FValue.X);
      end;
    1:
      begin
        APropName := 'Green';
        APropValue := FloatToStr(FValue.Y);
      end;
    2:
      begin
        APropName := 'Blue';
        APropValue := FloatToStr(FValue.Z);
      end;
    3:
      begin
        APropName := 'Alpha';
        APropValue := FloatToStr(FValue.W);
      end;
  else
    Result := False;
  end;
end;

procedure TCustomConstantNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'Red' then
    Val(APropValue, FValue.V[0], err)
  else if APropName = 'Green' then
    Val(APropValue, FValue.V[1], err)
  else if APropName = 'Blue' then
    Val(APropValue, FValue.V[2], err)
  else if APropName = 'Alpha' then
    Val(APropValue, FValue.V[3], err);
end;

function TCustomConstantNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Constants.Name;
  if Self is TConstantNode then
  begin
    ASample.Name := MaterialSystem.Constants.Constants_Scalar;
    ASample.Output := GLSLType1F;
  end
  else if Self is TConstant2fNode then
  begin
    ASample.Name := MaterialSystem.Constants.Constants_Vector2;
    ASample.Output := GLSLType2F;
  end
  else if Self is TConstant3fNode then
  begin
    ASample.Name := MaterialSystem.Constants.Constants_Vector3;
    ASample.Output := GLSLType3F;
  end
  else if Self is TConstant4fNode then
  begin
    ASample.Name := MaterialSystem.Constants.Constants_Vector4;
    ASample.Output := GLSLType4F;
  end;
  ASample.Purpose := sspConstant;
  ASample.ConstantValue := ValueToHex(FValue);
  ASample.Participate := [ptVertex, ptGeometry, ptFragment];
  Result := True;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TVertexColorNode'}{$ENDIF}

constructor TVertexColorNode.Create;
begin
  inherited;
  SetLength(FJoints, 5);
  Joints[0].SetAtOnce(0, sideOutput, [ccmWhite], '');
  Joints[1].SetAtOnce(1, sideOutput, [ccmRed], '');
  Joints[2].SetAtOnce(2, sideOutput, [ccmGreen], '');
  Joints[3].SetAtOnce(3, sideOutput, [ccmBlue], '');
  Joints[4].SetAtOnce(4, sideOutput, [ccmAlpha], '');
end;

function TVertexColorNode.Caption: string;
begin
  Result := 'Vertex Color';
end;

function TVertexColorNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    GetWorkResult_line +
    LBracket_line +
    '  return vec4(0.25, 0.5, 0.75, 1.0);' + #10#13 +
    RBracket_line;
end;

function TVertexColorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Constants.Name;
  ASample.Name := MaterialSystem.Constants.Constants_VertexColor;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;

function TVertexColorNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TTextureCoordinateNode'}{$ENDIF}

constructor TTextureCoordinateNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  FValue.X := 1;
  FValue.Y := 1;
end;

procedure TTextureCoordinateNode.SetValue(Index: Integer; Value: Single);
begin
  FValue.V[Index] := Value;
  NotifyChange;
end;

function TTextureCoordinateNode.GetValue(Index: Integer): Single;
begin
  Result := FValue.V[Index];
end;

procedure TTextureCoordinateNode.SetIndex(Value: Integer);
begin
  if Value < 0 then
    Value := 0
  else if Value > 7 then
    Value := 7;

  if Value <> FIndex then
  begin
    FIndex := Value;
    NotifyChange;
  end;
end;

function TTextureCoordinateNode.Caption: string;
begin
  Result := 'TexCoord';
end;

function TTextureCoordinateNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    GetTexCoord_line +
    GetWorkResult_line +
    LBracket_line +
    '  return vec4(GetTexCoord()' + AnsiString(Format(' * vec2(%g, %g) ', [FValue.X, FValue.Y])) + ', 0.0, 1.0);' + #10#13 +
    RBracket_line;
end;

function TTextureCoordinateNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'TilingS';
        APropValue := FloatToStr(FValue.X);
      end;
    1:
      begin
        APropName := 'TilingT';
        APropValue := FloatToStr(FValue.Y);
      end;
    2:
      begin
        APropName := 'Index';
        APropValue := IntToStr(FIndex);
      end;
  else
    Result := False;
  end;
end;

procedure TTextureCoordinateNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'TilingS' then
    Val(APropValue, FValue.V[0], err)
  else if APropName = 'TilingT' then
    Val(APropValue, FValue.V[1], err)
  else if APropName = 'Index' then
    Val(APropValue, FIndex, err)
end;

function TTextureCoordinateNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Coordinates.Name;
  case FIndex of
    0: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord0;
    1: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord1;
    2: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord2;
    3: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord3;
    4: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord4;
    5: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord5;
    6: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord6;
    7: ASample.Name := MaterialSystem.Coordinates.Coordinates_TexCoord7;
  end;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;

function TTextureCoordinateNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample{, vPassSample}: TShaderSample;
//  pSample : PShaderSample;
//  i: TIntVectorEXT;
//  v: TVectorEXT;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
//    vConstSampler.Clear;
//    vConstSampler.Category := MaterialSystem.Constants.Name;
//    vConstSampler.Name := MaterialSystem.Constants.Constants_Scalar;
//    vConstSampler.Purpose := sspConstant;
//    vConstSampler.Participate := gvarProgType;
//    vConstSampler.Output := GLSLType1I;
//    i.X := FIndex;
//    i.Y := 0;
//    i.Z := 0;
//    i.W := 0;
//    vConstSampler.ConstantValue := ValueToHex(i);
//
//    New(pSample);
//    pSample^ := vConstSampler;
//    Info.SampleList.Add(pSample);

//    vSample.InputRef[0] := pSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
    Info.PassTexCoord := True;

//    // Tiling
//    if (FValue.X <> 1.0) or (FValue.Y <> 1.0) then
//    begin
//      New(pConstSampler);
//      pConstSampler.Clear;
//      pConstSampler.Category := MaterialSystem.Constants.Name;
//      pConstSampler.Name := MaterialSystem.Constants.Constants_Vector2;
//      pConstSampler.Purpose := sspConstant;
//      pConstSampler.Participate := [ptVertex];
//      pConstSampler.Output := GLSLType2F;
//      v.X := FValue.X;
//      v.Y := FValue.Y;
//      v.Z := 0;
//      v.W := 0;
//      pConstSampler.ConstantValue := ValueToHex(v);
//      pPassSample.InputRef[0] := pConstSampler;
//    end
//    else
//    begin
//      pConstSampler := nil;
//      pPassSample.Input[0] := GLSLTypeVoid;
//    end;
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TScreenPositionNode'}{$ENDIF}

constructor TScreenPositionNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
end;

function TScreenPositionNode.Caption: string;
begin
  Result := 'Screen Position';
end;

function TScreenPositionNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    GetTexCoord_line +
    GetWorkResult_line +
    LBracket_line +
    '  return gl_FragCoord;' + #10#13 +
    RBracket_line;
end;

function TScreenPositionNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Coordinates.Name;
  ASample.Name := MaterialSystem.Coordinates.Coordinates_ScreenPosition;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TBinarOpMathNode'}{$ENDIF}

constructor TBinarOpMathNode.Create;
begin
  inherited;
  SetLength(FJoints, 3);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'A');
  Joints[2].SetAtOnce(2, sideInput, [], 'B');
end;

procedure TBinarOpMathNode.SetViewingUniforms;
begin
  with ShaderManager do
  begin
    UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
    UniformSampler(uniformTexUnit1, GivingNodeTextureID[2], 1);
  end;
end;

function TBinarOpMathNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing operand A', Result);
  CheckNodeInput(1, 'Missing operand B', Result);
end;

function TBinarOpMathNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;
    if Joints[2].GivingNode.GatherSamples(Info,
      Joints[2].GivingNodeJointIndex) then
      vSample.InputRef[1] := gvarSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TUnarOpMathNode'}{$ENDIF}

constructor TUnarOpMathNode.Create;
begin
  inherited;
  SetLength(FJoints, 2);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'Input');
end;

procedure TUnarOpMathNode.SetViewingUniforms;
begin
  ShaderManager.UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
end;

function TUnarOpMathNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing input', Result);
end;

function TUnarOpMathNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
  end;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TAddNode'}{$ENDIF}

function TAddNode.Caption: string;
begin
  Result := 'Add';
end;

function TAddNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = value01 + value02;' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TAddNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Add;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TSubtractNode'}{$ENDIF}

function TSubtractNode.Caption: string;
begin
  Result := 'Subtract';
end;

function TSubtractNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = value01 - value02;' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TSubtractNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Sub;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TMultiplyNode'}{$ENDIF}

function TMultiplyNode.Caption: string;
begin
  Result := 'Multiply';
end;

function TMultiplyNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = value01 * value02;' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TMultiplyNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := (MaterialSystem.Math.Name);
    ASample.Name := (MaterialSystem.Math.Math_Mul);
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TDivideNode'}{$ENDIF}

function TDivideNode.Caption: string;
begin
  Result := 'Divide';
end;

function TDivideNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = value01 / value02;' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TDivideNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Div;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TPowerNode'}{$ENDIF}

function TPowerNode.Caption: string;
begin
  Result := 'Power';
end;

function TPowerNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = pow(value01, value02);' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TPowerNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Power;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TDotProductNode'}{$ENDIF}

function TDotProductNode.Caption: string;
begin
  Result := 'DotProduct';
end;

function TDotProductNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Input[1]) + ' value02 = ' + GetGLSLTypeCast('value1', GLSLType4F, GivingNodeMask[2], vSample.Input[1]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = dot(value01, value02);' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TDotProductNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_DotProduct;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TNormalizeNode'}{$ENDIF}

function TNormalizeNode.Caption: string;
begin
  Result := 'Normalize';
end;

function TNormalizeNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = normalize(value01);' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TNormalizeNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Normalize;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCustomTrigonNode'}{$ENDIF}

constructor TCustomTrigonNode.Create;
begin
  inherited;
  SetLength(FJoints, 2);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'Input');
  FPeriod := 1.0;
end;

procedure TCustomTrigonNode.SetPeriod(Value: Single);
begin
  FPeriod := Value;
  NotifyChange;
end;

function TCustomTrigonNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  if Index = 0 then
  begin
    APropName := 'Period';
    APropValue := FloatToStr(Period);
  end
  else
    Result := False;
end;

procedure TCustomTrigonNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'Period' then
    Val(APropValue, FPeriod, err);
end;

procedure TCustomTrigonNode.SetViewingUniforms;
begin
  ShaderManager.UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
end;

function TCustomTrigonNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing input', Result);
end;

function TCustomTrigonNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
  pConstSampler: PShaderSample;
  v: TVectorEXT;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);

    // Period
    if gvarSample.Input[1] <> GLSLTypeUndefined then
    begin
      New(pConstSampler);
      pConstSampler.Clear;
      pConstSampler.Category := MaterialSystem.Constants.Name;
      pConstSampler.Name := MaterialSystem.Constants.Constants_Scalar;
      pConstSampler.Purpose := sspConstant;
      pConstSampler.Participate := gvarProgType;
      pConstSampler.Output := gvarSample.Input[1];
      v.X := 1 / FPeriod;
      v.Y := 0;
      v.Z := 0;
      v.W := 0;
      pConstSampler.ConstantValue := ValueToHex(v);
      gvarSample.InputRef[1] := pConstSampler;
      Info.SampleList.Add(pConstSampler);
    end;
  end;
end;

function TSineNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Sine;
    ASample.Input[0] := GivingNodeDataType[1];
    if (Period <> 0.0) and (Period <> 1.0) then
      ASample.Input[1] := GLSLType1F;
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TSineNode.Caption: string;
begin
  Result := 'Sine';
end;

function TSineNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      '  value01 *= ' + AnsiString(Format('%g;', [1 / Period])) + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = sin(value01);' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TCosineNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Cosine;
    ASample.Input[0] := GivingNodeDataType[1];
    if (Period <> 0.0) and (Period <> 1.0) then
      ASample.Input[1] := GLSLType1F;
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TCosineNode.Caption: string;
begin
  Result := 'Cosine';
end;

function TCosineNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GLSLTypeToString(vSample.Input[0]) + ' value01 = ' + GetGLSLTypeCast('value0', GLSLType4F, GivingNodeMask[1], vSample.Input[0]) + ';' + #10#13 +
      '  value01 *= ' + AnsiString(Format('%g;', [1 / Period])) + #10#13 +
      GLSLTypeToString(vSample.Output) + ' result = cos(value01);' + #10#13 +
      '  return ' + GetGLSLTypeCast('result', vSample.Output, [], GLSLType4F) + ';' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TFloorNode'}{$ENDIF}

function TFloorNode.Caption: string;
begin
  Result := 'Floor';
end;

function TFloorNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  return floor(value0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TFloorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Floor;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TAbsNode'}{$ENDIF}

function TAbsNode.Caption: string;
begin
  Result := 'Abs';
end;

function TAbsNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  return abs(value0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TAbsNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Abs;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TFractNode'}{$ENDIF}

function TFractNode.Caption: string;
begin
  Result := 'Fract';
end;

function TFractNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  return fract(value0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TFractNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_Fract;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TOneMinusNode'}{$ENDIF}

function TOneMinusNode.Caption: string;
begin
  Result := '1 - x';
end;

function TOneMinusNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  return (vec4(1.0) - value0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TOneMinusNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_OneMinus;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TSquareRootNode'}{$ENDIF}

function TSquareRootNode.Caption: string;
begin
  Result := 'Square Root';
end;

function TSquareRootNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  return sqrt(value0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

function TSquareRootNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Math.Name;
    ASample.Name := MaterialSystem.Math.Math_SquareRoot;
    ASample.Input[0] := GivingNodeDataType[1];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TTextureSamplerNode'}{$ENDIF}

constructor TTextureSamplerNode.Create;
begin
  inherited;
  SetLength(FJoints, 6);
  Joints[0].SetAtOnce(0, sideOutput, [ccmWhite], '');
  Joints[1].SetAtOnce(1, sideOutput, [ccmRed], '');
  Joints[2].SetAtOnce(2, sideOutput, [ccmGreen], '');
  Joints[3].SetAtOnce(3, sideOutput, [ccmBlue], '');
  Joints[4].SetAtOnce(4, sideOutput, [ccmAlpha], '');
  Joints[5].SetAtOnce(0, sideInput, [], 'UVs');
  FTexture := glsDIFFUSEMAP;
  FSampler := TGLSamplerParam.Create(Self);
end;

destructor TTextureSamplerNode.Destroy;
begin
  Owner.UnRegisterTexture(FTexture);
  inherited;
end;

function TTextureSamplerNode.Caption: string;
begin
  Result := 'Texture Sampler';
end;

function TTextureSamplerNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue1_line +
      '  return texture(TexUnit0, value1.st);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

procedure TTextureSamplerNode.SetViewingUniforms;
begin
  MaterialManager.ApplyTexture(FTexture, 0);
  ShaderManager.UniformSampler(uniformTexUnit1, GivingNodeTextureID[5], 1);
end;

function TTextureSamplerNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(5, 'Missing input', Result);
end;

function TTextureSamplerNode.GetOutputSample(out ASample: TShaderSample): Boolean;
var
  I: Integer;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Texture.Name;
    I := Owner.GetTextureUnitIndex(FTexture);
    case I of
      0: ASample.Name := MaterialSystem.Texture.Texture_Sampler0;
      1: ASample.Name := MaterialSystem.Texture.Texture_Sampler1;
      2: ASample.Name := MaterialSystem.Texture.Texture_Sampler2;
      3: ASample.Name := MaterialSystem.Texture.Texture_Sampler3;
      4: ASample.Name := MaterialSystem.Texture.Texture_Sampler4;
      5: ASample.Name := MaterialSystem.Texture.Texture_Sampler5;
      6: ASample.Name := MaterialSystem.Texture.Texture_Sampler6;
      7: ASample.Name := MaterialSystem.Texture.Texture_Sampler7;
    end;
    ASample.Input[0] := GivingNodeDataType[5];
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TTextureSamplerNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[5].GivingNode.GatherSamples(Info,
      Joints[5].GivingNodeJointIndex) then
    begin
      vSample.InputRef[0] := gvarSample;
      vSample.TextureInIntput := True;
      New(gvarSample);
      gvarSample^ := vSample;
      Info.SampleList.Add(gvarSample);
    end
    else
      Result := False;
  end;
end;

procedure TTextureSamplerNode.SetTexture(const AName: TGL3xTextureName);
begin
  if AName <> FTexture then
  begin
    Owner.UnRegisterTexture(FTexture);
    Owner.RegisterTexture(AName);
    FTexture := AName;
    NotifyChange;
  end;
end;

function TTextureSamplerNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'Texture';
        APropValue := FTexture;
      end;
  else
    Result := False;
  end;
end;

procedure TTextureSamplerNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
begin
  if APropName = 'Texture' then
    SetTexture(APropValue);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TPannerNode'}{$ENDIF}

constructor TPannerNode.Create;
begin
  inherited;
  SetLength(FJoints, 3);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'Coords');
  Joints[2].SetAtOnce(2, sideInput, [], 'Timer');
  FValue.X := 1;
  FValue.Y := 1;
end;

procedure TPannerNode.SetValue(Index: Integer; Value: Single);
begin
  FValue.V[Index] := Value;
  NotifyChange;
end;

function TPannerNode.GetValue(Index: Integer): Single;
begin
  Result := FValue.V[Index];
end;

function TPannerNode.Caption: string;
begin
  Result := 'Panner';
end;

function TPannerNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      '  value1 *= ' + AnsiString(Format('vec4(%g, %g, 0.0, 0.0);', [FValue.X, FValue.Y])) + #10#13 +
      '  value1 = fract(value1);' + #10#13 +
      '  value0 += value1;' + #10#13 +
      '  return value0;' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

procedure TPannerNode.SetViewingUniforms;
begin
  with ShaderManager do
  begin
    UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
    UniformSampler(uniformTexUnit1, GivingNodeTextureID[2], 1);
  end;
end;

function TPannerNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing Coords input', Result);
  CheckNodeInput(2, 'Missing Timer input', Result);
end;

function TPannerNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := (MaterialSystem.Coordinates.Name);
    ASample.Name := (MaterialSystem.Coordinates.Coordinates_Panner);
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    if (FValue.X <> 1.0) or (FValue.Y <> 1.0) then
      ASample.Input[2] := GLSLType2F;
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TPannerNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
  pConstSampler: PShaderSample;
  v: TVectorEXT;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;

    if Joints[2].GivingNode.GatherSamples(Info,
      Joints[2].GivingNodeJointIndex) then
      vSample.InputRef[1] := gvarSample;

    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
    // Speed
    if gvarSample.Input[2] <> GLSLTypeUndefined then
    begin
      New(pConstSampler);
      pConstSampler.Clear;
      pConstSampler.Category := (MaterialSystem.Constants.Name);
      pConstSampler.Name := (MaterialSystem.Constants.Constants_Vector2);
      pConstSampler.Purpose := sspConstant;
      pConstSampler.Participate := gvarProgType;
      pConstSampler.Output := GLSLType2F;
      v.X := FValue.X;
      v.Y := FValue.Y;
      v.Z := 0;
      v.W := 0;
      pConstSampler.ConstantValue := ValueToHex(v);
      gvarSample.InputRef[2] := pConstSampler;
      Info.SampleList.Add(pConstSampler);
    end;
  end;
end;

function TPannerNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'SpeedS';
        APropValue := FloatToStr(FValue.X);
      end;
    1:
      begin
        APropName := 'SpeedT';
        APropValue := FloatToStr(FValue.Y);
      end;
  else
    Result := False;
  end;
end;

procedure TPannerNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'SpeedS' then
    Val(APropValue, FValue.V[0], err)
  else if APropName = 'SpeedT' then
    Val(APropValue, FValue.V[1], err);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TRotatorNode'}{$ENDIF}

constructor TRotatorNode.Create;
begin
  inherited;
  SetLength(FJoints, 3);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'Coords');
  Joints[2].SetAtOnce(2, sideInput, [], 'Timer');
  FValue := VectorMakeEXT(0.5, 0.5, 0.25);
end;

procedure TRotatorNode.SetValue(Index: Integer; Value: Single);
begin
  FValue.V[Index] := Value;
  NotifyChange;
end;

function TRotatorNode.GetValue(Index: Integer): Single;
begin
  Result := FValue.V[Index];
end;

function TRotatorNode.Caption: string;
begin
  Result := 'Rotator';
end;

function TRotatorNode.GetViewingCode: AnsiString;
var
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      GetValue1_line +
      '  value0 -= ' + AnsiString(Format('vec4(%g, %g, 0.0, 0.0);', [FValue.X, FValue.Y])) + #10#13 +
      '  value1 *= ' + AnsiString(Format('%g;', [FValue.Z])) + #10#13 +
      '  value1 = 6.2831853 * fract(value1);' + #10#13 +
      '  vec2 sincos = vec2(sin(value1.x), cos(value1.x));' + #10#13 +
      '  vec2 rxy = vec2(dot(vec2(sincos.y, -sincos.x), value0.st), dot(sincos, value0.st));' + #10#13 +
      '  rxy += ' + AnsiString(Format('vec2(%g, %g);', [FValue.X, FValue.Y])) + #10#13 +
      '  return vec4(rxy, 0.0, 0.0);' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

procedure TRotatorNode.SetViewingUniforms;
begin
  with ShaderManager do
  begin
    UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
    UniformSampler(uniformTexUnit1, GivingNodeTextureID[2], 1);
  end;
end;

function TRotatorNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing Coords input', Result);
  CheckNodeInput(2, 'Missing Timer input', Result);
end;

function TRotatorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := (MaterialSystem.Coordinates.Name);
    ASample.Name := (MaterialSystem.Coordinates.Coordinates_Rotator);
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Input[1] := GivingNodeDataType[2];
    if (FValue.X <> 0.0) or (FValue.Y <> 0.0) then
      ASample.Input[2] := GLSLType2F;
    if FValue.Z <> 1.0 then
      ASample.Input[3] := GLSLType1F;
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TRotatorNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
  pConstSampler: PShaderSample;
  v: TVectorEXT;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;

    if Joints[2].GivingNode.GatherSamples(Info,
      Joints[2].GivingNodeJointIndex) then
      vSample.InputRef[1] := gvarSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
    // CenterXY
    if gvarSample.Input[2] <> GLSLTypeUndefined then
    begin
      New(pConstSampler);
      pConstSampler.Clear;
      pConstSampler.Category := MaterialSystem.Constants.Name;
      pConstSampler.Name := MaterialSystem.Constants.Constants_Vector2;
      pConstSampler.Purpose := sspConstant;
      pConstSampler.Participate := gvarProgType;
      pConstSampler.Output := GLSLType2F;
      v.X := FValue.X;
      v.Y := FValue.Y;
      v.Z := 0;
      v.W := 0;
      pConstSampler.ConstantValue := ValueToHex(v);
      gvarSample.InputRef[2] := pConstSampler;
      Info.SampleList.Add(pConstSampler);
    end;
    // Speed
    if gvarSample.Input[3] <> GLSLTypeUndefined then
    begin
      New(pConstSampler);
      pConstSampler.Clear;
      pConstSampler.Category := MaterialSystem.Constants.Name;
      pConstSampler.Name := MaterialSystem.Constants.Constants_Scalar;
      pConstSampler.Purpose := sspConstant;
      pConstSampler.Participate := gvarProgType;
      pConstSampler.Output := GLSLType1F;
      v.X := FValue.Z;
      v.Y := 0;
      v.Z := 0;
      v.W := 0;
      pConstSampler.ConstantValue := ValueToHex(v);
      gvarSample.InputRef[3] := pConstSampler;
      Info.SampleList.Add(pConstSampler);
    end;
  end;
end;

function TRotatorNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
begin
  Result := True;
  case Index of
    0:
      begin
        APropName := 'CenterX';
        APropValue := FloatToStr(FValue.X);
      end;
    1:
      begin
        APropName := 'CenterY';
        APropValue := FloatToStr(FValue.Y);
      end;
    2:
      begin
        APropName := 'Speed';
        APropValue := FloatToStr(FValue.Z);
      end;
  else
    Result := False;
  end;
end;

procedure TRotatorNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err: Integer;
begin
  if APropName = 'CenterX' then
    Val(APropValue, FValue.V[0], err)
  else if APropName = 'CenterY' then
    Val(APropValue, FValue.V[1], err)
  else if APropName = 'Speed' then
    Val(APropValue, FValue.V[2], err);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TTimerNode'}{$ENDIF}

constructor TTimerNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  uniformTime := TGLSLUniform.RegisterUniform('Time');
end;

function TTimerNode.Caption: string;
begin
  Result := 'Timer';
end;

function TTimerNode.GetViewingCode: AnsiString;
begin
  Result :=
    GetMaxGLSLVersion + #10#13 +
    'uniform float Time;' + #10#13 +
    GetWorkResult_line +
    LBracket_line +
    '  return vec4(Time);' + #10#13 +
    RBracket_line;
end;

procedure TTimerNode.SetViewingUniforms;
begin
  ShaderManager.Uniform1f(uniformTime, Now);
end;

function TTimerNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Utility.Name;
  ASample.Name := MaterialSystem.Utility.Utility_Timer;
  ASample.Input[0] := GLSLTypeVoid;
  ASample.UniformClasses[0] := TUniformTime;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TComponentMaskNode'}{$ENDIF}

constructor TComponentMaskNode.Create;
begin
  inherited;
  SetLength(FJoints, 2);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
  Joints[1].SetAtOnce(1, sideInput, [], 'Input');
  FMask := [ccmRed, ccmGreen];
end;

function TComponentMaskNode.Caption: string;
begin
  Result := 'ComponentMask';
end;

function TComponentMaskNode.CheckInput: Boolean;
begin
  Result := True;
  CheckNodeInput(1, 'Missing input input', Result);
  if FMask = [] then
    Owner.SetErrorMessage('Invalid color component set in node ' + Caption);
end;

function TComponentMaskNode.GetViewingCode: AnsiString;
var
  I, count: Integer;
  vSample: TShaderSample;
begin
  if GetOutputSample(vSample) then
  begin
    Result :=
      GetMaxGLSLVersion + #10#13 +
      UniformSampler_line +
      GetTexCoord_line +
      GetWorkResult_line +
      LBracket_line +
      GetValue0_line +
      '  vec4 masked = vec4(';

    count := 0;
    if ccmRed in FMask then
    begin
      Result := Result + 'value0.r, ';
      Inc(count);
    end;
    if ccmGreen in FMask then
    begin
      Result := Result + 'value0.g, ';
      Inc(count);
    end;
    if ccmBlue in FMask then
    begin
      Result := Result + 'value0.b, ';
      Inc(count);
    end;
    if ccmAlpha in FMask then
    begin
      Result := Result + 'value0.a, ';
      Inc(count);
    end;

    for I := 1 to 4 - count do
      Result := Result + '0.0, ';

    Delete(Result, Length(Result) - 1, 2);
    Result := Result + ');' + #10#13;

    Result := Result +
      '  return masked;' + #10#13 +
      RBracket_line;
  end
  else
    Result := inherited GetViewingCode;
end;

procedure TComponentMaskNode.SetViewingUniforms;
begin
  ShaderManager.UniformSampler(uniformTexUnit0, GivingNodeTextureID[1], 0);
end;

function TComponentMaskNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  if CheckInput then
  begin
    ASample.Clear;
    ASample.Category := MaterialSystem.Utility.Name;
    ASample.Name := MaterialSystem.Utility.Utility_ComponentMask;
    ASample.Input[0] := GivingNodeDataType[1];
    ASample.Mask := FMask;
    Result := MaterialManager.FindCompatibleSample(ASample);
    CheckOutput(Result);
  end
  else
    Result := False;
end;

function TComponentMaskNode.DoGatherSamples(var Info: TSampleGatherInfo): Boolean;
var
  vSample: TShaderSample;
begin
  Result := GetOutputSample(vSample);
  if Result then
  begin
    if Joints[1].GivingNode.GatherSamples(Info,
      Joints[1].GivingNodeJointIndex) then
      vSample.InputRef[0] := gvarSample;
    New(gvarSample);
    gvarSample^ := vSample;
    Info.SampleList.Add(gvarSample);
  end;
end;

procedure TComponentMaskNode.SetMask(Index: Integer; Value: Boolean);
begin
  case Index of
    0: if Value then
        Include(FMask, ccmRed)
      else
        Exclude(FMask, ccmRed);
    1: if Value then
        Include(FMask, ccmGreen)
      else
        Exclude(FMask, ccmGreen);
    2: if Value then
        Include(FMask, ccmBlue)
      else
        Exclude(FMask, ccmBlue);
    3: if Value then
        Include(FMask, ccmAlpha)
      else
        Exclude(FMask, ccmAlpha);
  end;
  NotifyChange;
end;

function TComponentMaskNode.GetMask(Index: Integer): Boolean;
begin
  case Index of
    0: Result := ccmRed in FMask;
    1: Result := ccmGreen in FMask;
    2: Result := ccmBlue in FMask;
    3: Result := ccmAlpha in FMask;
  else
    Result := False;
  end;
end;

function TComponentMaskNode.GetProperty(Index: Integer; out APropName: string; out APropValue: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Index = 0 then
  begin
    APropName := 'Mask';
    i := 0;
    if ccmRed in FMask then
      i := i + 1;
    if ccmGreen in FMask then
      i := i + 2;
    if ccmBlue in FMask then
      i := i + 4;
    if ccmAlpha in FMask then
      i := i + 8;
    APropValue := IntToStr(i);
  end
  else
    Result := False;
end;

procedure TComponentMaskNode.SetProperty(Index: Integer; const APropName: string; const APropValue: string);
var
  err, i: Integer;
begin
  if APropName = 'Mask' then
  begin
    Val(APropValue, i, err);
    FMask := [];
    if (i and 1) <> 0 then
      Include(FMask, ccmRed);
    if (i and 2) <> 0 then
      Include(FMask, ccmGreen);
    if (i and 4) <> 0 then
      Include(FMask, ccmBlue);
    if (i and 8) <> 0 then
      Include(FMask, ccmAlpha);
  end;
end;

{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TWorldNormalNode'}{$ENDIF}

constructor TWorldNormalNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
end;

function TWorldNormalNode.Caption: string;
begin
  Result := 'World Normal';
end;

function TWorldNormalNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Vectors.Name;
  ASample.Name := MaterialSystem.Vectors.Vectors_WorldNormal;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TLightVectorNode'}{$ENDIF}

constructor TLightVectorNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
end;

function TLightVectorNode.Caption: string;
begin
  Result := 'LightVector';
end;

function TLightVectorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Vectors.Name;
  ASample.Name := MaterialSystem.Vectors.Vectors_LightVector;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TCameraVectorNode'}{$ENDIF}

constructor TCameraVectorNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
end;

function TCameraVectorNode.Caption: string;
begin
  Result := 'Camera Vector';
end;

function TCameraVectorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Vectors.Name;
  ASample.Name := MaterialSystem.Vectors.Vectors_CameraVector;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'TReflectionVectorNode'}{$ENDIF}

constructor TReflectionVectorNode.Create;
begin
  inherited;
  SetLength(FJoints, 1);
  Joints[0].SetAtOnce(0, sideOutput, [], '');
end;

function TReflectionVectorNode.Caption: string;
begin
  Result := 'Reflection Vector';
end;

function TReflectionVectorNode.GetOutputSample(out ASample: TShaderSample): Boolean;
begin
  ASample.Clear;
  ASample.Category := MaterialSystem.Vectors.Name;
  ASample.Name := MaterialSystem.Vectors.Vectors_ReflectionVector;
  ASample.Input[0] := GLSLTypeVoid;
  Result := MaterialManager.FindCompatibleSample(ASample);
  CheckOutput(Result);
end;
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

initialization
  RegisterClasses([
    TMaterialNode, TPannerNode, TRotatorNode, TAddNode, TSubtractNode,
      TMultiplyNode, TDivideNode, TNormalizeNode, TTextureSamplerNode,
      TTimerNode, TConstantNode, TConstant2fNode, TConstant3fNode, TVertexColorNode,
      TConstant4fNode, TTextureCoordinateNode, TPowerNode, TDotProductNode,
      TWorldNormalNode, TLightVectorNode, TCameraVectorNode, TReflectionVectorNode,
      TComponentMaskNode, TSineNode, TCosineNode, TFloorNode, TAbsNode, TFractNode,
      TOneMinusNode, TSquareRootNode, TScreenPositionNode]);

end.

