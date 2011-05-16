//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSNishitaSky <p>

   GPU Crysis Atmospheric scattering.<p>
   There is two methods for update: <p>
    the first one just compute all nasty integral and <p>
    the other (FastUpdate) one use a precomputed texture to speed up thing <p>
    at the cost of loosing some quality when sun is near horizon.<p>

   <b>History : </b><font size=-1><ul>
      <li>16/05/11 - Yar - Transition to indirect rendering objects
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>08/03/10 - Yar - Creation
   </ul></font><p>
}

unit GLSNishitaSky;

interface

{$I GLScene.inc}
{.$DEFINE NISHITA_SKY_DEBUG_MODE}

uses
  // VCL
  SysUtils,
  Classes,

  // GLScene
  GLScene,
  GLCadencer,
  OpenGLTokens,
  VectorGeometry,
  VectorTypes,
  GLState,
  GLContext,
  GLTexture,
  GLGraphics,
  GLTextureFormat,
  GLSMesh,
  GLSDrawTechnique,
  GLMaterial,
  GLMaterialEx,
  GLRenderContextInfo,
  GLPipelineTransformation;

type

  TGLNishitaSkyChange =
  (
    nscConstants,
    nscRayleighMie,
    nscOpticalDepth,
    nscTime
  );

  TGLNishitaSkyChanges = set of TGLNishitaSkyChange;
  TGLNishitaSkyTexPrec = (nsp11bit, nsp16bit, nsp32bit);

  TNSConstantBlock = packed record
    WavelengthMie: TVector4f;
    v3HG: TVector4f;
    InvWavelength4: TVector4f;
    v2dRayleighMieScaleHeight: TVector2f;
    InvRayleighMieN: TVector2f;
    InvRayleighMieNLessOne: TVector2f;
    PI: Single;
    InnerRadius: Single;
    OuterRadius: Single;
    fScale: Single;
    KrESun: Single;
    KmESun: Single;
    Kr4PI: Single;
    Km4PI: Single;
    InvOpticalDepthN: Single;
    InvOpticalDepthNLessOne: Single;
    HalfTexelOpticalDepthN: Single;
    tNumSamples: Integer;
    iNumSamples: Integer;
    FakeConstForPadding: Single;
  end;

  TGLCustomNishitaSky = class(TGLBaseSceneObject)
  private
    { Private Declarations }
    FSoftwareMode: Boolean;
    FBatch: TDrawBatch;
    FScreenQuadBatch: TDrawBatch;
    FTransformation: TTransformationRec;
    ConstantBlock: TNSConstantBlock;
    FUBO: TGLUniformBufferHandle;
    FDomeDiv: Integer;
    FOpticalDepthN: Integer;
    FRayleighMieN: Integer;
    FMieTexture: TGLFrameBufferAttachment;
    FRayleighTexture: TGLFrameBufferAttachment;
    FOpticalDepthTexture: TGLFrameBufferAttachment;
    FCommonSampler: TGLTextureSampler;
    FOpticalDepthFBO: TGLFramebufferHandle;
    FRayleighMieFBO: TGLFramebufferHandle;
    v3SunDir: TAffineVector;
    FOclock: Double;
    FFastUpdate: Boolean;
    FColorPrecision: TGLNishitaSkyTexPrec;
    FChanges: TGLNishitaSkyChanges;
    FUpdateMaterial: TGLLibMaterialEx;
    FUpdateFastMaterial: TGLLibMaterialEx;
    FCreateOpticalDepthMaterial: TGLLibMaterialEx;
    FSun: TGLLightSource;
    procedure SetOclock(Value: Double);
    function StoreOclock: Boolean;
    procedure SetDomeDivision(Value: Integer);
    procedure SetOpticalDepth(Value: Integer);
    procedure SetRayleighMie(Value: Integer);
    procedure SetColorPrecision(const Value: TGLNishitaSkyTexPrec);
    function GetMieScaleHeight: Single;
    function GetRayleighScaleHeight: Single;
    procedure SetMieScaleHeight(Value: Single);
    procedure SetRayleighScaleHeight(Value: Single);
    function StoreMieScaleHeight: Boolean;
    function StoreRayleighScaleHeight: Boolean;
    procedure InitializeRenderShader(Sender: TGLBaseShaderModel);
    procedure SM3_Uniform_Settig(Sender: TGLBaseShaderModel; var ARci: TRenderContextInfo);
    procedure SM4_Uniform_Settig(Sender: TGLBaseShaderModel; var ARci: TRenderContextInfo);
    procedure DoOnPrepareOpticalDepthFBO(Sender: TGLContext);
    procedure DoOnPrepareRayleighMieFBO(Sender: TGLContext);
    procedure SetSun(const Value: TGLLightSource);
  protected
    { Protected Declarations }
    procedure Update(var ARci: TRenderContextInfo);
    procedure MakeGPUOpticalDepth(var ARci: TRenderContextInfo);
    procedure MakeGPUMieRayleighBuffer(var ARci: TRenderContextInfo);
    procedure BuildMesh;
    procedure SetScene(const value: TGLScene); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    {: Light source (must be parallel) to update it direction according Oclock. }
    property Sun: TGLLightSource read FSun write SetSun;
    {: Time of day in 24 hours format. }
    property Oclock: Double read FOclock write SetOclock stored StoreOclock;
    {: Turn on fast updating GPU algorithm, but reduce quality. }
    property FastUpdate: Boolean read FFastUpdate write FFastUpdate
      default False;
    property DomeDivision: Integer read FDomeDiv write SetDomeDivision
      default 128;
    property OpticalDepthSize: Integer read FOpticalDepthN write SetOpticalDepth
      default 256;
    property RayleighMieSize: Integer read FRayleighMieN write SetRayleighMie
      default 256;
    property ColorPrecision: TGLNishitaSkyTexPrec read FColorPrecision write
      SetColorPrecision default nsp11bit;
    property MieScaleHeight: Single read GetMieScaleHeight write
      SetMieScaleHeight stored StoreMieScaleHeight;
    property RayleighScaleHeight: Single read GetRayleighScaleHeight write
      SetRayleighScaleHeight stored StoreRayleighScaleHeight;
  end;

  TGLNishitaSky = class(TGLCustomNishitaSky)
  published
    property Oclock;
    property FastUpdate;
    property DomeDivision;
    property OpticalDepthSize;
    property RayleighMieSize;
    property ColorPrecision;
    property MieScaleHeight;
    property RayleighScaleHeight;

    property Position;
    property Direction;
    property PitchAngle;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property ObjectsSorting;
    property OnProgress;
  end;

implementation

uses
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  GLFileDDS,
{$ENDIF}
  GLSLog,
  GLSLParameter;

const
  cDrawBuffers: array[0..1] of GLenum =
    (
    GL_COLOR_ATTACHMENT0,
    GL_COLOR_ATTACHMENT1
    );

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}
const

  Header120 =
    '#version 120'#10#13+
    '#define SM4 0'#10#13+
    '#define VERTEX_IN attribute'#10#13+
    '#define VERTEX_OUT varying'#10#13+
    '#define FRAGMENT_IN varying'#10#13+
    '#define TEXTURE texture2D'#10#13;

  Header330 =
    '#version 330'#10#13 +
    'precision highp float;'#10#13+
    '#define SM4 1'#10#13+
    '#define VERTEX_IN in'#10#13+
    '#define VERTEX_OUT out'#10#13+
    '#define FRAGMENT_IN in'#10#13+
    '#define TEXTURE texture'#10#13;

  Uniforms =
    '#if (SM4 == 1)'#10#13 +
    'layout(std140) uniform ConstantBlock {'#10#13 +
    '#endif'#10#13 +
    ' uniform vec3 WavelengthMie;'#10#13 +
    ' uniform vec3 v3HG;'#10#13 +
    ' uniform vec3 InvWavelength4;'#10#13 +
    ' uniform vec2 v2dRayleighMieScaleHeight;'#10#13 +
    ' uniform vec2 InvRayleighMieN;'#10#13 +
    ' uniform vec2 InvRayleighMieNLessOne;'#10#13 +
    ' uniform float PI;'#10#13 +
    ' uniform float InnerRadius;'#10#13 +
    ' uniform float OuterRadius;'#10#13 +
    ' uniform float fScale;'#10#13 +
    ' uniform float KrESun;'#10#13 +
    ' uniform float KmESun;'#10#13 +
    ' uniform float Kr4PI;'#10#13 +
    ' uniform float Km4PI;'#10#13 +
    ' uniform float InvOpticalDepthN;'#10#13 +
    ' uniform float InvOpticalDepthNLessOne;'#10#13 +
    ' uniform float HalfTexelOpticalDepthN;'#10#13 +
    ' uniform int tNumSamples;'#10#13 +
    ' uniform int iNumSamples;'#10#13 +
    '#if (SM4 == 1)'#10#13 +
    '};'#10#13 +
    '#endif'#10#13;

  Share_fp =
    'vec2 GetDensityRatio( float fHeight )'#10#13 +
    '{'#10#13 +
    '	float fAltitude = (fHeight - InnerRadius) * fScale;'#10#13 +
    '	return exp( vec2(-fAltitude) / v2dRayleighMieScaleHeight.xy );'#10#13 +
    '}'#10#13 +
    'vec2 t( vec3 P, vec3 Px )'#10#13 +
    '{'#10#13 +
    '	vec2 OpticalDepth = vec2(0.0);'#10#13 +
    '	vec3 v3Vector = Px - P;'#10#13 +
    '	float fFar = length( v3Vector );'#10#13 +
    '	vec3 v3Dir = v3Vector / fFar;'#10#13 +
    '	float fSampleLength = fFar / float(tNumSamples);'#10#13 +
    '	float fScaledLength = fSampleLength * fScale;'#10#13 +
    '	vec3 v3SampleRay = v3Dir * fSampleLength;'#10#13 +
    '	P += v3SampleRay * 0.5;'#10#13 +
    '	for(int i = 0; i < tNumSamples; i++)'#10#13 +
    '	{'#10#13 +
    '		float fHeight = length( P );'#10#13 +
    '		OpticalDepth += GetDensityRatio( fHeight );'#10#13 +
    '		P += v3SampleRay;'#10#13 +
    '	}'#10#13 +
    '	OpticalDepth *= fScaledLength;'#10#13 +
    '	return OpticalDepth;'#10#13 +
    '}'#10#13 +
    'float HitOuterSphere( vec3 O, vec3 Dir )'#10#13 +
    '{'#10#13 +
    '	vec3 L = -O;'#10#13 +
    '	float B = dot( L, Dir );'#10#13 +
    '	float C = dot( L, L );'#10#13 +
    '	float D = C - B * B;'#10#13 +
    '	float q = sqrt( OuterRadius * OuterRadius - D );'#10#13 +
    '	float t = B;'#10#13 +
    '	t += q;'#10#13 +
    '	return t;'#10#13 +
    '}'#10#13;

  Render_vp =
    'VERTEX_IN vec3 Position;'#10#13 +
    'VERTEX_IN vec2 TexCoord0;'#10#13 +
    'VERTEX_OUT vec2 v2f_TexCoord;'#10#13 +
    'VERTEX_OUT vec3 v2f_ObjPos;'#10#13 +
    'uniform mat4 ViewProjectionMatrix;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '	v2f_ObjPos = -Position;'#10#13 +
    '	v2f_TexCoord = TexCoord0;'#10#13 +
    '	gl_Position = ViewProjectionMatrix * vec4(Position, 1.0);'#10#13 +
    '}';

  Update_vp =
    'VERTEX_IN vec2 Position;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    '	gl_Position = vec4(Position, 0.0, 1.0);'#10#13 +
    '}';

  Render_fp =
    'const float fExposure = -2.0;'#10#13 +
    'vec3 HDR( vec3 LDR)'#10#13 +
    '{'#10#13 +
    '	return 1.0 - exp( fExposure * LDR );'#10#13 +
    '}'#10#13 +
    'float getMiePhase(float fCos, float fCos2)'#10#13 +
    '{'#10#13 +
    '	return v3HG.x * (1.0 + fCos2) / pow(v3HG.y - v3HG.z * fCos, 1.5);'#10#13 +
    '}'#10#13 +
    'float getRayleighPhase(float fCos2)'#10#13 +
    '{'#10#13 +
    '	return 0.75 * (1.0 + fCos2);'#10#13 +
    '}'#10#13 +

    'FRAGMENT_IN vec3 v2f_ObjPos;'#10#13 +
    'FRAGMENT_IN vec2 v2f_TexCoord;'#10#13 +
    'uniform sampler2D Mie;'#10#13 +
    'uniform sampler2D Rayleigh;'#10#13 +
    'uniform vec3 v3SunDir;'#10#13 +
    '#if (SM4 == 1)'#10#13 +
    'out vec4 FragColor;'#10#13 +
    '#else'#10#13+
    '#define FragColor gl_FragColor'#10#13+
    '#endif'#10#13+

    'void main()'#10#13 +
    '{'#10#13 +
    ' float fCos = dot( v3SunDir, v2f_ObjPos ) / length( v2f_ObjPos );'#10#13 +
    ' float fCos2 = fCos * fCos;'#10#13 +
    ' vec3 v3RayleighSamples = TEXTURE(Rayleigh, v2f_TexCoord).rgb;'#10#13 +
    ' vec3 v3MieSamples = TEXTURE(Mie, v2f_TexCoord).rgb;'#10#13 +
    ' FragColor.rgb = getRayleighPhase(fCos2) * v3RayleighSamples.rgb +'#10#13 +
    '  getMiePhase(fCos, fCos2) * v3MieSamples.rgb;'#10#13 +
    ' FragColor = vec4(HDR( FragColor.rgb ), 1.0);'#10#13 +
    '}';

  Update_fp =
    '#if (SM4 == 1)'#10#13 +
    'out vec4 Mie;'#10#13 +
    'out vec4 RayLeigh;'#10#13 +
    '#else'#10#13+
    '#define Mie gl_FragData[0]'#10#13+
    '#define RayLeigh gl_FragData[1]'#10#13+
    '#endif'#10#13+
    'uniform vec3 v3SunDir;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * InvRayleighMieNLessOne.xy;'#10#13 +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 1e-3, 0.0 );'#10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;'#10#13 +
    ' float AngleXZ = PI * Tex0.y;'#10#13 +
    ' vec3 v3Dir;'#10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );'#10#13 +
    ' v3Dir.y = cos( AngleY );'#10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );'#10#13 +
    ' v3Dir = normalize( v3Dir );'#10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );'#10#13 +
    ' vec3 v3Ray = v3Dir;'#10#13 +
    ' vec3 v3PointP = v3PointPv;'#10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;'#10#13 +
    ' float fScaledLength = fSampleLength * fScale;'#10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;'#10#13 +
    ' v3PointP += v3SampleRay * 0.5;'#10#13 +
    ' vec3 v3RayleighSum = vec3(0.0);'#10#13 +
    ' vec3 v3MieSum = vec3(0.0);'#10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )'#10#13 +
    ' {'#10#13 +
    ' 	float PointPHeight = length( v3PointP );'#10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );'#10#13 +
    ' 	DensityRatio *= fScaledLength;'#10#13 +
    ' 	vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );'#10#13 +
    ' 	float dFarPPc = HitOuterSphere( v3PointP, v3SunDir );'#10#13 +
    ' 	vec2 SunOpticalDepth = t( v3PointP, v3PointP + v3SunDir * dFarPPc );'#10#13 +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;'#10#13  +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );'#10#13 +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;'#10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;'#10#13 +
    ' 	v3PointP += v3SampleRay;'#10#13 +
    ' }'#10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );'#10#13 +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );'#10#13 +
    '}';

  UpdateFast_fp =
    '#if (SM4 == 1)'#10#13 +
    'out vec4 Mie;'#10#13 +
    'out vec4 RayLeigh;'#10#13 +
    '#else'#10#13+
    '#define Mie gl_FragData[0]'#10#13+
    '#define RayLeigh gl_FragData[1]'#10#13+
    '#endif'#10#13+
    'uniform sampler2D OpticalDepth;'#10#13 +
    'uniform vec3 v3SunDir;'#10#13 +
    'void main()'#10#13 +
    '{'#10#13 +
    ' vec2 pos = gl_FragCoord.xy - vec2(0.5);'#10#13 +
    ' vec2 Tex0 = pos * InvRayleighMieNLessOne.xy;'#10#13 +
    ' float Tex1x = pos.x * InvRayleighMieN.x + HalfTexelOpticalDepthN;'#10#13 +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );'#10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;'#10#13 +
    ' float AngleXZ = PI * Tex0.y;'#10#13 +

    ' vec3 v3Dir;'#10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );'#10#13 +
    ' v3Dir.y = cos( AngleY );'#10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );'#10#13 +
    ' v3Dir = normalize( v3Dir );'#10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );'#10#13 +
    ' vec3 v3Ray = v3Dir;'#10#13 +
    ' vec3 v3PointP = v3PointPv;'#10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;'#10#13 +
    ' float fScaledLength = fSampleLength * fScale;'#10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;'#10#13 +
    ' v3PointP += v3SampleRay * 0.5;'#10#13 +

    ' vec3 v3RayleighSum = vec3(0.0);'#10#13 +
    ' vec3 v3MieSum = vec3(0.0);'#10#13 +
    ' float SampleU = HalfTexelOpticalDepthN;'#10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )'#10#13 +
    ' {'#10#13 +
    ' 	float PointPHeight = length( v3PointP );'#10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );'#10#13 +
    ' 	DensityRatio *= fScaledLength;'#10#13 +
    ' 	vec2 ViewerOpticalDepth = TEXTURE( OpticalDepth, vec2( Tex1x, SampleU ) ).rg;'#10#13  +
    ' 	float fAngle = dot(v3PointP, v3SunDir) / length(v3PointP);'#10#13 +
    ' 	float index = ((1.0 - fAngle) * 9.0/ 10.0) * 127.0/128.0;'#10#13 +
    ' 	vec2 SunOpticalDepth = TEXTURE( OpticalDepth, vec2( index, SampleU ) ).ba;'#10#13 +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;'#10#13 +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );'#10#13  +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;'#10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;'#10#13 +
    ' 	v3PointP += v3SampleRay;'#10#13 +
    ' 	SampleU += 1.0 / float(iNumSamples);'#10#13 +
    ' }'#10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );'#10#13  +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );'#10#13 +
    '}';

  CreateOpticalDepth_fp =
    '#if (SM4 == 1)'#10#13 +
    'out vec4 FragColor;'#10#13 +
    '#else'#10#13+
    '#define FragColor gl_FragColor'#10#13+
    '#endif'#10#13+
    'void main()'#10#13 +
    '{'#10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * vec2( InvOpticalDepthNLessOne, InvOpticalDepthN );'#10#13 +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );'#10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;'#10#13 +
    ' vec3 v3Ray;'#10#13 +
    ' v3Ray.x = sin( AngleY );'#10#13 +
    ' v3Ray.y = cos( AngleY );'#10#13 +
    ' v3Ray.z = 0.0;'#10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Ray );'#10#13 +
    ' vec3 v3PointP = v3PointPv;'#10#13 +
    ' float fSampleLength = fFarPvPa / float(iNumSamples);'#10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;'#10#13 +
    ' v3PointP += v3SampleRay * 0.5;'#10#13 +
    ' v3PointP += v3SampleRay * Tex0.y * iNumSamples;'#10#13 +
    ' vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );'#10#13 +
    ' vec2 SunOpticalDepth = t( v3PointP, v3Ray * fFarPvPa + v3PointPv );'#10#13 +
    ' FragColor = vec4( ViewerOpticalDepth, SunOpticalDepth.xy );'#10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

const
  cTexPrec: array[TGLNishitaSkyTexPrec] of TGLInternalFormat =
    (tfR11F_G11F_B10F, tfRGBA_FLOAT16, tfRGBA_FLOAT32);

var
  cIdentityTransformationRec: TTransformationRec;
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  SaveOnce: Boolean = true;
{$ENDIF}

constructor TGLCustomNishitaSky.Create(AOwner: TComponent);
const
  cRenderMaterialName = 'GLScene_NishitaSky_Material';
  cUpdateMaterialName = 'GLScene_NishitaSky_Update';
  cUpdateFastMaterialName = 'GLScene_NishitaSky_UpdateFast';
  cCreateOpticalDepthMaterialName = 'GLScene_NishitaSky_CreateOpticalDepth';
  cSamplerName = 'GLScene_NishitaSky_Sampler';
const
  ESun: Extended = 20.0;
  Kr: Extended = 0.0025;
  Km: Extended = 0.0010;
  g: Extended = -0.995;
  g2: Extended = 0.995 * 0.995;

var
  LShader: TGLShaderEx;
  LUpdateVertexShader120, LUpdateVertexShader330: TGLShaderEx;
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw, osDeferredDraw];
  FChanges := [nscConstants, nscRayleighMie,  nscOpticalDepth, nscTime];

  ConstantBlock.PI := PI;
  ConstantBlock.KrESun := Kr * ESun;
  ConstantBlock.KmESun := Km * ESun;
  ConstantBlock.Kr4PI := Kr * 4.0 * PI;
  ConstantBlock.Km4PI := Km * 4.0 * PI;
  ConstantBlock.InnerRadius := 6356.7523142;
  ConstantBlock.OuterRadius := ConstantBlock.InnerRadius * 1.0157313;
  // karman line
  ConstantBlock.fScale := 1.0 / (ConstantBlock.OuterRadius -
    ConstantBlock.InnerRadius);
  ConstantBlock.v2dRayleighMieScaleHeight := Vector2fMake(0.5, 0.1);
  ConstantBlock.InvWavelength4 := VectorMake(
    1.0 / power(0.650, 4),
    1.0 / power(0.570, 4),
    1.0 / power(0.475, 4),
    0);
  ConstantBlock.WavelengthMie := VectorMake(
    power(0.650, -0.84),
    power(0.570, -0.84),
    power(0.475, -0.84),
    0);
  ConstantBlock.v3HG := VectorMake(
    1.5 * (1.0 - g2) / (2.0 + g2),
    1.0 + g2,
    2.0 * g,
    0);

  ConstantBlock.tNumSamples := 50;
  ConstantBlock.iNumSamples := 20;
  FDomeDiv := 128;
  FOpticalDepthN := 256;
  FRayleighMieN := 256;

  ConstantBlock.InvOpticalDepthN := 1.0 / FOpticalDepthN;
  ConstantBlock.InvOpticalDepthNLessOne := 1.0 / (FOpticalDepthN - 1);
  ConstantBlock.HalfTexelOpticalDepthN := 0.5 / FOpticalDepthN;
  ConstantBlock.InvRayleighMieN := Vector2fMake(
    1.0 / FRayleighMieN,
    1.0 / (FRayleighMieN / 2));
  ConstantBlock.InvRayleighMieNLessOne := Vector2fMake(
    1.0 / (FRayleighMieN - 1),
    1.0 / (FRayleighMieN / 2 - 1));

  SetOclock(12.0);

  // Create uniform buffer block
  FUBO := TGLUniformBufferHandle.Create;
  // Create framebuffer
  FOpticalDepthFBO := TGLFramebufferHandle.Create;
  FOpticalDepthFBO.OnPrapare := DoOnPrepareOpticalDepthFBO;
  FRayleighMieFBO := TGLFramebufferHandle.Create;
  FRayleighMieFBO.OnPrapare := DoOnPrepareRayleighMieFBO;
  // Create attachments
  FMieTexture := GetInternalMaterialLibrary.AddAttachment('NishitaSkyMie');
  FRayleighTexture := GetInternalMaterialLibrary.AddAttachment('NishitaSkyRayleigh');
  FOpticalDepthTexture := GetInternalMaterialLibrary.AddAttachment('NishitaSkyOpticalDepth');
  // Create sampler
  FCommonSampler := GetInternalMaterialLibrary.Components.GetSamplerByName(cSamplerName);
  if not Assigned(FCommonSampler) then
  begin
    FCommonSampler := GetInternalMaterialLibrary.AddSampler(cSamplerName);
    FCommonSampler.MinFilter := miLinear;
    FCommonSampler.FilteringQuality := tfIsotropic;
    FCommonSampler.WrapX := twClampToEdge;
    FCommonSampler.WrapY := twClampToEdge;
  end;

  FBatch.Mesh := TMeshAtom.Create;
  FBatch.Transformation := @FTransformation;
  FBatch.Changed := True;
  FBatch.Mesh.TagName := ClassName;

  FBatch.Material :=
    GetInternalMaterialLibrary.Materials.GetLibMaterialByName(cRenderMaterialName);
  if FBatch.Material = nil then
  begin
    FBatch.Material := GetInternalMaterialLibrary.Materials.Add;
    with TGLLibMaterialEx(FBatch.Material) do
    begin
      Name := cRenderMaterialName;
      FixedFunction.MaterialOptions := [moNoLighting];
      FixedFunction.DepthProperties.DepthTest := False;
      FixedFunction.DepthProperties.DepthWrite := False;
      FixedFunction.DepthProperties.DepthClamp := True;
      // GLSL 120
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtVertex;
      LShader.Source.Add(Header120+Uniforms+Render_vp);
      ShaderModel3.LibVertexShaderName := LShader.Name;
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtFragment;
      LShader.Source.Add(Header120+Uniforms+Render_fp);
      ShaderModel3.LibFragmentShaderName := LShader.Name;
      ShaderModel3.Enabled := True;
      OnSM3UniformInitialize := InitializeRenderShader;
      // GLSL 330
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtVertex;
      LShader.Source.Add(Header330+Uniforms+Render_vp);
      ShaderModel4.LibVertexShaderName := LShader.Name;
      LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
      LShader.ShaderType := shtFragment;
      LShader.Source.Add(Header330+Uniforms+Render_fp);
      ShaderModel4.LibFragmentShaderName := LShader.Name;
      ShaderModel4.Enabled := True;
      OnSM4UniformInitialize := InitializeRenderShader;
      OnSM4UniformSetting := SM4_Uniform_Settig;
    end;
  end;

  FUpdateMaterial := GetInternalMaterialLibrary.Materials.Add;
  with FUpdateMaterial do
  begin
    Name := cUpdateMaterialName;
    FixedFunction.Enabled := False;
    // GLSL 120
    LUpdateVertexShader120 := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LUpdateVertexShader120.ShaderType := shtVertex;
    LUpdateVertexShader120.Source.Add(Header120+Uniforms+Update_vp);
    ShaderModel3.LibVertexShaderName := LUpdateVertexShader120.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header120+Uniforms+Share_fp+Update_fp);
    ShaderModel3.LibFragmentShaderName := LShader.Name;
    ShaderModel3.Enabled := True;
    OnSM3UniformInitialize := InitializeRenderShader;
    OnSM3UniformSetting := SM3_Uniform_Settig;
    // GLSL 330
    LUpdateVertexShader330 := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LUpdateVertexShader330.ShaderType := shtVertex;
    LUpdateVertexShader330.Source.Add(Header330+Uniforms+Update_vp);
    ShaderModel4.LibVertexShaderName := LUpdateVertexShader330.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header330+Uniforms+Share_fp+Update_fp);
    ShaderModel4.LibFragmentShaderName := LShader.Name;
    ShaderModel4.Enabled := True;
    OnSM4UniformInitialize := InitializeRenderShader;
    OnSM4UniformSetting := SM4_Uniform_Settig;
  end;

  FUpdateFastMaterial := GetInternalMaterialLibrary.Materials.Add;
  with FUpdateFastMaterial do
  begin
    Name := cUpdateFastMaterialName;
    FixedFunction.Enabled := False;
    // GLSL 120
    ShaderModel3.LibVertexShaderName := LUpdateVertexShader120.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header120+Uniforms+Share_fp+UpdateFast_fp);
    ShaderModel3.LibFragmentShaderName := LShader.Name;
    ShaderModel3.Enabled := True;
    OnSM3UniformInitialize := InitializeRenderShader;
    OnSM3UniformSetting := SM3_Uniform_Settig;
    // GLSL 330
    ShaderModel4.LibVertexShaderName := LUpdateVertexShader330.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header330+Uniforms+Share_fp+UpdateFast_fp);
    ShaderModel4.LibFragmentShaderName := LShader.Name;
    ShaderModel4.Enabled := True;
    OnSM4UniformInitialize := InitializeRenderShader;
    OnSM4UniformSetting := SM4_Uniform_Settig;
  end;

  FCreateOpticalDepthMaterial := GetInternalMaterialLibrary.Materials.Add;
  with FCreateOpticalDepthMaterial do
  begin
    Name := cCreateOpticalDepthMaterialName;
    FixedFunction.Enabled := False;
    // GLSL 120
    ShaderModel3.LibVertexShaderName := LUpdateVertexShader120.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header120+Uniforms+Share_fp+CreateOpticalDepth_fp);
    ShaderModel3.LibFragmentShaderName := LShader.Name;
    ShaderModel3.Enabled := True;
    OnSM3UniformInitialize := InitializeRenderShader;
    OnSM3UniformSetting := SM3_Uniform_Settig;
    // GLSL 330
    ShaderModel4.LibVertexShaderName := LUpdateVertexShader330.Name;
    LShader := GetInternalMaterialLibrary.AddShader(cInternalShader);
    LShader.ShaderType := shtFragment;
    LShader.Source.Add(Header330+Uniforms+Share_fp+CreateOpticalDepth_fp);
    ShaderModel4.LibFragmentShaderName := LShader.Name;
    ShaderModel4.Enabled := True;
    OnSM4UniformInitialize := InitializeRenderShader;
    OnSM4UniformSetting := SM4_Uniform_Settig;
  end;

  FScreenQuadBatch.Mesh := TMeshAtom.Create;
  with FScreenQuadBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType2f);

      BeginAssembly(mpTRIANGLE_STRIP);
      Attribute2f(attrPosition, -1, -1);
      EmitVertex;
      Attribute2f(attrPosition, -1, 1);
      EmitVertex;
      Attribute2f(attrPosition, 1, -1);
      EmitVertex;
      Attribute2f(attrPosition, 1, 1);
      EmitVertex;
      EndAssembly;
    finally
      UnLock;
    end;
  end;
  FScreenQuadBatch.Transformation := @cIdentityTransformationRec;

  FSoftwareMode := False;
  FColorPrecision := nsp11bit;
  FFastUpdate := False;

  if not (csLoading in ComponentState) then
    Loaded;
end;

destructor TGLCustomNishitaSky.Destroy;
begin
  FBatch.Mesh.Free;
  FScreenQuadBatch.Mesh.Free;
  FUBO.Destroy;
  FMieTexture.Destroy;
  FRayleighTexture.Destroy;
  FOpticalDepthTexture.Destroy;
  FOpticalDepthFBO.Destroy;
  FRayleighMieFBO.Destroy;
  inherited;
end;

procedure TGLCustomNishitaSky.Update(var ARci: TRenderContextInfo);
begin

  if nscConstants in FChanges then
  begin
    ConstantBlock.InvOpticalDepthN := 1.0 / FOpticalDepthN;
    ConstantBlock.InvOpticalDepthNLessOne := 1.0 / (FOpticalDepthN - 1);
    ConstantBlock.HalfTexelOpticalDepthN := 0.5 / FOpticalDepthN;
    ConstantBlock.InvRayleighMieN := Vector2fMake(
      1.0 / FRayleighMieN,
      1.0 / (FRayleighMieN / 2));
    ConstantBlock.InvRayleighMieNLessOne := Vector2fMake(
      1.0 / (FRayleighMieN - 1),
      1.0 / (FRayleighMieN / 2 - 1));
    FUBO.NotifyChangesOfData;
    Exclude(FChanges, nscConstants);
    Include(FChanges, nscTime);
  end;

  if FSoftwareMode then
  begin

  end
  else
  begin
    // Initialize constant buffer
    with FUBO do
    if IsSupported then
    begin
      AllocateHandle;
      if IsDataNeedUpdate then
      begin
        BindBufferData(@ConstantBlock, SizeOf(TNSConstantBlock), GL_STATIC_READ);
        Unbind;
        NotifyDataUpdated;
      end;
    end;

    // Updates
    if nscRayleighMie in FChanges then
    begin
      with FMieTexture do
      begin
        InternalWidth := FRayleighMieN;
        InternalHeight := FRayleighMieN div 2;
        InternalFormat := cTexPrec[FColorPrecision];
        MaxLOD := 0;
      end;

      with FRayleighTexture do
      begin
        InternalWidth := FRayleighMieN;
        InternalHeight := FRayleighMieN div 2;
        InternalFormat := cTexPrec[FColorPrecision];
        MaxLOD := 0;
      end;

      Exclude(FChanges, nscRayleighMie);
      Include(FChanges, nscTime);
    end;

    if (nscOpticalDepth in FChanges) and FFastUpdate then
    begin
      with FOpticalDepthTexture do
      begin
        InternalWidth := FOpticalDepthN;
        InternalHeight := FOpticalDepthN;
        InternalFormat := cTexPrec[FColorPrecision];
        MaxLOD := 0;
      end;
      FRayleighMieFBO.NotifyChangesOfData;
      DoOnPrepareRayleighMieFBO(CurrentGLContext);
      MakeGPUOpticalDepth(ARci);
      Exclude(FChanges, nscOpticalDepth);
      Include(FChanges, nscTime);
    end;

    if nscTime in FChanges then
    begin
      MakeGPUMieRayleighBuffer(ARci);
      if Assigned(FSun) then
        FSun.Direction.SetVector(VectorNegate(v3SunDir));
    end;
  end;
end;

procedure TGLCustomNishitaSky.InitializeRenderShader(Sender: TGLBaseShaderModel);
begin
  with Sender do
  begin
    Uniforms['ViewProjectionMatrix'].AutoSetMethod := cafViewProjectionMatrix;
    Uniforms['OpticalDepth'].TextureName := FOpticalDepthTexture.Name;
    Uniforms['OpticalDepth'].SamplerName := FCommonSampler.Name;
    Uniforms['Mie'].TextureName := FMieTexture.Name;
    Uniforms['Mie'].SamplerName := FCommonSampler.Name;
    Uniforms['Rayleigh'].TextureName := FRayleighTexture.Name;
    Uniforms['Rayleigh'].SamplerName := FCommonSampler.Name;
  end;
end;

procedure TGLCustomNishitaSky.Loaded;
begin
  inherited;

  with FOpticalDepthTexture do
  begin
    InternalWidth := FOpticalDepthN;
    InternalHeight := FOpticalDepthN;
    InternalFormat := cTexPrec[FColorPrecision];
    MaxLOD := 0;
  end;

  with FMieTexture do
  begin
    InternalWidth := FRayleighMieN;
    InternalHeight := FRayleighMieN div 2;
    InternalFormat := cTexPrec[FColorPrecision];
    MaxLOD := 0;
  end;

  with FRayleighTexture do
  begin
    InternalWidth := FRayleighMieN;
    InternalHeight := FRayleighMieN div 2;
    InternalFormat := cTexPrec[FColorPrecision];
    MaxLOD := 0;
  end;
end;

procedure TGLCustomNishitaSky.MakeGPUOpticalDepth(var ARci: TRenderContextInfo);
{$IFDEF NISHITA_SKY_DEBUG_MODE}
var
  debugImg: TGLDDSImage;
  uniformBlockSize: Integer;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
  FOpticalDepthFBO.BindForDrawing;

  with ARci.GLStates do
  begin
    ViewPort := Vector4iMake(0, 0, FOpticalDepthN, FOpticalDepthN);
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
    SetDepthRange(0, 1);
  end;

  FScreenQuadBatch.Material := FCreateOpticalDepthMaterial;

  Scene.RenderManager.DrawTechnique.DrawBatch(ARci, FScreenQuadBatch);

{$IFDEF NISHITA_SKY_DEBUG_MODE}
  debugImg := TGLDDSImage.Create;
  if FColorPrecision = nsp11bit then
  begin
    useCurrent := false;
    castFormat := tfRGBA_FLOAT16;
  end
  else
  begin
    useCurrent := true;
    castFormat := tfRGBA8;
  end;
  debugImg.AssignFromTexture(CurrentGLContext,
    FOpticalDepthTexture.Handle.Handle,
    ttTexture2D, useCurrent, castFormat);
  debugImg.SaveToFile('OpticalDepth.dds');
  debugImg.Free;
//  GLSLogger.Log(Format('GPU Uniform block size = %d', [ublockConstants.DataSize]));
//  uniformBlockSize := SizeOf(TNSConstantBlock);
//  GLSLogger.Log(Format('CPU Uniform block size = %d', [uniformBlockSize]));
{$ENDIF}
end;

procedure TGLCustomNishitaSky.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSun) then
    FSun := nil;
  inherited;
end;

procedure TGLCustomNishitaSky.MakeGPUMieRayleighBuffer(
  var ARci: TRenderContextInfo);

{$IFDEF NISHITA_SKY_DEBUG_MODE}
var
  debugImg: TGLDDSImage;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    Gl.StringMarkerGREMEDY(24, 'MakeGPUMieRayleighBuffer');
{$ENDIF}
  FRayleighMieFBO.BindForDrawing;
  GL.DrawBuffers(2, @cDrawBuffers);

  with ARci.GLStates do
  begin
    ViewPort := Vector4iMake(0, 0, FRayleighMieN, FRayleighMieN div 2);
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
  end;

  if FFastUpdate then
    FScreenQuadBatch.Material := FUpdateFastMaterial
  else
    FScreenQuadBatch.Material := FUpdateMaterial;

  Scene.RenderManager.DrawTechnique.DrawBatch(ARci, FScreenQuadBatch);

  Exclude(FChanges, nscTime);

{$IFDEF NISHITA_SKY_DEBUG_MODE}
  if SaveOnce then
  begin
    debugImg := TGLDDSImage.Create;
    if FColorPrecision = nsp11bit then
    begin
      useCurrent := false;
      castFormat := tfRGBA_FLOAT16;
    end
    else
    begin
      useCurrent := true;
      castFormat := tfRGBA8;
    end;
    debugImg.AssignFromTexture(CurrentGLContext, FMieTexture.Handle.Handle,
      ttTexture2D, useCurrent, castFormat);
    debugImg.SaveToFile('Mie.dds');
    debugImg.AssignFromTexture(CurrentGLContext,
      FRayleighTexture.Handle.Handle,
      ttTexture2D, useCurrent, castFormat);
    debugImg.SaveToFile('Rayleigh.dds');
    debugImg.Free;
    SaveOnce := false;
  end;
{$ENDIF}
end;

procedure TGLCustomNishitaSky.DoOnPrepareOpticalDepthFBO(Sender: TGLContext);
var
  Status: string;
begin
  if FOpticalDepthFBO.IsSupported then
  begin
    FOpticalDepthFBO.AllocateHandle;
    if FOpticalDepthFBO.IsDataNeedUpdate then
    begin
      FOpticalDepthTexture.DoOnPrepare(Sender);
      if FOpticalDepthTexture.IsValid then
      begin
        FOpticalDepthFBO.BindForDrawing;
        FOpticalDepthFBO.Attach2DTexture(
          GL_FRAMEBUFFER,
          GL_COLOR_ATTACHMENT0,
          GL_TEXTURE_2D,
          FOpticalDepthTexture.Handle.Handle,
          0);
        FOpticalDepthFBO.UnBindForDrawing;

        if FOpticalDepthFBO.GetStringStatus(Status) <> fsComplete then
        begin
          GLSLogger.LogErrorFmt(
            '%s framebuffer error for optical-depth texture - %s', [ClassName, Status]);
          exit;
        end;
        FOpticalDepthFBO.NotifyDataUpdated;
        Include(FChanges, nscTime);
        Include(FChanges, nscOpticalDepth);
      end;
    end;
  end;

  if FOpticalDepthFBO.IsDataNeedUpdate then
    FSoftwareMode := True;
end;

procedure TGLCustomNishitaSky.DoOnPrepareRayleighMieFBO(Sender: TGLContext);
var
  Status: string;
begin
  if FRayleighMieFBO.IsSupported then
  begin
    FRayleighMieFBO.AllocateHandle;
    if FRayleighMieFBO.IsDataNeedUpdate then
    begin
      FMieTexture.DoOnPrepare(Sender);
      FRayleighTexture.DoOnPrepare(Sender);
      if FMieTexture.IsValid and FRayleighTexture.IsValid then
      begin
        FRayleighMieFBO.BindForDrawing;
        FRayleighMieFBO.Attach2DTexture(
          GL_FRAMEBUFFER,
          GL_COLOR_ATTACHMENT0,
          GL_TEXTURE_2D,
          FMieTexture.Handle.Handle,
          0);
        FRayleighMieFBO.Attach2DTexture(
          GL_FRAMEBUFFER,
          GL_COLOR_ATTACHMENT1,
          GL_TEXTURE_2D,
          FRayleighTexture.Handle.Handle,
          0);
        FRayleighMieFBO.UnBindForDrawing;

        if FRayleighMieFBO.GetStringStatus(Status) <> fsComplete then
        begin
          GLSLogger.LogErrorFmt(
            '%s framebuffer error for rayleigh-mie texture - %s', [ClassName, Status]);
          exit;
        end;
        FRayleighMieFBO.NotifyDataUpdated;
        Include(FChanges, nscTime);
      end;
    end;
  end;

  if FRayleighMieFBO.IsDataNeedUpdate then
    FSoftwareMode := True;
end;

procedure TGLCustomNishitaSky.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  vp: TVector4i;
  storeFrameBuffer: TGLuint;
begin
  // store states
  vp := ARci.GLStates.ViewPort;
  storeFrameBuffer := ARci.GLStates.DrawFrameBuffer;
  Update(ARci);
  // restore states
  ARci.GLStates.ViewPort := vp;
  ARci.GLStates.DrawFrameBuffer := storeFrameBuffer;

  if ocStructure in Changes then
  begin
    BuildMesh;
  end;
  FTransformation := ARci.PipelineTransformation.StackTop;
  FTransformation.FViewMatrix[3, 0] := 0;
  FTransformation.FViewMatrix[3, 1] := 0;
  FTransformation.FViewMatrix[3, 2] := 0;
  FTransformation.FStates := cAllStatesChanged;
  FBatch.Order := ARci.orderCounter;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGLCustomNishitaSky.BuildMesh;
var
  V1, V2: TAffineVector;
  i, j: Integer;
  StepH, StepV: Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  vTexCoord, TexFactor, uTexCoord0, uTexCoord1: Single;
begin
  // common settings
  StepV := Pi / FDomeDiv;
  StepH := 2 * StepV;
  Phi := Pi / 2;
  Phi2 := Phi - StepH;
  TexFactor := 2 / (FDomeDiv + 2);

  with FBatch.Mesh do
  begin
    Lock;
    try
      Clear;
      DeclareAttribute(attrPosition, GLSLType3f);
      DeclareAttribute(attrTexCoord0, GLSLType2f);

      BeginAssembly(mpTRIANGLE_STRIP);
      for j := 0 to FDomeDiv div 2 - 1 do
      begin
        Theta := 0;
        SinCos(Phi, SinP, CosP);
        SinCos(Phi2, SinP2, CosP2);
        V1[1] := SinP;
        V2[1] := SinP2;
        uTexCoord0 := (0.5 + j) * TexFactor;
        uTexCoord1 := (j + 1.5) * TexFactor;

        for i := 0 to FDomeDiv div 2 do
        begin
          SinCos(Theta, SinT, CosT);
          V1[0] := CosP * CosT;
          V2[0] := CosP2 * CosT;
          V1[2] := CosP * SinT;
          V2[2] := CosP2 * SinT;
          vTexCoord := (0.5 + i) * TexFactor;
          Attribute2f(attrTexCoord0, uTexCoord1, vTexCoord);
          Attribute3f(attrPosition, V2[0], V2[1], V2[2]);
          EmitVertex;
          Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
          Attribute3f(attrPosition, V1[0], V1[1], V1[2]);
          EmitVertex;
          Theta := Theta - StepH;
        end;

        for i := 0 to FDomeDiv div 2 do
        begin
          SinCos(Theta, SinT, CosT);
          V1[0] := CosP * CosT;
          V2[0] := CosP2 * CosT;
          V1[2] := CosP * SinT;
          V2[2] := CosP2 * SinT;
          vTexCoord := 1.0 - (0.5 + i) * TexFactor;
          Attribute2f(attrTexCoord0, uTexCoord1, vTexCoord);
          Attribute3f(attrPosition, V2[0], V2[1], V2[2]);
          EmitVertex;
          Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
          Attribute3f(attrPosition, V1[0], V1[1], V1[2]);
          EmitVertex;
          Theta := Theta - StepH;
        end;

        RestartStrip;
        Phi := Phi2;
        Phi2 := Phi2 - StepV;
      end;

      EndAssembly;
      WeldVertices;
    finally
      UnLock;
    end;
  end;

  FBatch.Changed := True;
  ClearStructureChanged;
end;

procedure TGLCustomNishitaSky.Assign(Source: TPersistent);
var
  sky: TGLCustomNishitaSky;
begin
  inherited;
  if Source is TGLCustomNishitaSky then
  begin
    sky := TGLCustomNishitaSky(Source);
    ConstantBlock := sky.ConstantBlock;
    DomeDivision := sky.FDomeDiv;
    OpticalDepthSize := sky.FOpticalDepthN;
    RayleighMieSize := sky.FRayleighMieN;
    ColorPrecision := sky.FColorPrecision;
    MieScaleHeight := sky.GetMieScaleHeight;
    RayleighScaleHeight := sky.GetRayleighScaleHeight;
    Oclock := sky.FOclock;
    FFastUpdate := sky.FFastUpdate;
  end;
end;

procedure TGLCustomNishitaSky.SetScene(const value: TGLScene);
begin
  if value <> Scene then
  begin
    if Assigned(Scene) then
    begin
      Scene.RenderManager.UnRegisterBatch(FBatch);
    end;
    if Assigned(value) then
    begin
      value.RenderManager.RegisterBatch(FBatch);
    end;
  end;
  inherited;
end;

procedure TGLCustomNishitaSky.SetSun(const Value: TGLLightSource);
begin
  if FSun <> Value then
  begin
    if Assigned(FSun) then
      FSun.RemoveFreeNotification(Self);
    FSun := Value;
    if Assigned(FSun) then
      FSun.FreeNotification(Self);
    Include(FChanges, nscTime);
    NotifyChange(Self);
  end;
end;

procedure TGLCustomNishitaSky.SM3_Uniform_Settig(Sender: TGLBaseShaderModel; var ARci: TRenderContextInfo);
begin
  with Sender do
  begin
    Uniforms['WavelengthMie'].vec3 := AffineVectorMake(ConstantBlock.WavelengthMie);
    Uniforms['v3HG'].vec3 := AffineVectorMake(ConstantBlock.v3HG);
    Uniforms['InvWavelength4'].vec3 := AffineVectorMake(ConstantBlock.InvWavelength4);
    Uniforms['v2dRayleighMieScaleHeight'].vec2 := ConstantBlock.v2dRayleighMieScaleHeight;
    Uniforms['InvRayleighMieN'].vec2 := ConstantBlock.InvRayleighMieN;
    Uniforms['InvRayleighMieNLessOne'].vec2 := ConstantBlock.InvRayleighMieNLessOne;
    Uniforms['PI'].float := ConstantBlock.PI;
    Uniforms['InnerRadius'].float := ConstantBlock.InnerRadius;
    Uniforms['OuterRadius'].float := ConstantBlock.OuterRadius;
    Uniforms['fScale'].float := ConstantBlock.fScale;
    Uniforms['KrESun'].float := ConstantBlock.KrESun;
    Uniforms['KmESun'].float := ConstantBlock.KmESun;
    Uniforms['Kr4PI'].float := ConstantBlock.Kr4PI;
    Uniforms['Km4PI'].float := ConstantBlock.Km4PI;
    Uniforms['InvOpticalDepthN'].float := ConstantBlock.InvOpticalDepthN;
    Uniforms['InvOpticalDepthNLessOne'].float := ConstantBlock.InvOpticalDepthNLessOne;
    Uniforms['HalfTexelOpticalDepthN'].float := ConstantBlock.HalfTexelOpticalDepthN;
    Uniforms['tNumSamples'].int := ConstantBlock.tNumSamples;
    Uniforms['iNumSamples'].int := ConstantBlock.iNumSamples;
    Uniforms['v3SunDir'].vec3 := v3SunDir;
  end;
end;

procedure TGLCustomNishitaSky.SM4_Uniform_Settig(Sender: TGLBaseShaderModel; var ARci: TRenderContextInfo);
begin
  FUBO.BindBase(1);
  GL.UniformBlockBinding(ARci.GLStates.CurrentProgram, 0, 1);
  Sender.Uniforms['v3SunDir'].vec3 := v3SunDir;
end;

procedure TGLCustomNishitaSky.SetOclock(Value: Double);
begin
  FOclock := Value;
  Value := frac((Value - 12) / 24);
  v3SunDir := AffineVectorMake(sin(2 * Pi * Value), cos(2 * Pi * Value), 0);
  Include(FChanges, nscTime);
  NotifyChange(Self);
end;

function TGLCustomNishitaSky.StoreOclock: Boolean;
begin
  Result := FOclock <> 12.0;
end;

procedure TGLCustomNishitaSky.SetDomeDivision(Value: Integer);
begin
  if Value < 16 then
    Value := 16;
  if Value <> FDomeDiv then
  begin
    fDomeDiv := Value;
    StructureChanged;
  end;
end;

procedure TGLCustomNishitaSky.SetOpticalDepth(Value: Integer);
begin
  if Value <> FOpticalDepthN then
  begin
    FOpticalDepthN := Value;
    Include(FChanges, nscConstants);
    Include(FChanges, nscOpticalDepth);
    NotifyChange(Self);
  end;
end;

procedure TGLCustomNishitaSky.SetRayleighMie(Value: Integer);
begin
  if Value < 32 then
    Value := 32
  else if Value > 2048 then
    Value := 2048;

  if Value <> FRayleighMieN then
  begin
    FRayleighMieN := Value;
    Include(FChanges, nscConstants);
    Include(FChanges, nscRayleighMie);
    NotifyChange(Self);
  end;
end;

procedure TGLCustomNishitaSky.SetColorPrecision(const Value:
  TGLNishitaSkyTexPrec);
begin
  if Value <> FColorPrecision then
  begin
    FColorPrecision := Value;
    Include(FChanges, nscRayleighMie);
    Include(FChanges, nscOpticalDepth);
    NotifyChange(Self);
  end;
end;

function TGLCustomNishitaSky.GetMieScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[1];
end;

function TGLCustomNishitaSky.GetRayleighScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[0];
end;

procedure TGLCustomNishitaSky.SetMieScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight[1] then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight[1] := Value;
    Include(FChanges, nscConstants);
    NotifyChange(Self);
  end;
end;

procedure TGLCustomNishitaSky.SetRayleighScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight[0] then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight[0] := Value;
    Include(FChanges, nscConstants);
    NotifyChange(Self);
  end;
end;

function TGLCustomNishitaSky.StoreMieScaleHeight: Boolean;
const
  MSH: Single = 0.1;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[1] <> MSH;
end;

function TGLCustomNishitaSky.StoreRayleighScaleHeight: Boolean;
const
  RSH: Single = 0.5;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[0] <> RSH;
end;

initialization

  RegisterClasses([TGLCustomNishitaSky, TGLNishitaSky]);

  cIdentityTransformationRec.FStates := cAllStatesChanged;
  cIdentityTransformationRec.FModelMatrix := IdentityHmgMatrix;
  cIdentityTransformationRec.FViewMatrix := IdentityHmgMatrix;
  cIdentityTransformationRec.FProjectionMatrix := IdentityHmgMatrix;

end.

