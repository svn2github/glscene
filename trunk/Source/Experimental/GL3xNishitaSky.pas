//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GL3xNishitaSky <p>

   GPU Crysis Atmospheric scattering.<p>
   There is two methods for update: <p>
    the first one just compute all nasty integral and <p>
    the other (FastUpdate) one use a precomputed texture to speed up thing <p>
    at the cost of loosing some quality when sun is near horizon.<p>

   <b>History : </b><font size=-1><ul>
      <li>08/03/10 - Yar - Creation
   </ul></font><p>
}

unit GL3xNishitaSky;

interface

{$I GLScene.inc}
{.$DEFINE NISHITA_SKY_DEBUG_MODE}

uses
  // VCL
  SysUtils, Classes,

  // GLScene
  GLScene, GLCadencer, OpenGL1x, VectorGeometry, VectorTypes, VectorGeometryEXT,
  GLState, GLContext, GLTexture, GLGraphics,
  GLTextureFormat, GLFBO,
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  GLSLog,
{$ENDIF}
  GL3xObjects, GLVBOManagers, GLRenderContextInfo;

type

  TGLNishitaSkyChange = (nscConstants, nscRayleighMie, nscOpticalDepth,
    nscTime);
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

  TGL3xCustomNishitaSky = class(TGL3xBaseSceneObject)
  private
    ConstantBlock: TNSConstantBlock;
    FUBO: TGLUniformBufferHandle;
    FDomeDiv: Integer;
    FOpticalDepthN: Integer;
    FRayleighMieN: Integer;
    FMieTexture: TGLTexture;
    FRayleighTexture: TGLTexture;
    FOpticalDepthTexture: TGLTexture;
    OpticalDepthFBO: TGLFrameBuffer;
    RayleighMieFBO: TGLFrameBuffer;
    v3SunDir: TAffineVector;
    FOclock: Double;
    FFastUpdate: Boolean;
    FColorPrecision: TGLNishitaSkyTexPrec;
    FChanges: TGLNishitaSkyChanges;
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
  protected
    procedure Initialize(var rci: TRenderContextInfo);
    procedure MakeGPUOpticalDepth(var rci: TRenderContextInfo);
    procedure MakeGPUMieRayleighBuffer(var rci: TRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    property Oclock: Double read FOclock write SetOclock stored StoreOclock;
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

  TGL3xNishitaSky = class(TGL3xCustomNishitaSky)
  published
    property Oclock;
    property FastUpdate;
    property DomeDivision;
    property OpticalDepthSize;
    property RayleighMieSize;
    property ColorPrecision;
    property MieScaleHeight;
    property RayleighScaleHeight;

    property BuiltProperties;
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
{$IFDEF NISHITA_SKY_DEBUG_MODE}
uses
  GLFileDDS;
{$ENDIF}

{$IFDEF GLS_COMPILER_2005_UP}{$REGION 'Shaders'}{$ENDIF}
const

  Header_fp: AnsiString =
    '#version 150' + #10#13 +
    'precision highp float;' + #10#13 +
//    'layout(origin_upper_left, pixel_center_integer) in vec4 gl_FragCoord;' + #10#13 +
   'layout(std140) uniform ConstantBlock {' + #10#13 +
    ' vec3 WavelengthMie;' + #10#13 +
    ' vec3 v3HG;' + #10#13 +
    ' vec3 InvWavelength4;' + #10#13 +
    ' vec2 v2dRayleighMieScaleHeight;' + #10#13 +
    ' vec2 InvRayleighMieN;' + #10#13 +
    ' vec2 InvRayleighMieNLessOne;' + #10#13 +
    ' float PI;' + #10#13 +
    ' float InnerRadius;' + #10#13 +
    ' float OuterRadius;' + #10#13 +
    ' float fScale;' + #10#13 +
    ' float KrESun;' + #10#13 +
    ' float KmESun;' + #10#13 +
    ' float Kr4PI;' + #10#13 +
    ' float Km4PI;' + #10#13 +
    ' float InvOpticalDepthN;' + #10#13 +
    ' float InvOpticalDepthNLessOne;' + #10#13 +
    ' float HalfTexelOpticalDepthN;' + #10#13 +
    ' int tNumSamples;' + #10#13 +
    ' int iNumSamples;' + #10#13 +
    '};' + #10#13;

  Share_fp: AnsiString =
    'vec2 GetDensityRatio( float fHeight )' + #10#13 +
    '{' + #10#13 +
    '	float fAltitude = (fHeight - InnerRadius) * fScale;' + #10#13 +
    '	return exp( vec2(-fAltitude) / v2dRayleighMieScaleHeight.xy );' + #10#13 +
    '}' + #10#13 +
    'vec2 t( vec3 P, vec3 Px )' + #10#13 +
    '{' + #10#13 +
    '	vec2 OpticalDepth = vec2(0.0);' + #10#13 +
    '	vec3 v3Vector = Px - P;' + #10#13 +
    '	float fFar = length( v3Vector );' + #10#13 +
    '	vec3 v3Dir = v3Vector / fFar;' + #10#13 +
    '	float fSampleLength = fFar / float(tNumSamples);' + #10#13 +
    '	float fScaledLength = fSampleLength * fScale;' + #10#13 +
    '	vec3 v3SampleRay = v3Dir * fSampleLength;' + #10#13 +
    '	P += v3SampleRay * 0.5;' + #10#13 +
    '	for(int i = 0; i < tNumSamples; i++)' + #10#13 +
    '	{' + #10#13 +
    '		float fHeight = length( P );' + #10#13 +
    '		OpticalDepth += GetDensityRatio( fHeight );' + #10#13 +
    '		P += v3SampleRay;' + #10#13 +
    '	}' + #10#13 +
    '	OpticalDepth *= fScaledLength;' + #10#13 +
    '	return OpticalDepth;' + #10#13 +
    '}' + #10#13 +
    'float HitOuterSphere( vec3 O, vec3 Dir )' + #10#13 +
    '{' + #10#13 +
    '	vec3 L = -O;' + #10#13 +
    '	float B = dot( L, Dir );' + #10#13 +
    '	float C = dot( L, L );' + #10#13 +
    '	float D = C - B * B;' + #10#13 +
    '	float q = sqrt( OuterRadius * OuterRadius - D );' + #10#13 +
    '	float t = B;' + #10#13 +
    '	t += q;' + #10#13 +
    '	return t;' + #10#13 +
    '}' + #10#13;

  Render_vp: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'in vec2 TexCoord0;' + #10#13 +
    'out vec2 texcoord;' + #10#13 +
    'out vec3 vertex;' + #10#13 +
    'uniform mat4 ModelViewProjectionMatrix;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	vertex = -Position;' + #10#13 +
    '	texcoord = TexCoord0;' + #10#13 +
    '	gl_Position = ModelViewProjectionMatrix * vec4(Position, 1.0);' + #10#13 +
    '}';

  Update_vp: AnsiString =
    '#version 150' + #10#13 +
    'in vec3 Position;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    '	gl_Position = vec4(Position, 1.0);' + #10#13 +
    '}';

  Render_fp: AnsiString =
    'const float fExposure = -2.0;' + #10#13 +
    'vec3 HDR( vec3 LDR)' + #10#13 +
    '{' + #10#13 +
    '	return 1.0 - exp( fExposure * LDR );' + #10#13 +
    '}' + #10#13 +
    'float getMiePhase(float fCos, float fCos2)' + #10#13 +
    '{' + #10#13 +
    '	return v3HG.x * (1.0 + fCos2) / pow(v3HG.y - v3HG.z * fCos, 1.5);' + #10#13
    +
    '}' + #10#13 +
    'float getRayleighPhase(float fCos2)' + #10#13 +
    '{' + #10#13 +
    '	return 0.75 * (1.0 + fCos2);' + #10#13 +
    '}' + #10#13 +

  'in vec3 vertex;' + #10#13 +
    'in vec2 texcoord;' + #10#13 +
    'uniform sampler2D Mie;' + #10#13 +
    'uniform sampler2D Rayleigh;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' float fCos = dot( v3SunDir, vertex ) / length( vertex );' + #10#13 +
    ' float fCos2 = fCos * fCos;' + #10#13 +
    ' vec3 v3RayleighSamples = texture(Rayleigh, texcoord).rgb;' + #10#13 +
    ' vec3 v3MieSamples = texture(Mie, texcoord).rgb;' + #10#13 +
    ' FragColor.rgb = getRayleighPhase(fCos2) * v3RayleighSamples.rgb +' + #10#13
    +
    '  getMiePhase(fCos, fCos2) * v3MieSamples.rgb;' + #10#13 +
    ' FragColor = vec4(HDR( FragColor.rgb ), 1.0);' + #10#13 +
    '}';

  Update_fp: AnsiString =
    'out vec4 Mie;' + #10#13 +
    'out vec4 RayLeigh;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * InvRayleighMieNLessOne.xy;' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 1e-3, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +
    ' float AngleXZ = PI * Tex0.y;' + #10#13 +
    ' vec3 v3Dir;' + #10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );' + #10#13 +
    ' v3Dir.y = cos( AngleY );' + #10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );' + #10#13 +
    ' v3Dir = normalize( v3Dir );' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );' + #10#13 +
    ' vec3 v3Ray = v3Dir;' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;' + #10#13 +
    ' float fScaledLength = fSampleLength * fScale;' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +
    ' vec3 v3RayleighSum = vec3(0.0);' + #10#13 +
    ' vec3 v3MieSum = vec3(0.0);' + #10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )' + #10#13 +
    ' {' + #10#13 +
    ' 	float PointPHeight = length( v3PointP );' + #10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );' + #10#13 +
    ' 	DensityRatio *= fScaledLength;' + #10#13 +
    ' 	vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );' + #10#13 +
    ' 	float dFarPPc = HitOuterSphere( v3PointP, v3SunDir );' + #10#13 +
    ' 	vec2 SunOpticalDepth = t( v3PointP, v3PointP + v3SunDir * dFarPPc );' + #10#13
    +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;' + #10#13
    +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );' + #10#13
    +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;' + #10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;' + #10#13 +
    ' 	v3PointP += v3SampleRay;' + #10#13 +
    ' }' + #10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );' + #10#13
    +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );' + #10#13 +
    '}';

  UpdateFast_fp: AnsiString =
    'out vec4 Mie;' + #10#13 +
    'out vec4 RayLeigh;' + #10#13 +
    'uniform sampler2D OpticalDepth;' + #10#13 +
    'uniform vec3 v3SunDir;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 pos = gl_FragCoord.xy - vec2(0.5);' + #10#13 +
    ' vec2 Tex0 = pos * InvRayleighMieNLessOne.xy;' + #10#13 +
    ' float Tex1x = pos.x * InvRayleighMieN.x + HalfTexelOpticalDepthN;' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +
    ' float AngleXZ = PI * Tex0.y;' + #10#13 +

  ' vec3 v3Dir;' + #10#13 +
    ' v3Dir.x = sin( AngleY ) * cos( AngleXZ  );' + #10#13 +
    ' v3Dir.y = cos( AngleY );' + #10#13 +
    ' v3Dir.z = sin( AngleY ) * sin( AngleXZ  );' + #10#13 +
    ' v3Dir = normalize( v3Dir );' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Dir );' + #10#13 +
    ' vec3 v3Ray = v3Dir;' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / iNumSamples;' + #10#13 +
    ' float fScaledLength = fSampleLength * fScale;' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +

  ' vec3 v3RayleighSum = vec3(0.0);' + #10#13 +
    ' vec3 v3MieSum = vec3(0.0);' + #10#13 +
    ' float SampleU = HalfTexelOpticalDepthN;' + #10#13 +
    ' for( int k = 0; k < iNumSamples; k++ )' + #10#13 +
    ' {' + #10#13 +
    ' 	float PointPHeight = length( v3PointP );' + #10#13 +
    ' 	vec2 DensityRatio = GetDensityRatio( PointPHeight );' + #10#13 +
    ' 	DensityRatio *= fScaledLength;' + #10#13 +
    ' 	vec2 ViewerOpticalDepth = texture( OpticalDepth, vec2( Tex1x, SampleU ) ).rg;' + #10#13
    +
    ' 	float fAngle = dot(v3PointP, v3SunDir) / length(v3PointP);' + #10#13 +
    ' 	float index = ((1.0 - fAngle) * 9.0/ 10.0) * 127.0/128.0;' + #10#13 +
    ' 	vec2 SunOpticalDepth = texture( OpticalDepth, vec2( index, SampleU ) ).ba;' + #10#13
    +
    ' 	vec2 OpticalDepthP = SunOpticalDepth.xy + ViewerOpticalDepth.xy;' + #10#13
    +
    ' 	vec3 v3Attenuation = exp( - Kr4PI * InvWavelength4 * OpticalDepthP.x - Km4PI * OpticalDepthP.y );' + #10#13
    +
    ' 	v3RayleighSum += DensityRatio.x * v3Attenuation;' + #10#13 +
    ' 	v3MieSum += DensityRatio.y * v3Attenuation;' + #10#13 +
    ' 	v3PointP += v3SampleRay;' + #10#13 +
    ' 	SampleU += 1.0 / float(iNumSamples);' + #10#13 +
    ' }' + #10#13 +
    ' RayLeigh = vec4( v3RayleighSum * KrESun * InvWavelength4, 1.0 );' + #10#13
    +
    ' Mie = vec4( v3MieSum * KmESun * WavelengthMie, 1.0 );' + #10#13 +
    '}';

  CreateOpticalDepth_fp: AnsiString =
    'out vec4 FragColor;' + #10#13 +
    'void main(void)' + #10#13 +
    '{' + #10#13 +
    ' vec2 Tex0 = (gl_FragCoord.xy - vec2(0.5)) * vec2( InvOpticalDepthNLessOne, InvOpticalDepthN );' + #10#13
    +
    ' vec3 v3PointPv = vec3( 0.0, InnerRadius + 0.001, 0.0 );' + #10#13 +
    ' float AngleY = 100.0 * Tex0.x * PI / 180.0;' + #10#13 +

  ' vec3 v3Ray;' + #10#13 +
    ' v3Ray.x = sin( AngleY );' + #10#13 +
    ' v3Ray.y = cos( AngleY );' + #10#13 +
    ' v3Ray.z = 0.0;' + #10#13 +
    ' float fFarPvPa = HitOuterSphere( v3PointPv , v3Ray );' + #10#13 +
    ' vec3 v3PointP = v3PointPv;' + #10#13 +
    ' float fSampleLength = fFarPvPa / float(iNumSamples);' + #10#13 +
    ' vec3 v3SampleRay = v3Ray * fSampleLength;' + #10#13 +
    ' v3PointP += v3SampleRay * 0.5;' + #10#13 +
    ' v3PointP += v3SampleRay * Tex0.y * iNumSamples;' + #10#13 +
    ' vec2 ViewerOpticalDepth = t( v3PointP, v3PointPv );' + #10#13 +
    ' vec2 SunOpticalDepth = t( v3PointP, v3Ray * fFarPvPa + v3PointPv );' + #10#13
    +
    ' FragColor = vec4( ViewerOpticalDepth, SunOpticalDepth.xy );' + #10#13 +
    '}';
{$IFDEF GLS_COMPILER_2005_UP}{$ENDREGION}{$ENDIF}

const
  cTexPrec: array[TGLNishitaSkyTexPrec] of TGLInternalFormat =
    (tfR11F_G11F_B10F, tfRGBA_FLOAT16, tfRGBA_FLOAT32);

var
  RenderShader: TGLProgramHandle = nil;
  UpdateShader: TGLProgramHandle = nil;
  UpdateFastShader: TGLProgramHandle = nil;
  CreateOpticalDepthShader: TGLProgramHandle = nil;
  ProgramWorks: Boolean = true;
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  SaveOnce: Boolean = true;
{$ENDIF}

constructor TGL3xCustomNishitaSky.Create(AOwner: TComponent);
const
  ESun: Extended = 20.0;
  Kr: Extended = 0.0025;
  Km: Extended = 0.0010;
  g: Extended = -0.995;
  g2: Extended = 0.995 * 0.995;

  procedure SetupTexture(tex: TGLTexture);
  begin
    with tex do
    begin
      ImageClassName := TGLBlankImage.ClassName;
      TextureWrap := twSeparate;
      TextureWrapS := twClampToEdge;
      TextureWrapR := twClampToEdge;
      MinFilter := miLinear;
      TextureFormatEx := cTexPrec[FColorPrecision];
      Disabled := false;
    end;
  end;

begin
  inherited;

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
  SetVector(ConstantBlock.v2dRayleighMieScaleHeight, 0.5, 0.1);
  SetVector(ConstantBlock.InvWavelength4,
    1.0 / power(0.650, 4),
    1.0 / power(0.570, 4),
    1.0 / power(0.475, 4),
    0);
  SetVector(ConstantBlock.WavelengthMie,
    power(0.650, -0.84),
    power(0.570, -0.84),
    power(0.475, -0.84),
    0);
  SetVector(ConstantBlock.v3HG,
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
  SetVector(ConstantBlock.InvRayleighMieN,
    1.0 / FRayleighMieN,
    1.0 / (FRayleighMieN / 2));
  SetVector(ConstantBlock.InvRayleighMieNLessOne,
    1.0 / (FRayleighMieN - 1),
    1.0 / (FRayleighMieN / 2 - 1));

  FChanges := [];
  SetOclock(12.0);

  FUBO := TGLUniformBufferHandle.Create;

  OpticalDepthFBO := TGLFrameBuffer.Create;
  RayleighMieFBO := TGLFrameBuffer.Create;
  FMieTexture := TGLTexture.Create(Self);
  FRayleighTexture := TGLTexture.Create(Self);
  FOpticalDepthTexture := TGLTexture.Create(Self);
  FColorPrecision := nsp11bit;
  SetupTexture(FMieTexture);
  SetupTexture(FRayleighTexture);
  SetupTexture(FOpticalDepthTexture);

  FFastUpdate := false;
end;

destructor TGL3xCustomNishitaSky.Destroy;
begin
  FUBO.Destroy;
  FMieTexture.Destroy;
  FRayleighTexture.Destroy;
  FOpticalDepthTexture.Destroy;
  OpticalDepthFBO.Destroy;
  RayleighMieFBO.Destroy;
  inherited;
end;

procedure TGL3xCustomNishitaSky.Initialize(var rci: TRenderContextInfo);
const
  cDrawBuffers: array[0..1] of GLenum =
    (
    GL_COLOR_ATTACHMENT0,
    GL_COLOR_ATTACHMENT1
    );
var
  code: AnsiString;
begin
  // Initialize shaders
  if (RenderShader.Handle = 0) and ProgramWorks then
  begin
    with RenderShader do
    begin
      AllocateHandle;
      Name := 'RenderShader';
      AddShader(TGLVertexShaderHandle, string(Render_vp), true);
      code := Header_fp + Render_fp;
      AddShader(TGLFragmentShaderHandle, string(code), true);
      if not LinkProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;

      if not ValidateProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
    end;
    with UpdateShader do
    begin
      AllocateHandle;
      Name := 'UpdateShader';
      AddShader(TGLVertexShaderHandle, string(Update_vp), true);
      code := Header_fp + Share_fp + Update_fp;
      AddShader(TGLFragmentShaderHandle, string(code), true);
      BindFragDataLocation(0, 'Mie');
      BindFragDataLocation(1, 'RayLeigh');
      if not LinkProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
      if not ValidateProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
    end;
    with UpdateFastShader do
    begin
      AllocateHandle;
      Name := 'UpdateFastShader';
      AddShader(TGLVertexShaderHandle, string(Update_vp), true);
      code := Header_fp + Share_fp + UpdateFast_fp;
      AddShader(TGLFragmentShaderHandle, string(code), true);
      BindFragDataLocation(0, 'Mie');
      BindFragDataLocation(1, 'RayLeigh');
      if not LinkProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
      if not ValidateProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
    end;
    with CreateOpticalDepthShader do
    begin
      AllocateHandle;
      Name := 'CreateOpticalDepthShader';
      AddShader(TGLVertexShaderHandle, string(Update_vp), true);
      code := Header_fp + Share_fp + CreateOpticalDepth_fp;
      AddShader(TGLFragmentShaderHandle, string(code), true);
      if not LinkProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
      if not ValidateProgram then
      begin
{$IFDEF NISHITA_SKY_DEBUG_MODE}
        Log.Log(InfoLog);
{$ENDIF}
        ProgramWorks := false;
      end;
    end;
  end;
  // Initialize constant buffer
  if FUBO.Handle = 0 then
  begin
    with FUBO do
    begin
      AllocateHandle;
      BindBufferData(@ConstantBlock, SizeOf(TNSConstantBlock), GL_STATIC_READ);
      Unbind;
    end;
  end;
  // Initialize textures
  if not FOpticalDepthTexture.IsHandleAllocated and ProgramWorks then
  begin
    TGLBlankImage(FOpticalDepthTexture.Image).Width := FOpticalDepthN;
    TGLBlankImage(FOpticalDepthTexture.Image).Height := FOpticalDepthN;
    FOpticalDepthTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    TGLBlankImage(FRayleighTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FRayleighTexture.Image).Height := FRayleighMieN div 2;
    TGLBlankImage(FMieTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FMieTexture.Image).Height := FRayleighMieN div 2;
    FRayleighTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.AllocateHandle;
    FRayleighTexture.AllocateHandle;
    FOpticalDepthTexture.AllocateHandle;
    OpticalDepthFBO.Bind;
    OpticalDepthFBO.AttachTexture(0, FOpticalDepthTexture);
    Assert(OpticalDepthFBO.Status = fsComplete, 'Framebuffer not complete');
    RayleighMieFBO.Bind;
    RayleighMieFBO.AttachTexture(0, FMieTexture);
    RayleighMieFBO.AttachTexture(1, FRayleighTexture);
    glDrawBuffers(2, @cDrawBuffers);
    Assert(OpticalDepthFBO.Status = fsComplete, 'Framebuffer not complete');
    MakeGPUOpticalDepth(rci);
  end;
  // Updates
  if nscConstants in FChanges then
  begin
    ConstantBlock.InvOpticalDepthN := 1.0 / FOpticalDepthN;
    ConstantBlock.InvOpticalDepthNLessOne := 1.0 / (FOpticalDepthN - 1);
    ConstantBlock.HalfTexelOpticalDepthN := 0.5 / FOpticalDepthN;
    SetVector(ConstantBlock.InvRayleighMieN,
      1.0 / FRayleighMieN,
      1.0 / (FRayleighMieN / 2));
    SetVector(ConstantBlock.InvRayleighMieNLessOne,
      1.0 / (FRayleighMieN - 1),
      1.0 / (FRayleighMieN / 2 - 1));
    FUBO.BindBufferData(@ConstantBlock, SizeOf(TNSConstantBlock),
      GL_STATIC_READ);
    Exclude(FChanges, nscConstants);
    Include(FChanges, nscTime);
  end;

  if nscRayleighMie in FChanges then
  begin
    TGLBlankImage(FRayleighTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FRayleighTexture.Image).Height := FRayleighMieN div 2;
    TGLBlankImage(FMieTexture.Image).Width := FRayleighMieN;
    TGLBlankImage(FMieTexture.Image).Height := FRayleighMieN div 2;
    FRayleighTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    FMieTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    Exclude(FChanges, nscRayleighMie);
    Include(FChanges, nscTime);
  end;

  if nscOpticalDepth in FChanges then
  begin
    TGLBlankImage(FOpticalDepthTexture.Image).Width := FOpticalDepthN;
    TGLBlankImage(FOpticalDepthTexture.Image).Height := FOpticalDepthN;
    FOpticalDepthTexture.TextureFormatEx := cTexPrec[FColorPrecision];
    MakeGPUOpticalDepth(rci);
    Exclude(FChanges, nscOpticalDepth);
    Include(FChanges, nscTime);
  end;

end;

procedure TGL3xCustomNishitaSky.MakeGPUOpticalDepth(var rci:
  TRenderContextInfo);
{$IFDEF NISHITA_SKY_DEBUG_MODE}
var
  debugImg: TGLDDSImage;
  uniformBlockSize: Integer;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
  OpticalDepthFBO.Bind;
  glViewport(0, 0, FOpticalDepthN, FOpticalDepthN);
  with rci.GLStates do
  begin
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
    SetDepthRange(0, 1);
  end;
  CreateOpticalDepthShader.UseProgramObject;

  FUBO.BindRange(CreateOpticalDepthShader.GetUniformBlockIndex('ConstantBlock'),
    0, SizeOf(TNSConstantBlock));

  with DynamicVBOManager do
  begin
    BeginObject(FBuiltProperties);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    Vertex(-1, -1);
    Vertex(-1, 1);
    Vertex(1, -1);
    Vertex(1, 1);
    EndPrimitives;
    EndObject;
  end;
  OpticalDepthFBO.Unbind;
  with rci.viewPortSize do
    glViewport(0, 0, cx, cy);

{$IFDEF NISHITA_SKY_DEBUG_MODE}
  debugImg := TGLDDSImage.Create;
  if FColorPrecision = nsp11bit then
  begin
    useCurrent := false;
    castFormat := tfRGBA_FLOAT16;
  end
  else begin
    useCurrent := true;
    castFormat := tfRGBA8;
  end;
  debugImg.AssignFromTexture(FOpticalDepthTexture.RenderingContext,
    FOpticalDepthTexture.Handle,
    FOpticalDepthTexture.Image.NativeTextureTarget, useCurrent, castFormat);
  debugImg.SaveToFile('OpticalDepth.dds');
  debugImg.Free;
  glGetActiveUniformBlockiv(CreateOpticalDepthShader.Handle, 0,
    GL_UNIFORM_BLOCK_DATA_SIZE, @uniformBlockSize);
  Log.Log(Format('GPU Uniform block size = %d', [uniformBlockSize]));
  uniformBlockSize := SizeOf(TNSConstantBlock);
  Log.Log(Format('CPU Uniform block size = %d', [uniformBlockSize]));
{$ENDIF}
end;

procedure TGL3xCustomNishitaSky.MakeGPUMieRayleighBuffer(var rci:
  TRenderContextInfo);
var
  UpShader: TGLProgramHandle;
{$IFDEF NISHITA_SKY_DEBUG_MODE}
  debugImg: TGLDDSImage;
  useCurrent: Boolean;
  castFormat: TGLInternalFormat;
{$ENDIF}
begin
  RayleighMieFBO.Bind;
  glViewport(0, 0, FRayleighMieN, FRayleighMieN div 2);
  with rci.GLStates do
  begin
    Disable(stBlend);
    Disable(stDepthTest);
    Disable(stCullFace);
    DepthWriteMask := False;
    SetDepthRange(0, 1);
  end;
  if FFastUpdate then
    UpShader := UpdateFastShader
  else
    UpShader := UpdateShader;
  with UpShader do
  begin
    UseProgramObject;
    FUBO.BindRange(GetUniformBlockIndex('ConstantBlock'),
      0, SizeOf(TNSConstantBlock));
    Uniform3f['v3SunDir'] := v3SunDir;
    if FFastUpdate then
      with FOpticalDepthTexture do
        UniformTextureHandle['OpticalDepth', 0,
          Image.NativeTextureTarget] := Handle;
  end;
  with DynamicVBOManager do
  begin
    BeginObject(FBuiltProperties);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    Vertex(-1, -1);
    Vertex(-1,  1);
    Vertex( 1, -1);
    Vertex( 1,  1);
    EndPrimitives;
    EndObject;
  end;
  RayleighMieFBO.Unbind;
  with rci.viewPortSize do
    glViewport(0, 0, cx, cy);
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
    else begin
      useCurrent := true;
      castFormat := tfRGBA8;
    end;
    debugImg.AssignFromTexture(FMieTexture.RenderingContext, FMieTexture.Handle,
      FMieTexture.Image.NativeTextureTarget, useCurrent, castFormat);
    debugImg.SaveToFile('Mie.dds');
    debugImg.AssignFromTexture(FRayleighTexture.RenderingContext,
      FRayleighTexture.Handle,
      FRayleighTexture.Image.NativeTextureTarget, useCurrent, castFormat);
    debugImg.SaveToFile('Rayleigh.dds');
    debugImg.Free;
    SaveOnce := false;
  end;
{$ENDIF}
end;

procedure TGL3xCustomNishitaSky.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  M, VM: TMatrix;
  storeFrameBuffer: TGLuint;
begin
  // Render self
  if GL_VERSION_3_2 and ARenderSelf then
  begin
    storeFrameBuffer := ARci.GLStates.DrawFrameBuffer;
    Initialize(ARci);
    if ProgramWorks then
    begin
      if nscTime in FChanges then
        MakeGPUMieRayleighBuffer(ARci);
      ARci.GLStates.DrawFrameBuffer := storeFrameBuffer;
      with RenderShader do
      begin
        UseProgramObject;

        FBuiltProperties.Usage := FBuiltProperties.Usage;
        VM := TGLSceneBuffer(ARci.buffer).ViewMatrix;
        VM[3, 0] := 0;
        VM[3, 1] := 0;
        VM[3, 2] := 0;
        M := MatrixMultiply(TGLSceneBuffer(ARci.buffer).ModelMatrix, VM);
        M := MatrixMultiply(M, TGLSceneBuffer(ARci.buffer).ProjectionMatrix);
        UniformMatrix4fv['ModelViewProjectionMatrix'] := M;

        with FMieTexture do
          UniformTextureHandle['Mie', 0,
            Image.NativeTextureTarget] := Handle;
        with FRayleighTexture do
          UniformTextureHandle['Rayleigh', 1,
            Image.NativeTextureTarget] := Handle;
        Uniform3f['v3SunDir'] := v3SunDir;

        with ARci.GLStates do
        begin
          Disable(stBlend);
          Enable(stDepthTest);
          Enable(stCullFace);
          DepthWriteMask := False;
          SetDepthRange(0, 1);
        end;

        if not FBuiltProperties.Manager.IsBuilded then
        begin
          try
            Self.BuildList(ARci);
          except
            FBuiltProperties.Manager.Discard;
            Self.Visible := false;
          end;
          ObjectStyle := ObjectStyle - [osBuiltStage];
        end
        else
          FBuiltProperties.Manager.RenderClient(FBuiltProperties, ARci);
      end;
    end;
  end;

  // Render children
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGL3xCustomNishitaSky.BuildList(var rci: TRenderContextInfo);
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

  with BuiltProperties.Manager do
  begin
    BeginObject(BuiltProperties);
    Attribute2f(attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
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
        Vertex(V2[0], V2[1], V2[2]);
        Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
        Vertex(V1[0], V1[1], V1[2]);
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
        Vertex(V2[0], V2[1], V2[2]);
        Attribute2f(attrTexCoord0, uTexCoord0, vTexCoord);
        Vertex(V1[0], V1[1], V1[2]);
        Theta := Theta - StepH;
      end;

      RestartStrip;
      Phi := Phi2;
      Phi2 := Phi2 - StepV;
    end;
    EndPrimitives;
    EndObject;
  end;
end;

procedure TGL3xCustomNishitaSky.Assign(Source: TPersistent);
var
  sky: TGL3xCustomNishitaSky;
begin
  inherited;
  if Source is TGL3xCustomNishitaSky then
  begin
    sky := TGL3xCustomNishitaSky(Source);
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

procedure TGL3xCustomNishitaSky.SetOclock(Value: Double);
begin
  FOclock := Value;
  Value := frac((Value - 12) / 24);
  v3SunDir := AffineVectorMake(sin(2 * Pi * Value), cos(2 * Pi * Value), 0);
  Include(FChanges, nscTime);
  StructureChanged;
end;

function TGL3xCustomNishitaSky.StoreOclock: Boolean;
begin
  Result := FOclock <> 12.0;
end;

procedure TGL3xCustomNishitaSky.SetDomeDivision(Value: Integer);
begin
  if Value < 16 then
    Value := 16;
  if Value <> FDomeDiv then
  begin
    fDomeDiv := Value;
    StructureChanged;
  end;
end;

procedure TGL3xCustomNishitaSky.SetOpticalDepth(Value: Integer);
begin
  if Value <> FOpticalDepthN then
  begin
    FOpticalDepthN := Value;
    Include(FChanges, nscConstants);
    Include(FChanges, nscOpticalDepth);
    StructureChanged;
  end;
end;

procedure TGL3xCustomNishitaSky.SetRayleighMie(Value: Integer);
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
    StructureChanged;
  end;
end;

procedure TGL3xCustomNishitaSky.SetColorPrecision(const Value:
  TGLNishitaSkyTexPrec);
begin
  if Value <> FColorPrecision then
  begin
    FColorPrecision := Value;
    Include(FChanges, nscRayleighMie);
    Include(FChanges, nscOpticalDepth);
    StructureChanged;
  end;
end;

function TGL3xCustomNishitaSky.GetMieScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[1];
end;

function TGL3xCustomNishitaSky.GetRayleighScaleHeight: Single;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[0];
end;

procedure TGL3xCustomNishitaSky.SetMieScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight[1] then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight[1] := Value;
    Include(FChanges, nscConstants);
    StructureChanged;
  end;
end;

procedure TGL3xCustomNishitaSky.SetRayleighScaleHeight(Value: Single);
begin
  if Value <> ConstantBlock.v2dRayleighMieScaleHeight[0] then
  begin
    ConstantBlock.v2dRayleighMieScaleHeight[0] := Value;
    Include(FChanges, nscConstants);
    StructureChanged;
  end;
end;

function TGL3xCustomNishitaSky.StoreMieScaleHeight: Boolean;
const
  MSH: Single = 0.1;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[1] <> MSH;
end;

function TGL3xCustomNishitaSky.StoreRayleighScaleHeight: Boolean;
const
  RSH: Single = 0.5;
begin
  Result := ConstantBlock.v2dRayleighMieScaleHeight[0] <> RSH;
end;

initialization

  RenderShader := TGLProgramHandle.Create;
  UpdateShader := TGLProgramHandle.Create;
  UpdateFastShader := TGLProgramHandle.Create;
  CreateOpticalDepthShader := TGLProgramHandle.Create;
  RegisterClasses([TGL3xCustomNishitaSky, TGL3xNishitaSky]);

finalization

  RenderShader.Destroy;
  RenderShader := nil;
  UpdateShader.Destroy;
  UpdateShader := nil;
  UpdateFastShader.Destroy;
  UpdateFastShader := nil;
  CreateOpticalDepthShader.Destroy;
  CreateOpticalDepthShader := nil;

end.

