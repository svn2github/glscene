{: GLState<p>

   Miscellaneous support routines & classes.<p>

 <b>History : </b><font size=-1><ul>
      <li>22/02/10 - Yar - Added more control of states
      <li>13/05/07 - fig - Added stTexture3D (GL_TEXTURE_3D)
      <li>19/12/06 - DaStr - GetGLCurrentTexture, ResetGLTexture added to TGLStateCache
      <li>04/10/04 - NC - Added stTextureRect (GL_TEXTURE_RECTANGLE_NV)
      <li>07/01/04 - EG - Introduced TGLStateCache
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}
unit GLState;

interface

uses Classes, VectorGeometry, SysUtils, OpenGL1x;

{$I GLScene.inc}

type

  // TGLState
  //
//: Reflects all relevant (binary) states of OpenGL subsystem
  TGLState = (stAlphaTest, stAutoNormal,
    stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
    stFog, stLighting, stLineSmooth, stLineStipple,
    stLogicOp, stNormalize, stPointSmooth, stPointSprite, stPolygonSmooth,
    stPolygonStipple, stScissorTest, stStencilTest,
    stTexture1D, stTexture2D, stTextureCubeMap, stTextureRect,
    stTexture3D);

  TGLStates = set of TGLState;

  // TFaceWinding
  //
//: Describe what kind of winding has a front face
  TFaceWinding = (fwCounterClockWise, fwClockWise);

  // TGLStateCache
  //
  {: Manages an application-side cache of OpenGL states and parameters.<p>
     Purpose of this class is to eliminate redundant state and parameter
     changes, and there will typically be no more than one state cache per
     OpenGL context. }
  TGLStateCache = class
  private
    { Private Declarations }
    FFrontBackColors: array[0..1, 0..3] of TVector;
    FFrontBackShininess: array[0..1] of Integer;
    FBlendFunc: array[0..1] of TGLEnum;
    FAlphaFunc: TGLEnum;
    FAlphaRef: TGLclampf;
    FDepthFunc: TGLEnum;
    FDepthRange: array[0..1] of TGLclampf;
    FColorWriting: Boolean;
    FDepthWriting: Boolean;
    FLineWidth: Single;
    FStates: TGLStates;
    FTextureHandle: array[0..7] of Integer;
    FLastFrontMode, FLastBackMode: TGLEnum;
    FFrontFaceCCW: Boolean;
    FTextureMatrixIsIdenty: Boolean;
    FIgnoreDeprecation: Boolean;
  protected
    { Protected Declarations }

  public
    { Public Declarations }
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetGLState(const aState: TGLState);
    procedure UnSetGLState(const aState: TGLState);
    procedure PerformSetGLState(const aState: TGLState);
    procedure PerformUnSetGLState(const aState: TGLState);

    {: Adjusts PolygonMode for a face }
    procedure SetGLPolygonMode(const aFace, mode: TGLEnum);
    //: Reset GLPolygonMode, next calls to SetGLPolygonMode WILL do something
    procedure ResetGLPolygonMode;

    {: Adjusts material colors for a face. }
    procedure SetGLMaterialColors(const aFace: TGLEnum;
      const emission, ambient, diffuse, specular: TVector;
      const shininess: Integer);
    {: Adjusts material alpha channel for a face. }
    procedure SetGLMaterialAlphaChannel(const aFace: TGLEnum; const alpha:
      TGLFloat);
    //: Reset GLMaterial colors, next calls to SetGLMaterial WILL do something
    procedure ResetGLMaterialColors;

    {: Blending states }
    procedure SetGLBlendFuncion(sFactor, dFactor: TGLEnum);
    procedure ResetGLBlendFuncion;
    procedure SetGLAlphaFuncion(func: TGLEnum; ref: TGLclampf);
    procedure ResetGLAlphaFuncion;
    {: Depth states }
    procedure SetGLDepthFunction(func: TGLEnum);
    procedure SetGLDepthRange(Znear, Zfar: TGLclampf);
    procedure ResetGLDepthState;

    procedure SetGLLineWidth(width: Single);

    {: Specify a new texture handle for the target of textureUnit.<p>
       Does NOT perform glActiveTextureARB calls. }
    procedure SetGLCurrentTexture(const textureUnit, target, handle: Integer);
    function GetGLCurrentTexture(const TextureUnit: Integer): Integer;
    procedure ResetGLTexture(const TextureUnit: Integer);
    procedure ResetGLCurrentTexture;
    {: Defines the OpenGL texture matrix.<p>
       Assumed texture mode is GL_MODELVIEW. }
    procedure SetGLTextureMatrix(const matrix: TMatrix);
    {: Resets the OpenGL texture matrix to Identity.<p>
       Assumed texture mode is GL_MODELVIEW. }
    procedure ResetGLTextureMatrix;

    procedure SetGLColorWriting(flag: Boolean);
    procedure SetGLDepthWriting(flag: Boolean);

    {: Inverts front face winding (CCW/CW). }
    procedure InvertGLFrontFace;
    {: Reset to default front face winding (CCW). }
    procedure ResetGLFrontFace;
    {: Set front face winding to ClockWise. }
    procedure SetGLFrontFaceCW;
    {: Set front face winding to Counter-ClockWise. }
    procedure SetGLFrontFaceCCW;

    {: Invokes all Reset methods. }
    procedure ResetAll;

    // read only properties
    property States: TGLStates read FStates;

    {: True for ignore deprecated and removed features in OpenGL 3x }
    property IgnoreDeprecation: Boolean read FIgnoreDeprecation
      write FIgnoreDeprecation;
  end;

  //------------------------------------------------------
  //------------------------------------------------------
  //------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

type
  TStateRecord = record
    GLConst: TGLEnum;
    GLDeprecated: Boolean;
  end;

const
  cGLStateToGLEnum: array[TGLState] of TStateRecord =
    ((GLConst: GL_ALPHA_TEST; GLDeprecated: True),
    (GLConst: GL_AUTO_NORMAL; GLDeprecated: True),
    (GLConst: GL_BLEND; GLDeprecated: False),
    (GLConst: GL_COLOR_MATERIAL; GLDeprecated: True),
    (GLConst: GL_CULL_FACE; GLDeprecated: False),
    (GLConst: GL_DEPTH_TEST; GLDeprecated: False),
    (GLConst: GL_DITHER; GLDeprecated: False),
    (GLConst: GL_FOG; GLDeprecated: True),
    (GLConst: GL_LIGHTING; GLDeprecated: True),
    (GLConst: GL_LINE_SMOOTH; GLDeprecated: True),
    (GLConst: GL_LINE_STIPPLE; GLDeprecated: True),
    (GLConst: GL_LOGIC_OP; GLDeprecated: False),
    (GLConst: GL_NORMALIZE; GLDeprecated: True),
    (GLConst: GL_POINT_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POINT_SPRITE; GLDeprecated: True),
    (GLConst: GL_POLYGON_SMOOTH; GLDeprecated: True),
    (GLConst: GL_POLYGON_STIPPLE; GLDeprecated: True),
    (GLConst: GL_SCISSOR_TEST; GLDeprecated: False),
    (GLConst: GL_STENCIL_TEST; GLDeprecated: False),
    (GLConst: GL_TEXTURE_1D; GLDeprecated: True),
    (GLConst: GL_TEXTURE_2D; GLDeprecated: True),
    (GLConst: GL_TEXTURE_CUBE_MAP; GLDeprecated: True),
    (GLConst: GL_TEXTURE_RECTANGLE; GLDeprecated: False),
    (GLConst: GL_TEXTURE_3D; GLDeprecated: True));

  // ------------------
  // ------------------ TGLStateCache ------------------
  // ------------------

  // Create
  //

constructor TGLStateCache.Create;
begin
  inherited;
  FTextureMatrixIsIdenty := True;
  FFrontFaceCCW := True;
  FIgnoreDeprecation := False;
end;

// Destroy
//

destructor TGLStateCache.Destroy;
begin
  inherited;
end;

// SetGLState
//

procedure TGLStateCache.SetGLState(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FIgnoreDeprecation then
    exit;
  if not (aState in FStates) then
  begin
    Include(FStates, aState);
    glEnable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

// UnSetGLState
//

procedure TGLStateCache.UnSetGLState(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FIgnoreDeprecation then
    exit;
  if (aState in FStates) then
  begin
    Exclude(FStates, aState);
    glDisable(cGLStateToGLEnum[aState].GLConst);
  end;
end;

// PerformSetGLState
//

procedure TGLStateCache.PerformSetGLState(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FIgnoreDeprecation then
    exit;
  Include(FStates, aState);
  glEnable(cGLStateToGLEnum[aState].GLConst);
end;

// PerformUnSetGLState
//

procedure TGLStateCache.PerformUnSetGLState(const aState: TGLState);
begin
  if cGLStateToGLEnum[aState].GLDeprecated and FIgnoreDeprecation then
    exit;
  Exclude(FStates, aState);
  glDisable(cGLStateToGLEnum[aState].GLConst);
end;

// SetGLPolygonMode
//

procedure TGLStateCache.SetGLPolygonMode(const aFace, mode: TGLEnum);
begin
  case aFace of
    GL_FRONT:
      if mode <> FLastFrontMode then
      begin
        FLastFrontMode := mode;
        glPolygonMode(aFace, mode);
      end;
    GL_BACK:
      if mode <> FLastBackMode then
      begin
        FLastBackMode := mode;
        glPolygonMode(aFace, mode);
      end;
    GL_FRONT_AND_BACK:
      if (mode <> FLastFrontMode) or (mode <> FLastBackMode) then
      begin
        FLastFrontMode := mode;
        FLastBackMode := mode;
        glPolygonMode(aFace, mode);
      end;
  end;
end;

// ResetGLPolygonMode
//

procedure TGLStateCache.ResetGLPolygonMode;
begin
  FLastFrontMode := 0;
  FLastBackMode := 0;
end;

// SetGLMaterialColors
//

procedure TGLStateCache.SetGLMaterialColors(const aFace: TGLEnum;
  const emission, ambient, diffuse, specular: TVector;
  const shininess: Integer);
var
  i: Integer;
begin
  if FIgnoreDeprecation then
    exit;

  i := aFace - GL_FRONT;
  if FFrontBackShininess[i] <> shininess then
  begin
    glMateriali(AFace, GL_SHININESS, shininess);
    FFrontBackShininess[i] := shininess;
  end;
  if not AffineVectorEquals(FFrontBackColors[i][0], emission) then
  begin
    glMaterialfv(aFace, GL_EMISSION, @emission);
    SetVector(FFrontBackColors[i][0], emission);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][1], ambient) then
  begin
    glMaterialfv(aFace, GL_AMBIENT, @ambient);
    SetVector(FFrontBackColors[i][1], ambient);
  end;
  if not VectorEquals(FFrontBackColors[i][2], diffuse) then
  begin
    glMaterialfv(aFace, GL_DIFFUSE, @diffuse);
    SetVector(FFrontBackColors[i][2], diffuse);
  end;
  if not AffineVectorEquals(FFrontBackColors[i][3], specular) then
  begin
    glMaterialfv(aFace, GL_SPECULAR, @specular);
    SetVector(FFrontBackColors[i][3], specular);
  end;
end;

// SetGLMaterialAlphaChannel
//

procedure TGLStateCache.SetGLMaterialAlphaChannel(const aFace: TGLEnum; const
  alpha: TGLFloat);
var
  i: Integer;
begin
  if FIgnoreDeprecation then
    exit;
  i := aFace - GL_FRONT;
  if FFrontBackColors[i][2][3] <> alpha then
  begin
    FFrontBackColors[i][2][3] := alpha;
    glMaterialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
  end;
end;

// ResetGLMaterialColors
//

procedure TGLStateCache.ResetGLMaterialColors;
const
  clrBlack: TVector = (0, 0, 0, 1);
  clrGray20: TVector = (0.20, 0.20, 0.20, 1);
  clrGray80: TVector = (0.80, 0.80, 0.80, 1);
begin
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
  glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, 0);
  FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
  FFrontBackShininess[0] := 255;
  FFrontBackShininess[1] := 255;
end;

procedure TGLStateCache.SetGLBlendFuncion(sFactor, dFactor: TGLEnum);
begin
  if (sFactor <> FBlendFunc[0]) or (dFactor <> FBlendFunc[1]) then
  begin
    FBlendFunc[0] := sFactor;
    FBlendFunc[1] := dFactor;
    glBlendFunc(sFactor, dFactor);
  end;
end;

procedure TGLStateCache.ResetGLBlendFuncion;
begin
  FBlendFunc[0] := $FFFFFFFF;
  FBlendFunc[1] := $FFFFFFFF;
end;

procedure TGLStateCache.SetGLAlphaFuncion(func: TGLEnum; ref: TGLclampf);
begin
  if FIgnoreDeprecation then
    exit;
  if (FAlphaFunc <> func) or (FAlphaRef <> ref) then
  begin
    FAlphaFunc := func;
    FAlphaRef := ref;
    glAlphaFunc(func, ref);
  end;
end;

procedure TGLStateCache.ResetGLAlphaFuncion;
begin
  FAlphaFunc := $FFFFFFFF;
  FAlphaRef := -1;
end;

procedure TGLStateCache.SetGLDepthFunction(func: TGLEnum);
begin
  if FDepthFunc <> func then
  begin
    FDepthFunc := func;
    glDepthFunc(func);
  end;
end;

procedure TGLStateCache.SetGLDepthRange(Znear, Zfar: TGLclampf);
begin
  if (Znear <> FDepthRange[0]) or (Zfar <> FDepthRange[1]) then
  begin
    FDepthRange[0] := Znear;
    FDepthRange[1] := Zfar;
    glDepthRange(ZNear, ZFar);
  end;
end;

procedure TGLStateCache.ResetGLDepthState;
begin
  FDepthFunc := $FFFFFFFF;
  FDepthRange[0] := -1;
  FDepthRange[1] := -1;
  FDepthWriting := True;
  glDepthMask(True);
end;

procedure TGLStateCache.SetGLLineWidth(width: Single);
begin
  if FIgnoreDeprecation then
    if (width = 0) or (width > 1) then
      exit;
  if width <> FLineWidth then
  begin
    FLineWidth := width;
    glLineWidth(FLineWidth);
  end;
end;

// GetGLCurrentTexture
//

function TGLStateCache.GetGLCurrentTexture(
  const TextureUnit: Integer): Integer;
begin
  Result := FTextureHandle[TextureUnit];
end;

// ResetGLTexture
//

procedure TGLStateCache.ResetGLTexture(const TextureUnit: Integer);
begin
  FTextureHandle[TextureUnit] := -1;
end;

// SetGLCurrentTexture
//

procedure TGLStateCache.SetGLCurrentTexture(const textureUnit, target, handle:
  Integer);
begin
  if handle <> FTextureHandle[textureUnit] then
  begin
    glBindTexture(target, handle);
    FTextureHandle[textureUnit] := handle;
  end;
end;

// ResetGLCurrentTexture
//

procedure TGLStateCache.ResetGLCurrentTexture;
var
  i: Integer;
begin
  for i := 0 to 7 do
    FTextureHandle[i] := -1;
end;

// SetGLTextureMatrix
//

procedure TGLStateCache.SetGLTextureMatrix(const matrix: TMatrix);
begin
  if FIgnoreDeprecation then
    exit;
  FTextureMatrixIsIdenty := False;
  glMatrixMode(GL_TEXTURE);
  glLoadMatrixf(PGLFloat(@matrix[0][0]));
  glMatrixMode(GL_MODELVIEW);
end;

// ResetGLTextureMatrix
//

procedure TGLStateCache.ResetGLTextureMatrix;
begin
  if FIgnoreDeprecation then
    exit;
  if not FTextureMatrixIsIdenty then
  begin
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    FTextureMatrixIsIdenty := True;
  end;
end;

// SetGLColorIgnoring
//

procedure TGLStateCache.SetGLColorWriting(flag: Boolean);
begin
  if FColorWriting <> flag then
  begin
    FColorWriting := flag;
    glColorMask(flag, flag, flag, flag);
  end;
end;

// SetGLDepthIgnoring
//

procedure TGLStateCache.SetGLDepthWriting(flag: Boolean);
begin
  if FDepthWriting <> flag then
  begin
    FDepthWriting := flag;
    glDepthMask(flag);
  end;
end;

// InvertGLFrontFace
//

procedure TGLStateCache.InvertGLFrontFace;
begin
  FFrontFaceCCW := not FFrontFaceCCW;
  if FFrontFaceCCW then
    glFrontFace(GL_CCW)
  else
    glFrontFace(GL_CW);
end;

// ResetGLFrontFace
//

procedure TGLStateCache.ResetGLFrontFace;
begin
  glFrontFace(GL_CCW);
  FFrontFaceCCW := True;
end;

// SetGLFrontFaceCW
//

procedure TGLStateCache.SetGLFrontFaceCW;
begin
  if FFrontFaceCCW then
  begin
    glFrontFace(GL_CW);
    FFrontFaceCCW := False;
  end;
end;

// SetGLFrontFaceCCW
//

procedure TGLStateCache.SetGLFrontFaceCCW;
begin
  if not FFrontFaceCCW then
  begin
    glFrontFace(GL_CCW);
    FFrontFaceCCW := True;
  end;
end;

// ResetAll
//

procedure TGLStateCache.ResetAll;
begin
  ResetGLPolygonMode;
  ResetGLMaterialColors;
  ResetGLCurrentTexture;
  ResetGLFrontFace;
  ResetGLBlendFuncion;
  ResetGLAlphaFuncion;
  FColorWriting := True;
  glColorMask(True, True, True, True);
  ResetGLDepthState;
  FLineWidth := -1;
end;

end.

