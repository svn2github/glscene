unit GL3xObjects;

interface

{$I GLScene.inc}

uses
  Classes,
  BaseClasses,
  VectorGeometry,
  GLScene,
  OpenGLTokens,
  SysUtils,
  GLCrossPlatform,
  GLContext,
  GLSilhouette,
  GLSLShader,
  GLRenderContextInfo,
  GLCoordinates,
  GLObjects,
  GLShaderManager,
  GL3xMaterial,
  GL3xMesh,
  GL3xStaticMesh,
  GLDrawTechnique,
  GLShaderEnvironment,
  GL3xFactory;

type

  // TGL3xBaseBufferedObject
  //

  TGL3xBaseSceneObject = class(TGLBaseSceneObject)
  protected
    { Protected Declarations }
    FMesh: IGLName;
    FMaterial: IGLName;
    function GetMaterialName: string; virtual;
    procedure SetMaterialName(const Value: string); virtual;

    procedure BuildMesh; virtual; abstract;
    procedure BeforeRender; virtual;
    procedure AfterRender; virtual;
  public
    { Public Declarations }
    destructor Destroy; override;
    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    property MaterialRefName: IGLName read FMaterial;
    property Material: string read GetMaterialName write SetMaterialName;
  end;

  TGL3xCustomObject = class(TGL3xBaseSceneObject)
  published
    { Published Declarations }
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

  TGL3xBasePrimitiveObject = class(TGL3xCustomObject)
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published Declarations }
    property Material;
  end;

  // TGL3xPlane
  //

  TGL3xPlane = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FXOffset, FYOffset: TGLFloat;
    FXScope, FYScope: TGLFloat;
    FWidth, FHeight: TGLFloat;
    FXTiles, FYTiles: Cardinal;
    FStyle: TPlaneStyles;
    procedure SetHeight(const aValue: Single);
    procedure SetWidth(const aValue: Single);
    procedure SetXOffset(const Value: TGLFloat);
    procedure SetXScope(const Value: TGLFloat);
    function StoreXScope: Boolean;
    procedure SetXTiles(const Value: Cardinal);
    procedure SetYOffset(const Value: TGLFloat);
    procedure SetYScope(const Value: TGLFloat);
    function StoreYScope: Boolean;
    procedure SetYTiles(const Value: Cardinal);
    procedure SetStyle(const val: TPlaneStyles);
  protected
    { Protected declaration }
    procedure BuildMesh; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function ScreenRect(aBuffer: TGLSceneBuffer): TGLRect;
    function PointDistance(const aPoint: TVector): Single;

  published
    { Public Declarations }
    property Height: TGLFloat read FHeight write SetHeight;
    property Width: TGLFloat read FWidth write SetWidth;
    property XOffset: TGLFloat read FXOffset write SetXOffset;
    property XScope: TGLFloat read FXScope write SetXScope stored StoreXScope;
    property XTiles: Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: TGLFloat read FYOffset write SetYOffset;
    property YScope: TGLFloat read FYScope write SetYScope stored StoreYScope;
    property YTiles: Cardinal read FYTiles write SetYTiles default 1;
    property Style: TPlaneStyles read FStyle write SetStyle default
      [psSingleQuad, psTileTexture];
  end;

  TGLSpriteAlign = (alSpherical, alCylindrical);

  // TGL3xSprite
  //

  TGL3xSprite = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FWidth: TGLFloat;
    FHeight: TGLFloat;
    FRotation: TGLFloat;
    FMirrorU,
      FMirrorV: Boolean;
    FAlign: TGLSpriteAlign;
    procedure SetWidth(const val: TGLFloat);
    procedure SetHeight(const val: TGLFloat);
    procedure SetRotation(const val: TGLFloat);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
  protected
    { Protected declaration }
    procedure BuildMesh; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure DoRender(var ARci: TRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure SetSize(const width, height: TGLFloat);
    procedure SetSquareSize(const size: TGLFloat);
  published
    property Width: TGLFloat read FWidth write SetWidth;
    property Height: TGLFloat read FHeight write SetHeight;
    property Rotation: TGLFloat read FRotation write SetRotation;
    property MirrorU: Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV: Boolean read FMirrorV write SetMirrorV default False;
    property Align: TGLSpriteAlign read FAlign write FAlign default alSpherical;
  end;

  // TGL3xCube
  //

  TGL3xCube = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FCubeSize: TAffineVector;
    FParts: TCubeParts;
    FNormalDirection: TNormalDirection;
    procedure SetCubeWidth(const aValue: Single);
    procedure SetCubeHeight(const aValue: Single);
    procedure SetCubeDepth(const aValue: Single);
    procedure SetParts(aValue: TCubeParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure BuildMesh; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

  published
    { Published Declarations }
    property CubeWidth: TGLFloat read FCubeSize[0] write SetCubeWidth stored
      False;
    property CubeHeight: TGLFloat read FCubeSize[1] write SetCubeHeight stored
      False;
    property CubeDepth: TGLFloat read FCubeSize[2] write SetCubeDepth stored
      False;
    property NormalDirection: TNormalDirection read FNormalDirection write
      SetNormalDirection default ndOutside;
    property Parts: TCubeParts read FParts write SetParts default [cpTop,
      cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  // TGL3xSphere
  //

  TGL3xSphere = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FSlices, FStacks: TGLInt;
    FTop: TAngleLimit1;
    FBottom: TAngleLimit1;
    FStart: TAngleLimit2;
    FStop: TAngleLimit2;
    FTopCap, FBottomCap: TCapType;

    procedure SetBottom(aValue: TAngleLimit1);
    procedure SetBottomCap(aValue: TCapType);
    procedure SetRadius(const aValue: TGLFloat);
    procedure SetSlices(aValue: TGLInt);
    procedure SetStart(aValue: TAngleLimit2);
    procedure SetStop(aValue: TAngleLimit2);
    procedure SetStacks(aValue: TGLInt);
    procedure SetTop(aValue: TAngleLimit1);
    procedure SetTopCap(aValue: TCapType);
  protected
    { Protected declaration }
    procedure BuildMesh; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;
  published
    { Published Declarations }
    property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap default
      ctNone;
    property Radius: TGLFloat read FRadius write SetRadius;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property Stacks: TGLInt read FStacks write SetStacks default 16;
    property Start: TAngleLimit2 read FStart write SetStart default 0;
    property Stop: TAngleLimit2 read FStop write SetStop default 360;
    property Top: TAngleLimit1 read FTop write SetTop default 90;
    property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
  end;

  // TGL3xGeoSphere
  //

  TGL3xGeoSphere = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FLevel: TGLInt;

    procedure SetRadius(const Value: TGLFloat);
    procedure SetSubdivisionLevel(Value: TGLInt);
  protected
    { Protected declaration }
    procedure BuildMesh; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

    function GenerateSilhouette(const silhouetteParameters:
      TGLSilhouetteParameters): TGLSilhouette; override;
  published
    { Published Declarations }
    property Radius: TGLFloat read FRadius write SetRadius;
    property SubdivisionLevel: TGLInt read FLevel write SetSubdivisionLevel
      default 2;
  end;

  TGL3xDisk = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius: TGLFloat;
    FSlices, FLoops: TGLInt;
    procedure SetOuterRadius(const aValue: Single);
    procedure SetInnerRadius(const aValue: Single);
    procedure SetSlices(aValue: TGLInt);
    procedure SetLoops(aValue: TGLInt);
    procedure SetStartAngle(const aValue: Single);
    procedure SetSweepAngle(const aValue: Single);
  protected
    { Protected declaration }
    procedure BuildMesh; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;

  published
    property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
    property Loops: TGLInt read FLoops write SetLoops default 2;
    property OuterRadius: TGLFloat read FOuterRadius write SetOuterRadius;
    property Slices: TGLInt read FSlices write SetSlices default 16;
    property StartAngle: TGLFloat read FStartAngle write SetStartAngle;
    property SweepAngle: TGLFloat read FSweepAngle write SetSweepAngle;
  end;

  TFeedBackMeshPrimitive = (bmpPoint, bmpLine, bmpTriangle);

  TGL3xFeedBackMesh = class(TGL3xBasePrimitiveObject)
  private
    { Private Declarations }
    FFactory: TGL3xBaseFactory;
    FShader: IGLName;
    FPrimitiveType: TFeedBackMeshPrimitive;
    FVertexNumber: Integer;
    FIndexed: Boolean;
    FAttrArray: TGLSLAttributeArray;
    FAttrIsDefined: Boolean;
    procedure SetFactory(Value: TGL3xBaseFactory);
    procedure SetPrimitiveType(Value: TFeedBackMeshPrimitive);
    procedure SetVertexNumber(Value: Integer);
    procedure SetIndexed(Value: Boolean);
    function GetShader: IGLName;
    procedure SetShader(AName: IGLName);
  protected
    { Protected declaration }
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure BuildMesh; override;
    procedure SetMaterialName(const Value: string); override;
    procedure BeforeRender; override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    property Shader: IGLName read GetShader write SetShader;
  published
    property Factory: TGL3xBaseFactory read FFactory
      write SetFactory;
    property PrimitiveType: TFeedBackMeshPrimitive read FPrimitiveType
      write SetPrimitiveType default bmpPoint;
    property VertexNumber: Integer read FVertexNumber write SetVertexNumber
      default 1;
    property Indexed: Boolean read FIndexed write SetIndexed default false;
  end;

implementation

uses
  GLStrings,
  GLState,
  VectorLists,
  GLSLog;

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xBaseSceneObject'}{$ENDIF}
// ------------------
// ------------------ TGL3xBaseSceneObject ------------------
// ------------------

destructor TGL3xBaseSceneObject.Destroy;
begin
  FMesh := nil;
  FMaterial := nil;
  inherited;
end;

procedure TGL3xBaseSceneObject.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if ARenderSelf then
  begin
    try
      if ocStructure in Changes then
        BuildMesh;
      if ARci.ignoreMaterials then
      begin
        BeforeRender;
        DrawManager.Draw(ARci, FMesh);
        AfterRender;
      end
      else
        repeat
          MaterialManager.ApplyMaterial(FMaterial, ARci);
          BeforeRender;
          DrawManager.Draw(ARci, FMesh);
          AfterRender;
        until MaterialManager.UnApplyMaterial(ARci);
    except
      Visible := False;
      if not ARci.GLStates.ForwardContext then
        ShaderManager.UseFixedFunctionPipeline;
    end;
  end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TGL3xBaseSceneObject.BeforeRender;
begin
end;

procedure TGL3xBaseSceneObject.AfterRender;
begin
end;

function TGL3xBaseSceneObject.GetMaterialName: string;
begin
  Result := FMaterial.GetValue;
end;

procedure TGL3xBaseSceneObject.SetMaterialName(const Value: string);
begin
  with MaterialManager do
    try
      BeginWork;
      FMaterial := GetMaterialName(Value);
    finally
      EndWork;
    end;
  NotifyChange(Self);
end;

constructor TGL3xBasePrimitiveObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Material := glsDEFAULTMATERIALNAME;
  with MeshManager do
    try
      BeginWork;
      FMesh := CreateMesh(Self.ClassName, TGL3xStaticMesh, '', '');
    finally
      EndWork;
    end;
  StructureChanged;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xPlane'}{$ENDIF}
// ------------------
// ------------------ TGL3xPlane ------------------
// ------------------

constructor TGL3xPlane.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  FStyle := [psSingleQuad, psTileTexture];
end;

// BuildList
//

procedure TGL3xPlane.BuildMesh;
var
  hw, hh, posXFact, posYFact, pX, pY0, pY1: TGLFloat;
  tx0, tx1, ty0, ty1, texSFact, texTFact: TGLFloat;
  texS, texT0, texT1: TGLFloat;
  x, y: Integer;
  Builder: TGL3xStaticMeshBuilder;
begin
  hw := FWidth * 0.5;
  hh := FHeight * 0.5;
  // determine tex coords extents
  if psTileTexture in FStyle then
  begin
    tx0 := FXOffset;
    tx1 := FXTiles * FXScope + FXOffset;
    ty0 := FYOffset;
    ty1 := FYTiles * FYScope + FYOffset;
  end
  else
  begin
    tx0 := 0;
    ty0 := tx0;
    tx1 := FXScope;
    ty1 := FYScope;
  end;
  // single quad plane
  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTangent, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        if psSingleQuad in FStyle then
        begin
          BeginBatch(mpTRIANGLE_STRIP);
          Attribute3f(attrNormal, 0, 0, 1);
          Attribute3f(attrTangent, 1, 0, 0);

          Attribute2f(attrTexCoord0, tx1, ty1);
          Attribute3f(attrPosition, hw, hh, 0);
          EmitVertex;
          Attribute2f(attrTexCoord0, tx0, ty1);
          Attribute3f(attrPosition, -hw, hh, 0);
          EmitVertex;
          Attribute2f(attrTexCoord0, tx1, ty0);
          Attribute3f(attrPosition, hw, -hh, 0);
          EmitVertex;
          Attribute2f(attrTexCoord0, tx0, ty0);
          Attribute3f(attrPosition, -hw, -hh, 0);
          EmitVertex;
          EndBatch;
        end
        else
        begin
          // multi-quad plane (actually built from tri-strips)
          texSFact := (tx1 - tx0) / FXTiles;
          texTFact := (ty1 - ty0) / FYTiles;
          posXFact := FWidth / FXTiles;
          posYFact := FHeight / FYTiles;
          texT0 := 0;
          pY0 := -hh;
          BeginBatch(mpTRIANGLE_STRIP);
          Attribute3f(attrNormal, 0, 0, 1);
          Attribute3f(attrTangent, 1, 0, 0);

          for y := 0 to FYTiles - 1 do
          begin
            texT1 := (y + 1) * texTFact;
            pY1 := (y + 1) * posYFact - hh;
            for x := 0 to FXTiles do
            begin
              texS := tx0 + x * texSFact;
              pX := x * posXFact - hw;
              Attribute2f(attrTexCoord0, texS, texT1);
              Attribute3f(attrPosition, pX, pY1, 0);
              EmitVertex;
              Attribute2f(attrTexCoord0, texS, texT0);
              Attribute3f(attrPosition, pX, pY0, 0);
              EmitVertex;
            end;
            RestartStrip;
            texT0 := texT1;
            pY0 := pY1;
          end;
          EndBatch;
        end;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// Assign
//

procedure TGL3xPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGL3xPlane) then
  begin
    FWidth := TGL3xPlane(Source).FWidth;
    FHeight := TGL3xPlane(Source).FHeight;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGL3xPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := 0.5 * Abs(FWidth);
  Result[1] := 0.5 * Abs(FHeight);
  Result[2] := 0;
end;

// SetWidth
//

procedure TGL3xPlane.SetWidth(const aValue: Single);
begin

  if aValue <> FWidth then
  begin
    FWidth := aValue;
    StructureChanged;
  end;
end;

// ScreenRect
//

function TGL3xPlane.ScreenRect(aBuffer: TGLSceneBuffer): TGLRect;
var
  v: array[0..3] of TVector;
  buf: TGLSceneBuffer;
  hw, hh: TGLFloat;
begin
  buf := aBuffer;
  if Assigned(buf) then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    v[0] := LocalToAbsolute(PointMake(-hw, -hh, 0));
    v[1] := LocalToAbsolute(PointMake(hw, -hh, 0));
    v[2] := LocalToAbsolute(PointMake(hw, hh, 0));
    v[3] := LocalToAbsolute(PointMake(-hw, hh, 0));
    buf.WorldToScreen(@v[0], 4);
    Result.Left := Round(MinFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
    Result.Right := Round(MaxFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
    Result.Top := Round(MinFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
    Result.Bottom := Round(MaxFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
  end
  else
    FillChar(Result, SizeOf(TGLRect), 0);
end;

// PointDistance
//

function TGL3xPlane.PointDistance(const aPoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
    AbsoluteDirection);
end;

// SetHeight
//

procedure TGL3xPlane.SetHeight(const aValue: Single);
begin

  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

// SetXOffset
//

procedure TGL3xPlane.SetXOffset(const Value: TGLFloat);
begin

  if Value <> FXOffset then
  begin
    FXOffset := Value;
    StructureChanged;
  end;
end;

// SetXScope
//

procedure TGL3xPlane.SetXScope(const Value: TGLFloat);
begin

  if Value <> FXScope then
  begin
    FXScope := Value;
    if FXScope > 1 then
      FXScope := 1;
    StructureChanged;
  end;
end;

// StoreXScope
//

function TGL3xPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

// SetXTiles
//

procedure TGL3xPlane.SetXTiles(const Value: Cardinal);
begin

  if Value <> FXTiles then
  begin
    FXTiles := Value;
    StructureChanged;
  end;
end;

// SetYOffset
//

procedure TGL3xPlane.SetYOffset(const Value: TGLFloat);
begin

  if Value <> FYOffset then
  begin
    FYOffset := Value;
    StructureChanged;
  end;
end;

// SetYScope
//

procedure TGL3xPlane.SetYScope(const Value: TGLFloat);
begin

  if Value <> FYScope then
  begin
    FYScope := Value;
    if FYScope > 1 then
      FYScope := 1;
    StructureChanged;
  end;
end;

// StoreYScope
//

function TGL3xPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

// SetYTiles
//

procedure TGL3xPlane.SetYTiles(const Value: Cardinal);
begin

  if Value <> FYTiles then
  begin
    FYTiles := Value;
    StructureChanged;
  end;
end;

// SetStyle
//

procedure TGL3xPlane.SetStyle(const val: TPlaneStyles);
begin

  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xSprite'}{$ENDIF}
// ------------------
// ------------------ TGL3xSprite ------------------
// ------------------

// Create
//

constructor TGL3xSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osNoVisibilityCulling];
  FWidth := 1;
  FHeight := 1;
  FAlign := alSpherical;
end;

// Assign
//

procedure TGL3xSprite.Assign(Source: TPersistent);
begin
  if Source is TGL3xSprite then
  begin
    FWidth := TGL3xSprite(Source).FWidth;
    FHeight := TGL3xSprite(Source).FHeight;
    FRotation := TGL3xSprite(Source).FRotation;
  end;
  inherited Assign(Source);
end;

function TGL3xSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := 0.5 * Abs(FWidth);
  Result[1] := 0.5 * Abs(FHeight);
  Result[2] := 0.5 * Abs(FWidth);
end;

procedure TGL3xSprite.DoRender(var ARci: TRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  VM, M: TMatrix;
begin
  if ARenderSelf then
  begin
    try
      if ocStructure in Changes then
        BuildMesh;
      VM := ARci.PipelineTransformation.ViewMatrix;

      case FAlign of
        alSpherical:
          begin
            M := ARci.PipelineTransformation.ModelViewMatrix;
            M[0, 0] := Scale.X;
            M[0, 1] := 0;
            M[0, 2] := 0;
            M[1, 0] := 0;
            M[1, 1] := Scale.Y;
            M[1, 2] := 0;
            M[2, 0] := 0;
            M[2, 1] := 0;
            M[2, 2] := 1;
            ARci.PipelineTransformation.ViewMatrix := M;
          end;
        alCylindrical:
          begin
            M := ARci.PipelineTransformation.ModelViewMatrix;
            M[0, 0] := 1;
            M[0, 1] := 0;
            M[0, 2] := 0;
            M[2, 0] := 0;
            M[2, 1] := 0;
            M[2, 2] := 1;
            ARci.PipelineTransformation.ViewMatrix := M;
          end;
      end;
      ARci.PipelineTransformation.ModelMatrix := IdentityHmgMatrix;

      if ARci.ignoreMaterials then
      begin
        BeforeRender;
        DrawManager.Draw(ARci, FMesh);
        AfterRender;
      end
      else
        repeat
          MaterialManager.ApplyMaterial(FMaterial, ARci);
          BeforeRender;
          DrawManager.Draw(ARci, FMesh);
          AfterRender;
        until MaterialManager.UnApplyMaterial(ARci);
    except
      Visible := False;
      if not ARci.GLStates.ForwardContext then
        ShaderManager.UseFixedFunctionPipeline;
    end;
    ARci.PipelineTransformation.ViewMatrix := VM;
  end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

// BuildList
//

procedure TGL3xSprite.BuildMesh;
var
  x, y: TAffineVector;
  u0, v0, u1, v1: Integer;
  RM: TMatrix;
  Builder: TGL3xStaticMeshBuilder;
begin
  x := AffineVectorMake(FWidth * 0.5, 0, 0);
  y := AffineVectorMake(0, FHeight * 0.5, 0);
  if FMirrorU then
  begin
    u0 := 1;
    u1 := 0;
  end
  else
  begin
    u0 := 0;
    u1 := 1;
  end;
  if FMirrorV then
  begin
    v0 := 1;
    v1 := 0;
  end
  else
  begin
    v0 := 0;
    v1 := 1;
  end;

  if FRotation <> 0 then
  begin
    RM := CreateRotationMatrix(ZVector, FRotation);
    x := VectorTransform(x, RM);
    y := VectorTransform(y, RM);
  end;

  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTangent, GLSLType3f);
        BeginBatch(mpTRIANGLE_STRIP);
        Attribute3f(attrNormal, 0, 0, 1);
        Attribute3f(attrTangent, VectorNormalize(x));
        Attribute2f(attrTexCoord0, u1, v1);
        Attribute3f(attrPosition, x[0] + y[0], x[1] + y[1], x[2] + y[2]);
        EmitVertex;
        Attribute2f(attrTexCoord0, u0, v1);
        Attribute3f(attrPosition, -x[0] + y[0], -x[1] + y[1], -x[2] + y[2]);
        EmitVertex;
        Attribute2f(attrTexCoord0, u1, v0);
        Attribute3f(attrPosition, x[0] - y[0], x[1] - y[1], x[2] - y[2]);
        EmitVertex;
        Attribute2f(attrTexCoord0, u0, v0);
        Attribute3f(attrPosition, -x[0] - y[0], -x[1] - y[1], -x[2] - y[2]);
        EmitVertex;
        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// SetWidth
//

procedure TGL3xSprite.SetWidth(const val: TGLFloat);
begin

  if FWidth <> val then
  begin
    FWidth := val;
    StructureChanged;
  end;
end;

// SetHeight
//

procedure TGL3xSprite.SetHeight(const val: TGLFloat);
begin

  if FHeight <> val then
  begin
    FHeight := val;
    StructureChanged;
  end;
end;

// SetRotation
//

procedure TGL3xSprite.SetRotation(const val: TGLFloat);
begin
  if FRotation <> val then
  begin
    FRotation := val;
    StructureChanged;
  end;
end;

procedure TGL3xSprite.SetMirrorU(const val: Boolean);
begin
  if FMirrorU <> val then
  begin
    FMirrorU := val;
    StructureChanged;
  end;
end;

// SetMirrorV
//

procedure TGL3xSprite.SetMirrorV(const val: Boolean);
begin
  if FMirrorV <> val then
  begin
    FMirrorV := val;
    StructureChanged;
  end;
end;

// SetSize
//

procedure TGL3xSprite.SetSize(const width, height: TGLFloat);
begin
  FWidth := width;
  FHeight := height;
  StructureChanged;
end;

// SetSquareSize
//

procedure TGL3xSprite.SetSquareSize(const size: TGLFloat);
begin
  FWidth := size;
  FHeight := size;
  StructureChanged;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xCube'}{$ENDIF}
// ------------------
// ------------------ TGL3xCube ------------------
// ------------------

// Create
//

constructor TGL3xCube.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FCubeSize := XYZVector;
  FParts := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
end;

// BuildList
//

procedure TGL3xCube.BuildMesh;
var
  hw, hh, hd, nd, tan: TGLFloat;
  Builder: TGL3xStaticMeshBuilder;
begin
  if FNormalDirection = ndInside then
    nd := -1
  else
    nd := 1;
  tan := nd;
  hw := FCubeSize[0] * 0.5;
  hh := FCubeSize[1] * 0.5;
  hd := FCubeSize[2] * 0.5;
  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTangent, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        DeclareAttribute(attrVertexColor, GLSLType4f);
        BeginBatch(mpTRIANGLES);
        if cpFront in FParts then
        begin
          Attribute3f(attrNormal, 0, 0, nd);
          Attribute3f(attrTangent, tan, 0, 0);
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, hw, hh, hd);
          Attribute4f(attrVertexColor, 1, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, -hw * nd, hh * nd, hd);
          Attribute4f(attrVertexColor, 0, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, -hw, -hh, hd);
          Attribute4f(attrVertexColor, 0, 0, 1, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, hw * nd, -hh * nd, hd);
          Attribute4f(attrVertexColor, 1, 0, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, hw, hh, hd);
          Attribute4f(attrVertexColor, 1, 1, 1, 1);
          EmitVertex;
        end;
        if cpBack in FParts then
        begin
          Attribute3f(attrNormal, 0, 0, -nd);
          Attribute3f(attrTangent, -tan, 0, 0);
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, hw, hh, -hd);
          Attribute4f(attrVertexColor, 1, 1, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, hw * nd, -hh * nd, -hd);
          Attribute4f(attrVertexColor, 1, 0, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, -hw, -hh, -hd);
          Attribute4f(attrVertexColor, 0, 0, 0, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, -hw * nd, hh * nd, -hd);
          Attribute4f(attrVertexColor, 0, 1, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, hw, hh, -hd);
          Attribute4f(attrVertexColor, 1, 1, 0, 1);
          EmitVertex;
        end;
        if cpLeft in FParts then
        begin
          Attribute3f(attrNormal, -nd, 0, 0);
          Attribute3f(attrTangent, 0, 0, tan);
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, -hw, hh, hd);
          Attribute4f(attrVertexColor, 0, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, -hw, hh * nd, -hd * nd);
          Attribute4f(attrVertexColor, 0, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, -hw, -hh, -hd);
          Attribute4f(attrVertexColor, 0, 0, 0, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, -hw, -hh * nd, hd * nd);
          Attribute4f(attrVertexColor, 0, 0, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, -hw, hh, hd);
          Attribute4f(attrVertexColor, 0, 1, 1, 1);
          EmitVertex;
        end;
        if cpRight in FParts then
        begin
          Attribute3f(attrNormal, nd, 0, 0);
          Attribute3f(attrTangent, 0, 0, -tan);
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, hw, hh, hd);
          Attribute4f(attrVertexColor, 1, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, hw, -hh * nd, hd * nd);
          Attribute4f(attrVertexColor, 1, 0, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, hw, -hh, -hd);
          Attribute4f(attrVertexColor, 1, 0, 0, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, hw, hh * nd, -hd * nd);
          Attribute4f(attrVertexColor, 1, 1, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, hw, hh, hd);
          Attribute4f(attrVertexColor, 1, 1, 1, 1);
          EmitVertex;
        end;
        if cpTop in FParts then
        begin
          Attribute3f(attrNormal, 0, nd, 0);
          Attribute3f(attrTangent, tan, 0, 0);
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, -hw, hh, -hd);
          Attribute4f(attrVertexColor, 0, 1, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, -hw * nd, hh, hd * nd);
          Attribute4f(attrVertexColor, 0, 1, 1, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, hw, hh, hd);
          Attribute4f(attrVertexColor, 1, 1, 1, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, hw * nd, hh, -hd * nd);
          Attribute4f(attrVertexColor, 1, 1, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, -hw, hh, -hd);
          Attribute4f(attrVertexColor, 0, 1, 0, 1);
          EmitVertex;
        end;
        if cpBottom in FParts then
        begin
          Attribute3f(attrNormal, 0, -nd, 0);
          Attribute3f(attrTangent, -tan, 0, 0);
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, -hw, -hh, -hd);
          Attribute4f(attrVertexColor, 0, 0, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 0);
          Attribute3f(attrPosition, hw * nd, -hh, -hd * nd);
          Attribute4f(attrVertexColor, 1, 0, 0, 1);
          EmitVertex;
          Attribute2f(attrTexCoord0, 1, 1);
          Attribute3f(attrPosition, hw, -hh, hd);
          Attribute4f(attrVertexColor, 1, 0, 1, 1);
          EmitVertex;
          EmitVertex;
          Attribute2f(attrTexCoord0, 0, 1);
          Attribute3f(attrPosition, -hw * nd, -hh, hd * nd);
          Attribute4f(attrVertexColor, 0, 0, 1, 1);
          EmitVertex;
          Attribute3f(attrNormal, 0, -nd, 0);
          Attribute2f(attrTexCoord0, 0, 0);
          Attribute3f(attrPosition, -hw, -hh, -hd);
          Attribute4f(attrVertexColor, 0, 0, 0, 1);
          EmitVertex;
        end;
        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// GenerateSilhouette
//

function TGL3xCube.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  hw, hh, hd: TGLFloat;
  connectivity: TConnectivity;
  sil: TGLSilhouette;
begin
  Connectivity := TConnectivity.Create(true);

  hw := FCubeSize[0] * 0.5;
  hh := FCubeSize[1] * 0.5;
  hd := FCubeSize[2] * 0.5;

  if cpFront in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(hw, hh, hd),
      AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(-hw, -hh, hd),
      AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(hw, hh, -hd),
      AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(-hw, hh, -hd),
      AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, -hh, hd),
      AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(-hw, hh, -hd),
      AffineVectorMake(-hw, hh, hd),
      AffineVectorMake(hw, hh, hd),
      AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    Connectivity.AddQuad(
      AffineVectorMake(-hw, -hh, -hd),
      AffineVectorMake(hw, -hh, -hd),
      AffineVectorMake(hw, -hh, hd),
      AffineVectorMake(-hw, -hh, hd));
  end;

  sil := nil;
  Connectivity.CreateSilhouette(
    silhouetteParameters, sil, false);

  result := sil;

  Connectivity.Free;
end;

// SetCubeWidth
//

procedure TGL3xCube.SetCubeWidth(const aValue: Single);
begin

  if aValue <> FCubeSize[0] then
  begin
    FCubeSize[0] := aValue;
    StructureChanged;
  end;
end;

// SetCubeHeight
//

procedure TGL3xCube.SetCubeHeight(const aValue: Single);
begin

  if aValue <> FCubeSize[1] then
  begin
    FCubeSize[1] := aValue;
    StructureChanged;
  end;
end;

// SetCubeDepth
//

procedure TGL3xCube.SetCubeDepth(const aValue: Single);
begin

  if aValue <> FCubeSize[2] then
  begin
    FCubeSize[2] := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TGL3xCube.SetParts(aValue: TCubeParts);
begin

  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TGL3xCube.SetNormalDirection(aValue: TNormalDirection);
begin

  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGL3xCube.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TGL3xCube) then
  begin
    FCubeSize := TGL3xCube(Source).FCubeSize;
    FParts := TGL3xCube(Source).FParts;
    FNormalDirection := TGL3xCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGL3xCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := FCubeSize[0] * 0.5;
  Result[1] := FCubeSize[1] * 0.5;
  Result[2] := FCubeSize[2] * 0.5;
  Result[3] := 0;
end;

// RayCastIntersect
//

function TGL3xCube.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  p: array[0..5] of TVector;
  rv: TVector;
  rs, r: TVector;
  i: Integer;
  t, e: Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  e := 0.5 + 0.0001; //Small value for floating point imprecisions
  eSize[0] := FCubeSize[0] * e;
  eSize[1] := FCubeSize[1] * e;
  eSize[2] := FCubeSize[2] * e;
  p[0] := XHmgVector;
  p[1] := YHmgVector;
  p[2] := ZHmgVector;
  SetVector(p[3], -1, 0, 0);
  SetVector(p[4], 0, -1, 0);
  SetVector(p[5], 0, 0, -1);
  for i := 0 to 5 do
  begin
    if VectorDotProduct(p[i], rv) > 0 then
    begin
      t := -(p[i][0] * rs[0] + p[i][1] * rs[1] + p[i][2] * rs[2] + 0.5 *
        FCubeSize[i mod 3])
        / (p[i][0] * rv[0] + p[i][1] * rv[1] + p[i][2] * rv[2]);
      MakePoint(r, rs[0] + t * rv[0], rs[1] + t * rv[1], rs[2] + t * rv[2]);
      if (Abs(r[0]) <= eSize[0])
        and (Abs(r[1]) <= eSize[1])
        and (Abs(r[2]) <= eSize[2])
        and (VectorDotProduct(VectorSubtract(r, rs), rv) > 0) then
      begin
        if Assigned(intersectPoint) then
          MakePoint(intersectPoint^, LocalToAbsolute(r));
        if Assigned(intersectNormal) then
          MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

// DefineProperties
//

procedure TGL3xCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
    (FCubeSize[0] <> 1) or (FCubeSize[1] <> 1) or (FCubeSize[2] <> 1));
end;

// ReadData
//

procedure TGL3xCube.ReadData(Stream: TStream);
begin
  Stream.Read(FCubeSize, SizeOf(TAffineVector));
end;

// WriteData
//

procedure TGL3xCube.WriteData(Stream: TStream);
begin
  Stream.Write(FCubeSize, SizeOf(TAffineVector));
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xSphere'}{$ENDIF}
// ------------------
// ------------------ TGL3xSphere ------------------
// ------------------

// Create
//

constructor TGL3xSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FSlices := 16;
  FStacks := 16;
  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
end;

// BuildList
//

procedure TGL3xSphere.BuildMesh;
var
  V1, V2, N1, T1: TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  I, J: Integer;
  NeedEnd: Boolean;
  Builder: TGL3xStaticMeshBuilder;
begin
  // common settings
  AngTop := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart := DegToRad(1.0 * FStart);
  AngStop := DegToRad(1.0 * FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTangent, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        // top cap
        NeedEnd := false;
        if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
        begin
          BeginBatch(mpTRIANGLE_FAN);
          SinCos(AngTop, SinP, CosP);
          Attribute2f(attrTexCoord0, 0.5, 0.5);
          Attribute3f(attrNormal, 0, 1, 0);
          Attribute3f(attrTangent, 1, 0, 0);
          if FTopCap = ctCenter then
            Attribute3f(attrPosition, 0, 0, 0)
          else
          begin
            Attribute3f(attrPosition, 0, SinP * Radius, 0);
            N1 := YVector;
            T1 := XVector;
          end;
          EmitVertex;
          Theta := AngStart;
          for I := 0 to FSlices do
          begin
            SinCos(Theta, SinT, CosT);
            V1[0] := CosP * SinT;
            V1[1] := SinP;
            V1[2] := CosP * CosT;
            if FTopCap = ctCenter then
            begin
              N1 := VectorPerpendicular(YVector, V1);
              T1 := VectorCrossProduct(N1, YVector);
            end;
            Attribute2f(attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
            Attribute3f(attrNormal, N1);
            Attribute3f(attrTangent, T1);
            ScaleVector(V1, Radius);
            Attribute3f(attrPosition, V1);
            EmitVertex;
            Theta := Theta + StepH;
          end;
          RestartStrip;
          NeedEnd := true;
        end;
        // bottom cap
        if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then
        begin
          if not NeedEnd then
            BeginBatch(mpTRIANGLE_FAN);
          SinCos(AngBottom, SinP, CosP);
          Attribute2f(attrTexCoord0, 0.5, 0.5);
          Attribute3f(attrNormal, 0, -1, 0);
          Attribute3f(attrTangent, -1, 0, 0);
          if FBottomCap = ctCenter then
            Attribute3f(attrPosition, 0, 0, 0)
          else
          begin
            Attribute3f(attrPosition, 0, SinP * Radius, 0);
            N1 := YVector;
            T1 := XVector;
          end;
          EmitVertex;
          Theta := AngStop;
          for I := 0 to FSlices do
          begin
            SinCos(Theta, SinT, CosT);
            V1[0] := CosP * SinT;
            V1[1] := SinP;
            V1[2] := CosP * CosT;
            if FTopCap = ctCenter then
            begin
              N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
              T1 := VectorCrossProduct(N1, YVector);
            end;
            Attribute2f(attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
            Attribute3f(attrNormal, N1);
            Attribute3f(attrTangent, T1);
            ScaleVector(V1, Radius);
            Attribute3f(attrPosition, V1);
            EmitVertex;
            Theta := Theta - StepH;
          end;
          NeedEnd := true;
        end;
        if NeedEnd then
          EndBatch;

        // main body
        Phi := AngTop;
        Phi2 := Phi - StepV;

        uTexFactor := 1 / FSlices;
        vTexFactor := 1 / FStacks;

        BeginBatch(mpTRIANGLE_STRIP);
        for j := 0 to FStacks - 1 do
        begin
          Theta := AngStart;
          SinCos(Phi, SinP, CosP);
          SinCos(Phi2, SinP2, CosP2);

          vTexCoord0 := 1 - j * vTexFactor;
          vTexCoord1 := 1 - (j + 1) * vTexFactor;

          for i := 0 to FSlices do
          begin
            SinCos(Theta, SinT, CosT);
            V1[0] := CosP * SinT;
            V1[1] := SinP;
            V1[2] := CosP * CosT;

            V2[0] := CosP2 * SinT;
            V2[1] := SinP2;
            V2[2] := CosP2 * CosT;

            uTexCoord := i * uTexFactor;
            Attribute2f(attrTexCoord0, uTexCoord, vTexCoord0);
            Attribute3f(attrNormal, V1);
            T1 := VectorCrossProduct(V1, YVector);
            Attribute3f(attrTangent, T1);
            ScaleVector(V1, Radius);
            Attribute3f(attrPosition, V1);
            EmitVertex;

            Attribute2f(attrTexCoord0, uTexCoord, vTexCoord1);
            Attribute3f(attrNormal, V2);
            T1 := VectorCrossProduct(V2, YVector);
            Attribute3f(attrTangent, T1);
            ScaleVector(V2, Radius);
            Attribute3f(attrPosition, V2);
            EmitVertex;

            Theta := Theta + StepH;
          end;
          RestartStrip;
          Phi := Phi2;
          Phi2 := Phi2 - StepV;
        end;
        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// RayCastIntersect
//

function TGL3xSphere.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  i1, i2: TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1[3] := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//

function TGL3xSphere.GenerateSilhouette(const silhouetteParameters:
  TGLSilhouetteParameters): TGLSilhouette;
var
  i, j: Integer;
  s, c, angleFactor: Single;
  sVec, tVec: TAffineVector;
  Segments: integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1e-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i := 0 to Segments - 1 do
  begin
    SinCos(i * angleFactor, FRadius, s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    j := (i + 1) mod Segments;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

// SetBottom
//

procedure TGL3xSphere.SetBottom(aValue: TAngleLimit1);
begin

  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TGL3xSphere.SetBottomCap(aValue: TCapType);
begin

  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TGL3xSphere.SetRadius(const aValue: TGLFloat);
begin

  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGL3xSphere.SetSlices(aValue: Integer);
begin

  if aValue <> FSlices then
  begin
    if aValue <= 0 then
      FSlices := 1
    else
      FSlices := aValue;
    StructureChanged;
  end;
end;

// SetStacks
//

procedure TGL3xSphere.SetStacks(aValue: TGLInt);
begin

  if aValue <> FStacks then
  begin
    if aValue <= 0 then
      FStacks := 1
    else
      FStacks := aValue;
    StructureChanged;
  end;
end;

// SetStart
//

procedure TGL3xSphere.SetStart(aValue: TAngleLimit2);
begin

  if FStart <> aValue then
  begin
    Assert(aValue <= FStop);
    FStart := aValue;
    StructureChanged;
  end;
end;

// SetStop
//

procedure TGL3xSphere.SetStop(aValue: TAngleLimit2);
begin

  if FStop <> aValue then
  begin
    Assert(aValue >= FStart);
    FStop := aValue;
    StructureChanged;
  end;
end;

// SetTop
//

procedure TGL3xSphere.SetTop(aValue: TAngleLimit1);
begin

  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TGL3xSphere.SetTopCap(aValue: TCapType);
begin

  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGL3xSphere.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TGL3xSphere) then
  begin
    FRadius := TGL3xSphere(Source).FRadius;
    FSlices := TGL3xSphere(Source).FSlices;
    FStacks := TGL3xSphere(Source).FStacks;
    FBottom := TGL3xSphere(Source).FBottom;
    FTop := TGL3xSphere(Source).FTop;
    FStart := TGL3xSphere(Source).FStart;
    FStop := TGL3xSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGL3xSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := Abs(FRadius);
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := 0;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xGeoSphere'}{$ENDIF}
// ------------------
// ------------------ TGL3xGeoSphere ------------------
// ------------------
// Create
//

constructor TGL3xGeoSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FLevel := 1;
end;

// BuildList
//

procedure TGL3xGeoSphere.BuildMesh;
var
  pivot, dir1, dir2, V, B: TAffineVector;
  level, numLayers: Integer;
  dt, t_top, t_bot, ds_top, ds_bot, s_top, s_bot,
    smax_top, smax_bot: Single;
  offset: Boolean;
  Builder: TGL3xStaticMeshBuilder;

  procedure ProduceVertex(s, t, sb, tb: Single; uprow: Boolean);
  var
    XZR: Single;
  begin
    V[0] := pivot[0] + s * dir1[0] + t * dir2[0];
    V[1] := pivot[1] + s * dir1[1] + t * dir2[1];
    V[2] := pivot[2] + s * dir1[2] + t * dir2[2];
    B[0] := pivot[0] + sb * dir1[0] + tb * dir2[0];
    B[1] := pivot[1] + sb * dir1[1] + tb * dir2[1];
    B[2] := pivot[2] + sb * dir1[2] + tb * dir2[2];
    NormalizeVector(V);
    NormalizeVector(B);

    if uprow then
    begin
      // Poles
      XZR := sqrt(B[0] * B[0] + B[2] * B[2]);
      s := V[0];
      t := V[2];
      if XZR > 0 then
      begin
        s := s / XZR;
        t := t / XZR;
      end;
      s := 0.5 + (1 / 6) * s;
      t := (1 / 6) * (t + 1);
      if V[1] > 0 then
        t := 1 - t;
    end
    else
    begin
      // Equator
      s := 0.5 + 0.5 * arctan2(V[0], V[2]) / Pi;
      if offset and (s > 0.99) then
        s := 0;

      t := 0.5;
      if B[1] <> 0 then
        t := t + (1 / 6) * V[1] / Abs(B[1]);
    end;

    with Builder do
    begin
      Attribute2f(attrTexCoord0, s, t);
      Attribute3f(attrNormal, V[0], V[1], V[2]);
      ScaleVector(V, FRadius);
      Attribute3f(attrPosition, V[0], V[1], V[2]);
      EmitVertex;
    end;
  end;

  procedure ProduceOctant;
  var
    i, j, caps: Integer;
    t_border, smax_border: Single;
  begin
    t_top := 1;
    ds_top := 0;
    caps := numLayers div 2;

    t_border := 1 - caps * dt;
    smax_border := 1 - t_border;

    for i := 0 to numLayers - 1 do
    begin
      t_bot := t_top - dt;
      smax_top := 1 - t_top;
      smax_bot := 1 - t_bot;
      if i > 0 then
        ds_top := smax_top / i;
      ds_bot := smax_bot / (i + 1);
      s_top := 0;
      s_bot := 0;
      for j := 0 to i do
      begin
        ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i
          < caps);
        ProduceVertex(s_top, t_top, s_top * smax_border / smax_top, t_border, i
          < caps);
        s_top := s_top + ds_top;
        s_bot := s_bot + ds_bot;
      end;
      ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i <
        caps);
      Builder.RestartStrip;
      t_top := t_bot;
    end;
  end;

begin
  level := FLevel;
  numLayers := 1 shl level;
  dt := 1 / numLayers;

  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        BeginBatch(mpTRIANGLE_STRIP);

        offset := false;
        SetVector(pivot, 1, 0, 0);
        SetVector(dir1, -1, 0, 1);
        SetVector(dir2, -1, 1, 0);
        ProduceOctant;

        SetVector(pivot, 0, 0, 1);
        SetVector(dir1, -1, 0, -1);
        SetVector(dir2, 0, 1, -1);
        ProduceOctant;

        SetVector(pivot, 0, 0, -1);
        SetVector(dir1, 1, 0, 1);
        SetVector(dir2, 0, 1, 1);
        ProduceOctant;

        SetVector(pivot, -1, 0, 0);
        SetVector(dir1, 1, 0, 1);
        SetVector(dir2, 1, -1, 0);
        ProduceOctant;

        SetVector(pivot, 0, 0, 1);
        SetVector(dir1, 1, 0, -1);
        SetVector(dir2, 0, -1, -1);
        ProduceOctant;

        SetVector(pivot, 1, 0, 0);
        SetVector(dir1, -1, 0, -1);
        SetVector(dir2, -1, -1, 0);
        ProduceOctant;

        offset := true;
        SetVector(pivot, -1, 0, 0);
        SetVector(dir1, 1, 0, -1);
        SetVector(dir2, 1, 1, 0);
        ProduceOctant;

        SetVector(pivot, 0, 0, -1);
        SetVector(dir1, -1, 0, 1);
        SetVector(dir2, 0, -1, 1);
        ProduceOctant;

        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// Assign
//

procedure TGL3xGeoSphere.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TGL3xGeoSphere) then
  begin
    FRadius := TGL3xGeoSphere(Source).FRadius;
    FLevel := TGL3xGeoSphere(Source).FLevel;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGL3xGeoSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result[0] := Abs(FRadius);
  Result[1] := Result[0];
  Result[2] := Result[0];
  Result[3] := 0;
end;

// RayCastIntersect
//

function TGL3xGeoSphere.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  i1, i2: TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1,
    i2) > 0 then
  begin
    Result := True;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1[3] := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//

function TGL3xGeoSphere.GenerateSilhouette(const silhouetteParameters:
  TGLSilhouetteParameters): TGLSilhouette;
var
  i, j: Integer;
  s, c, angleFactor: Single;
  sVec, tVec: TAffineVector;
begin

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1e-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result := TGLSilhouette.Create;
  angleFactor := (2 * PI) / FLevel;
  for i := 0 to FLevel - 1 do
  begin
    SinCos(i * angleFactor, FRadius, s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    j := (i + 1) mod FLevel;
    Result.Indices.Add(i, j);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(FLevel, i, j)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

// SetRadius
//

procedure TGL3xGeoSphere.SetRadius(const Value: TGLFloat);
begin

  if Value <> FRadius then
  begin
    FRadius := Value;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGL3xGeoSphere.SetSubdivisionLevel(Value: Integer);
begin

  if Value <> FLevel then
  begin
    if Value < 0 then
      Value := 0;
    if Value > 4 then
      Value := 4;
    FLevel := Value;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xDisk'}{$ENDIF}
// ------------------
// ------------------ TGL3xDisk ------------------
// ------------------

// Create
//

constructor TGL3xDisk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius := 0.5;
  FInnerRadius := 0;
  FSlices := 16;
  FLoops := 2;
  FStartAngle := 0;
  FSweepAngle := 360;
end;

// BuildList
//

procedure TGL3xDisk.BuildMesh;
var
  i, j: Integer;
  Astart, Astep, angle, Rstep, R: Single;
  s, c: Single;
  Builder: TGL3xStaticMeshBuilder;
begin
  Astart := DegToRad(FStartAngle);
  Astep := DegToRad(FSweepAngle) / FSlices;
  Rstep := (FOuterRadius - FInnerRadius) / FLoops;
  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        DeclareAttribute(attrPosition, GLSLType3f);
        DeclareAttribute(attrNormal, GLSLType3f);
        DeclareAttribute(attrTangent, GLSLType3f);
        DeclareAttribute(attrTexCoord0, GLSLType2f);
        BeginBatch(mpTRIANGLE_STRIP);
        Attribute3f(attrNormal, 0, 0, 1);
        Attribute3f(attrTangent, 1, 0, 0);
        for j := 0 to FLoops - 1 do
        begin
          angle := Astart;
          for i := 0 to FSlices do
          begin
            SinCos(angle, s, c);
            Attribute2f(attrTexCoord0, i / FSlices, j / FLoops);
            R := FInnerRadius + j * Rstep;
            Attribute3f(attrPosition, c * R, s * R, 0);
            EmitVertex;
            Attribute2f(attrTexCoord0, i / FSlices, (j + 1) / FLoops);
            R := FInnerRadius + (j + 1) * Rstep;
            Attribute3f(attrPosition, c * R, s * R, 0);
            EmitVertex;
            angle := angle + Astep;
          end;
        end;
        EndBatch;
        WeldVertices;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
  ClearStructureChanged;
end;

// SetOuterRadius
//

procedure TGL3xDisk.SetOuterRadius(const aValue: Single);
begin

  if aValue <> FOuterRadius then
  begin
    FOuterRadius := aValue;
    StructureChanged;
  end;
end;

// SetInnerRadius
//

procedure TGL3xDisk.SetInnerRadius(const aValue: Single);
begin

  if aValue <> FInnerRadius then
  begin
    FInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TGL3xDisk.SetSlices(aValue: Integer);
begin

  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetLoops
//

procedure TGL3xDisk.SetLoops(aValue: Integer);
begin

  if aValue <> FLoops then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

// SetStartAngle
//

procedure TGL3xDisk.SetStartAngle(const aValue: Single);
begin

  if aValue <> FStartAngle then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

// SetSweepAngle
//

procedure TGL3xDisk.SetSweepAngle(const aValue: Single);
begin

  if aValue <> FSweepAngle then
  begin
    FSweepAngle := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TGL3xDisk.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TGL3xDisk) then
  begin
    FOuterRadius := TGL3xDisk(Source).FOuterRadius;
    FInnerRadius := TGL3xDisk(Source).FInnerRadius;
    FSlices := TGL3xDisk(Source).FSlices;
    FLoops := TGL3xDisk(Source).FLoops;
    FStartAngle := TGL3xDisk(Source).FStartAngle;
    FSweepAngle := TGL3xDisk(Source).FSweepAngle;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TGL3xDisk.AxisAlignedDimensionsUnscaled: TVector;
var
  r: TGLFloat;
begin
  r := Abs(FOuterRadius);
  Result := VectorMake(r, r, 0);
end;

// RayCastIntersect
//

function TGL3xDisk.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  ip: TVector;
  d: Single;
  angle, beginAngle, endAngle: Single;
  localIntPoint: TVector;
begin
  Result := false;
  if SweepAngle > 0 then
    if RayCastPlaneIntersect(rayStart, rayVector, AbsolutePosition,
      AbsoluteDirection, @ip) then
    begin
      if Assigned(intersectPoint) then
        SetVector(intersectPoint^, ip);
      localIntPoint := AbsoluteToLocal(ip);
      d := VectorNorm(localIntPoint);
      if (d >= Sqr(InnerRadius)) and (d <= Sqr(OuterRadius)) then
      begin
        if SweepAngle >= 360 then
          Result := true
        else
        begin
          //arctan2 returns results between -pi and +pi, we want between 0 and 360
          angle := 180 / pi * arctan2(localIntPoint[0], localIntPoint[1]);
          if angle < 0 then
            angle := angle + 360;
          //we also want StartAngle and StartAngle+SweepAngle to be in this range
          beginAngle := Trunc(StartAngle) mod 360;
          endAngle := Trunc(StartAngle + SweepAngle) mod 360;
          //If beginAngle>endAngle then area crosses the boundary from 360=>0 degrees
          //therefore have 2 valid regions  (beginAngle to 360) & (0 to endAngle)
          //otherwise just 1 valid region (beginAngle to endAngle)
          if beginAngle > endAngle then
          begin
            if (angle > beginAngle) or (angle < endAngle) then
              Result := True;
          end
          else if (angle > beginAngle) and (angle < endAngle) then
            Result := True;
        end;
      end;
    end;
  if Result = true then
    if Assigned(intersectNormal) then
      SetVector(intersectNormal^, AbsoluteUp);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'TGL3xFeedBackMesh'}{$ENDIF}
// ------------------
// ------------------ TGL3xFeedBackMesh ------------------
// ------------------

// Create
//

constructor TGL3xFeedBackMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrimitiveType := bmpPoint;
  FVertexNumber := 1;
  FIndexed := False;
  FAttrIsDefined := False;
end;

procedure TGL3xFeedBackMesh.BeforeRender;
begin
  if not (csDesigning in ComponentState)
    and FAttrIsDefined
    and Assigned(FFactory) then
    FFactory.Produce(Self);
end;

procedure TGL3xFeedBackMesh.SetMaterialName(const Value: string);
begin
  if Length(Value) > 0 then
    inherited
  else
    FMaterial := nil;
  FAttrIsDefined := False;
  StructureChanged;
end;

function TGL3xFeedBackMesh.GetShader: IGLName;
begin
  if Assigned(FMaterial) then
    try
      MaterialManager.BeginWork;
      Result := MaterialManager.GetMaterialProgramName(FMaterial);
    finally
      MaterialManager.EndWork;
    end
  else
    Result := FShader;
end;

procedure TGL3xFeedBackMesh.SetShader(AName: IGLName);
begin
  SetMaterialName('');
  FShader := AName;
  FAttrIsDefined := False;
  StructureChanged;
end;

// BuildList
//

procedure TGL3xFeedBackMesh.BuildMesh;
const
  cPrimitives: array[TFeedBackMeshPrimitive] of TGLMeshPrimitive =
    (mpPOINTS, mpLINES, mpTRIANGLES);
var
  A: Integer;
  Builder: TGL3xStaticMeshBuilder;
begin
  if csDesigning in ComponentState then
    exit;

  if not FAttrIsDefined then
  begin
    with ShaderManager do
      try
        BeginWork;
        FAttrIsDefined := GetProgramAttribs(GetShader, FAttrArray);
      finally
        EndWork;
      end;

    if not FAttrIsDefined then
    begin
      Visible := False;
      GLSLogger.LogError(Format('Material of %s has not attributes', [Name]));
      exit;
    end;
  end;

  // Create empty graphic buffers
  with MeshManager do
    try
      BeginWork;
      Builder := TGL3xStaticMeshBuilder(GetMeshBuilder(FMesh));
      with Builder do
      begin
        BeginMeshAssembly;
        Clear;
        for A := 0 to GLS_VERTEX_ATTR_NUM - 1 do
          if Assigned(FAttrArray[A]) then
            DeclareAttribute(FAttrArray[A], FAttrArray[A].DataFormat);
        BeginBatch(cPrimitives[FPrimitiveType]);
        EmitVertices(FVertexNumber);
        EndBatch;
        Blank := True;
        EndMeshAssembly;
      end;
    finally
      EndWork;
    end;
end;

procedure TGL3xFeedBackMesh.Assign(Source: TPersistent);
begin
  if Source is TGL3xFeedBackMesh then
  begin
    SetFactory(TGL3xFeedBackMesh(Source).Factory);
  end;
  inherited;
end;

procedure TGL3xFeedBackMesh.SetFactory(Value: TGL3xBaseFactory);
begin
  if Value <> FFactory then
  begin
    if Assigned(FFactory) then
      FFactory.RemoveFreeNotification(Self);
    FFactory := Value;
    if Assigned(FFactory) then
      FFactory.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGL3xFeedBackMesh.SetPrimitiveType(Value: TFeedBackMeshPrimitive);
begin
  if Value <> FPrimitiveType then
  begin
    FPrimitiveType := Value;
    StructureChanged;
  end;
end;

procedure TGL3xFeedBackMesh.SetVertexNumber(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value > 1000000 then
    Value := 1000000;
  if Value <> FVertexNumber then
  begin
    FVertexNumber := Value;
    StructureChanged;
  end;
end;

procedure TGL3xFeedBackMesh.SetIndexed(Value: Boolean);
begin
  if Value <> FIndexed then
  begin
    FIndexed := Value;
    StructureChanged;
  end;
end;

procedure TGL3xFeedBackMesh.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FFactory) then
    FFactory := nil;
  inherited;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization

  RegisterClasses([TGL3xPlane, TGL3xSprite, TGL3xCube, TGL3xSphere,
    TGL3xGeoSphere, TGL3xDisk, TGL3xFeedBackMesh]);

end.

