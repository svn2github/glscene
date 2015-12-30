//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : GLObjects<p>

  Implementation of basic scene objects plus some management routines.<p>

  All objects declared in this unit are part of the basic GLScene package,
  these are only simple objects and should be kept simple and lightweight.<br>

  More complex or more specialized versions should be placed in dedicated
  units where they can grow and prosper untammed. "Generic" geometrical like SuperEllipsoid and pqTorus
  objects can be found in DGLGeomObjects.<p>

  You'll find specialized Meshes and Actors Objects in DGLMeshObject unit<p>

  <b>History : </b><font size=-1><ul>
  <li>24/12/15 - JD - imported and updated from GLSNewton project thanks to Yar
  </ul></font><p>

}
unit DGLObjects;

interface

{$I DGLEngine.inc}

uses
  Classes, SysUtils,
  // DGLE
  DGLCrossPlatform, dglOpenGL, DGLContext, DGLContextHandles, DGLRenderContextInfo,
  DGLBaseClasses, DGLTypes, DGLCoordinates, DGLVectorMaths, DGLVectorTypes,
  DGLScene,
  DGLSilhouette,
  DGLSLShader,
  DGLRenderManager,
  DGLColor,
 // DGLMaterial;
  DGLShader;

// DGLShadersManager,  DGLMaterial, DGLFactory;

type

  // TGLVisibilityDeterminationEvent
  //
  TGLVisibilityDeterminationEvent = function(Sender: TObject; var rci: TRenderContextInfo): Boolean of object;

  PVertexRec = ^TVertexRec;

  TVertexRec = record
    Position: TVector3f;
    Normal: TVector3f;
    Binormal: TVector3f;
    Tangent: TVector3f;
    TexCoord: TVector2f;
  end;

  // TDGLDummyCube
  //
  { : A simple cube, invisible at run-time.<p>
    This is a usually non-visible object -except at design-time- used for
    building hierarchies or groups, when some kind of joint or movement
    mechanism needs be described, you can use DummyCubes.<br>
    DummyCube's barycenter is its children's barycenter.<br>
    The DummyCube can optionnally amalgamate all its children into a single
    display list (see Amalgamate property). }
  TDGLDummyCube = class(TDGLCameraInvariantObject)
  private
    { Private Declarations }
    FCubeSize:                      TGLFloat;
    FEdgeColor:                     TDGLColor;
    FVisibleAtRunTime, FAmalgamate: Boolean;
    // FGroupList: TGLListHandle;
    FOnVisibilityDetermination: TGLVisibilityDeterminationEvent;

  protected
    { Protected Declarations }
    procedure SetCubeSize(const val: TGLFloat);
    procedure SetEdgeColor(const val: TDGLColor);
    procedure SetVisibleAtRunTime(const val: Boolean);
    procedure SetAmalgamate(const val: Boolean);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure DoRender(var rci: TRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    procedure StructureChanged; override;
    function BarycenterAbsolutePosition: TVector; override;

  published
    { Published Declarations }
    property CubeSize:  TGLFloat read FCubeSize write SetCubeSize;
    property EdgeColor: TDGLColor read FEdgeColor write SetEdgeColor;
    { : If true the dummycube's edges will be visible at runtime.<p>
      The default behaviour of the dummycube is to be visible at design-time
      only, and invisible at runtime. }
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;
    { : Amalgamate the dummy's children in a single OpenGL entity.<p>
      This activates a special rendering mode, which will compile
      the rendering of all of the dummycube's children objects into a
      single display list. This may provide a significant speed up in some
      situations, however, this means that changes to the children will
      be ignored untill you call StructureChanged on the dummy cube.<br>
      Some objects, that have their own display list management, may not
      be compatible with this behaviour. This will also prevents sorting
      and culling to operate as usual.<p>
      In short, this features is best used for static, non-transparent
      geometry, or when the point of view won't change over a large
      number of frames. }
    property Amalgamate: Boolean read FAmalgamate write SetAmalgamate default False;
    { : Camera Invariance Options.<p>
      These options allow to "deactivate" sensitivity to camera, f.i. by
      centering the object on the camera or ignoring camera orientation. }
    property CamInvarianceMode default cimNone;
    { : Event for custom visibility determination.<p>
      Event handler should return True if the dummycube and its children
      are to be considered visible for the current render. }
    property OnVisibilityDetermination: TGLVisibilityDeterminationEvent read FOnVisibilityDetermination write FOnVisibilityDetermination;
  end;


  // TGLAbstractBaseBufferedObject
  //

  TDGLAbstractBaseSceneObject = class(TDGLBaseSceneObject)
  protected
    { Protected Declarations }
    FBuiltProperties: TDGLBuiltProperties;
    FShaderLibrary: TDGLShaderLibrary;
    FShader:        TDGLLibShader;
    procedure SetBuiltProperties(const Value: TDGLBuiltProperties);
    procedure SetShader(const Value: TDGLLibShader);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure StructureChanged; override;
    procedure NotifyChange(Sender: TObject); override;
    property BuiltProperties: TDGLBuiltProperties read FBuiltProperties write SetBuiltProperties;
    property ShaderLibrary: TDGLShaderLibrary read FShaderLibrary write FShaderLibrary;
    property Shader: TDGLLibShader read FShader write SetShader;
  end;

  TDGLCustomObject = class(TDGLAbstractBaseSceneObject)
  published
    { Published Declarations }
    property BuiltProperties;
    property ShaderLibrary;
    property Shader;
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

  // TDGLPlane
  //

  TDGLPlaneStyle  = (psSingleQuad, psTileTexture);
  TDGLPlaneStyles = set of TDGLPlaneStyle;
  TDGLPlane = class(TDGLCustomObject)
  private
    FXOffset, FYOffset: TGLFloat;
    FXScope, FYScope:   TGLFloat;
    FWidth, FHeight:    TGLFloat;
    FXTiles, FYTiles:   Cardinal;
    FStyle:             TDGLPlaneStyles;
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
    procedure SetStyle(const val: TDGLPlaneStyles);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;

    function AxisAlignedDimensionsUnscaled: TVector; override;
    function ScreenRect(aBuffer: TDGLSceneBuffer): TDGLRect;
    function PointDistance(const aPoint: TVector): Single;

  published
    { Public Declarations }
    property Height:  TGLFloat read FHeight write SetHeight;
    property Width:   TGLFloat read FWidth write SetWidth;
    property XOffset: TGLFloat read FXOffset write SetXOffset;
    property XScope:  TGLFloat read FXScope write SetXScope stored StoreXScope;
    property XTiles:  Cardinal read FXTiles write SetXTiles default 1;
    property YOffset: TGLFloat read FYOffset write SetYOffset;
    property YScope:  TGLFloat read FYScope write SetYScope stored StoreYScope;
    property YTiles:  Cardinal read FYTiles write SetYTiles default 1;
    property Style:   TDGLPlaneStyles read FStyle write SetStyle default [psSingleQuad, psTileTexture];
  end;

  // TDGLSprite
  //

  TDGLSprite = class(TDGLCustomObject)
  private
    { Private Declarations }
    FWidth:             TGLFloat;
    FHeight:            TGLFloat;
    FRotation:          TGLFloat;
    FMirrorU, FMirrorV: Boolean;
  protected
    { Protected Declarations }
    procedure SetWidth(const val: TGLFloat);
    procedure SetHeight(const val: TGLFloat);
    procedure SetRotation(const val: TGLFloat);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure SetSize(const Width, Height: TGLFloat);
    procedure SetSquareSize(const size: TGLFloat);
  published
    property Width:    TGLFloat read FWidth write SetWidth;
    property Height:   TGLFloat read FHeight write SetHeight;
    property Rotation: TGLFloat read FRotation write SetRotation;
    property MirrorU:  Boolean read FMirrorU write SetMirrorU default False;
    property MirrorV:  Boolean read FMirrorV write SetMirrorV default False;
  end;

  // TDGLCube
  //
  TCubePart = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
  TCubeParts = set of TCubePart;
  TDGLCube = class(TDGLCustomObject)
  private
    { Private Declarations }
    FCubeSize:        TAffineVector;
    FParts:           TCubeParts;
    FNormalDirection: TNormalDirection;
    procedure SetCubeSize(const Index:Integer;const aValue: Single);
    function GetCubeSize(const Index:Integer):Single;
    procedure SetParts(aValue: TCubeParts);
    procedure SetNormalDirection(aValue: TNormalDirection);
  protected
    { Protected Declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;

    function GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette; override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;

  published
    { Published Declarations }
    property CubeWidth:       Single index 0 read GetCubeSize write SetCubeSize stored False;
    property CubeHeight:      Single index 1 read GetCubeSize write SetCubeSize stored False;
    property CubeDepth:       Single index 2 read GetCubeSize write SetCubeSize stored False;
    property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
    property Parts:           TCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  end;

  // TDGLSphere
  //
  TAngleLimit1 = -90 .. 90;
  TAngleLimit2 = 0 .. 360;
  TCapType = (ctNone, ctCenter, ctFlat);
  TDGLSphere = class(TDGLCustomObject)
  private
    { Private Declarations }
    FRadius:             TGLFloat;
    FSlices, FStacks:    TGLInt;
    FTop:                TAngleLimit1;
    FBottom:             TAngleLimit1;
    FStart:              TAngleLimit2;
    FStop:               TAngleLimit2;
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
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;

    function GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette; override;
  published
    { Published Declarations }
    property Bottom:    TAngleLimit1 read FBottom write SetBottom default -90;
    property BottomCap: TCapType read FBottomCap write SetBottomCap default ctNone;
    property Radius:    TGLFloat read FRadius write SetRadius;
    property Slices:    TGLInt read FSlices write SetSlices default 16;
    property Stacks:    TGLInt read FStacks write SetStacks default 16;
    property Start:     TAngleLimit2 read FStart write SetStart default 0;
    property Stop:      TAngleLimit2 read FStop write SetStop default 360;
    property Top:       TAngleLimit1 read FTop write SetTop default 90;
    property TopCap:    TCapType read FTopCap write SetTopCap default ctNone;
  end;

  // TDGLGeoSphere
  //
  TDGLGeoSphere = class(TDGLCustomObject)
  private
    { Private Declarations }
    FRadius: TGLFloat;
    FLevel:  TGLInt;

    procedure SetRadius(const Value: TGLFloat);
    procedure SetSubdivisionLevel(Value: TGLInt);
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TRenderContextInfo); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;

    function GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette; override;
  published
    { Published Declarations }
    property Radius:           TGLFloat read FRadius write SetRadius;
    property SubdivisionLevel: TGLInt read FLevel write SetSubdivisionLevel default 2;
  end;

  TDGLDisk = class(TDGLCustomObject)
  private
    { Private Declarations }
    FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius: TGLFloat;
    FSlices, FLoops:                                      TGLInt;
    procedure SetOuterRadius(const aValue: Single);
    procedure SetInnerRadius(const aValue: Single);
    procedure SetSlices(aValue: TGLInt);
    procedure SetLoops(aValue: TGLInt);
    procedure SetStartAngle(const aValue: Single);
    procedure SetSweepAngle(const aValue: Single);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TRenderContextInfo); override;

    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;

  published
    property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
    property Loops:       TGLInt read FLoops write SetLoops default 2;
    property OuterRadius: TGLFloat read FOuterRadius write SetOuterRadius;
    property Slices:      TGLInt read FSlices write SetSlices default 16;
    property StartAngle:  TGLFloat read FStartAngle write SetStartAngle;
    property SweepAngle:  TGLFloat read FSweepAngle write SetSweepAngle;
  end;

  TBilletMeshPrimitive = (bmpPoint, bmpLine, bmpTriangle);
  // TDGLPoint
  // TDGLine
  // TDGLTriangle

  TDGLBilletMesh = class(TDGLCustomObject)
  private
    { Private Declarations }
   // FFactory:       TDGLBaseFactory;
    FPrimitiveType: TBilletMeshPrimitive;
    FVertexNumber:  Integer;
    FIndexed:       Boolean;
  //  procedure SetFactory(Value: TDGLBaseFactory);
    procedure SetPrimitiveType(Value: TBilletMeshPrimitive);
    procedure SetVertexNumber(Value: Integer);
    procedure SetIndexed(Value: Boolean);
  protected
    { Protected declaration }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
  published
   // property Factory:       TDGLBaseFactory read FFactory write SetFactory;
    property PrimitiveType: TBilletMeshPrimitive read FPrimitiveType write SetPrimitiveType default bmpPoint;
    property VertexNumber:  Integer read FVertexNumber write SetVertexNumber default 1;
    property Indexed:       Boolean read FIndexed write SetIndexed default False;
  end;

  //TDGLCylinder
  //TDGLCone
  //TDGLTorus
  //TDGLPQTorus
  //TDGLSuperEllipsoid




implementation

uses
  Math, DGLResStrings, DGLState, DGLVectorLists;

var
  DefaultProgram: TDGLProgramHandle = nil;



  // CubeWireframeBuildList
  //

procedure CubeWireframeBuildList(var rci: TRenderContextInfo; Size: TGLFloat; const Color: TColorVector);
//var
//  mi, ma: Single;
begin
{$IFDEF GLS_OPENGL_DEBUG}
  if glGREMEDY_string_marker then
    glStringMarkerGREMEDY(22, 'CubeWireframeBuildList');
{$ENDIF}
//  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stLineSmooth);
//  if stipple then
//  begin
//    rci.GLStates.Enable(stLineStipple);
//    rci.GLStates.Enable(stBlend);
//    rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
//    rci.GLStates.LineStippleFactor := 1;
//    rci.GLStates.LineStipplePattern := $CCCC;
//  end;
//  rci.GLStates.LineWidth := 1;
//  ma := 0.5 * Size;
//  mi := -ma;

//  glColor4fv(@Color);
//  glBegin_(GL_LINE_STRIP);
//  // front face
//  glVertex3f(ma, mi, mi);
//  glVertex3f(ma, ma, mi);
//  glVertex3f(ma, ma, ma);
//  glVertex3f(ma, mi, ma);
//  glVertex3f(ma, mi, mi);
//  // partial up back face
//  glVertex3f(mi, mi, mi);
//  glVertex3f(mi, mi, ma);
//  glVertex3f(mi, ma, ma);
//  glVertex3f(mi, ma, mi);
//  // right side low
//  glVertex3f(ma, ma, mi);
//  glEnd_;
//  glBegin_(GL_LINES);
//  // right high
//  glVertex3f(ma, ma, ma);
//  glVertex3f(mi, ma, ma);
//  // back low
//  glVertex3f(mi, mi, mi);
//  glVertex3f(mi, ma, mi);
//  // left high
//  glVertex3f(ma, mi, ma);
//  glVertex3f(mi, mi, ma);
//  glEnd_;
end;

// ------------------
// ------------------ TGLDummyCube ------------------
// ------------------
{$IFDEF GLS_REGION}{$REGION 'TDGLDummyCube'}{$ENDIF}
// Create
//

constructor TDGLDummyCube.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FCubeSize := 1;
  FEdgeColor := TDGLColor.Create(Self);
  FEdgeColor.Initialize(clrWhite);
//  FGroupList := TGLListHandle.Create;
  CamInvarianceMode := cimNone;
end;

// Destroy
//

destructor TDGLDummyCube.Destroy;
begin
//  FGroupList.Free;
  FEdgeColor.Free;
  inherited;
end;

// Assign
//

procedure TDGLDummyCube.Assign(Source: TPersistent);
begin
  if Source is TDGLDummyCube then
  begin
    FCubeSize := TDGLDummyCube(Source).FCubeSize;
    FEdgeColor.Color := TDGLDummyCube(Source).FEdgeColor.Color;
    FVisibleAtRunTime := TDGLDummyCube(Source).FVisibleAtRunTime;
    NotifyChange(Self);
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//

function TDGLDummyCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FCubeSize);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// RayCastIntersect
//

function TDGLDummyCube.RayCastIntersect(const rayStart, rayVector: TVector;
  intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
begin
  Result := False;
end;

// BuildList
//

procedure TDGLDummyCube.BuildList(var rci: TRenderContextInfo);
begin
  if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
    CubeWireframeBuildList(rci, FCubeSize, EdgeColor.Color);
end;

// DoRender
//

procedure TDGLDummyCube.DoRender(var rci: TRenderContextInfo;
  renderSelf, renderChildren: Boolean);
begin
  if Assigned(FOnVisibilityDetermination) then
    if not FOnVisibilityDetermination(Self, rci) then  Exit;
 // if FAmalgamate and (not rci.amalgamating) then
//  begin
//    if FGroupList.Handle = 0 then
//    begin
//      FGroupList.AllocateHandle;
//      Assert(FGroupList.Handle <> 0, 'Handle=0 for ' + ClassName);
//      rci.GLStates.NewList(FGroupList.Handle, GL_COMPILE);
//      rci.amalgamating := True;
//      try
//        inherited;
//      finally
//        rci.amalgamating := False;
//        rci.GLStates.EndList;
//      end;
//    end;
//    rci.GLStates.CallList(FGroupList.Handle);
//  end
//  else
//  begin
    // proceed as usual
    inherited;
//  end;
end;

// StructureChanged
//

procedure TDGLDummyCube.StructureChanged;
begin
//  if FAmalgamate then
//    FGroupList.DestroyHandle;
  inherited;
end;

// BarycenterAbsolutePosition
//

function TDGLDummyCube.BarycenterAbsolutePosition: TVector;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Children[0].BarycenterAbsolutePosition;
    for i := 1 to Count - 1 do
      Result := VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
    ScaleVector(Result, 1 / Count);
  end
  else
    Result := AbsolutePosition;
end;

// SetCubeSize
//

procedure TDGLDummyCube.SetCubeSize(const val: TGLFloat);
begin
  if val <> FCubeSize then
  begin
    FCubeSize := val;
    StructureChanged;
  end;
end;

// SetEdgeColor
//

procedure TDGLDummyCube.SetEdgeColor(const val: TDGLColor);
begin
  if val <> FEdgeColor then
  begin
    FEdgeColor.Assign(val);
    StructureChanged;
  end;
end;

// SetVisibleAtRunTime
//

procedure TDGLDummyCube.SetVisibleAtRunTime(const val: Boolean);
begin
  if val <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := val;
    StructureChanged;
  end;
end;

// SetAmalgamate
//

procedure TDGLDummyCube.SetAmalgamate(const val: Boolean);
begin
//  if val <> FAmalgamate then
//  begin
//    FAmalgamate := val;
//    if not val then
//      FGroupList.DestroyHandle;
    inherited StructureChanged;
//  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}


// ------------------
{ TDGLAbstractBaseSceneObjec }
{$IFDEF GLS_REGION}{$REGION 'TDGLDummyCube'}{$ENDIF}
constructor TDGLAbstractBaseSceneObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuiltProperties                   := TDGLBuiltProperties.Create(Self);
  FBuiltProperties.OwnerNotifyChange := NotifyChange;
  FShader                            := TDGLLibShader.Create(nil);
  ObjectStyle                        := ObjectStyle + [osDirectDraw, osBuiltStage];
end;

destructor TDGLAbstractBaseSceneObject.Destroy;
begin
  FBuiltProperties.Destroy;
  FShader.Destroy;
  inherited;
end;

procedure TDGLAbstractBaseSceneObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
//  if Assigned(FShader) then
//    if (AComponent = FShader.LibMaterial) and (Operation = opRemove) then
//    begin
//      FShader.LibMaterial := nil;
//    end;
  inherited;
end;

procedure TDGLAbstractBaseSceneObject.DoRender(var ARci: TRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
var
  vManager:          TDGLBaseRenderManager;
  UseDefaultProgram: Boolean;
  SB:                TDGLSceneBuffer;
  LS:                TDGLLightSource;
  ModelMatrix:       TMatrix;
begin
  if GL_VERSION_3_3 then
  begin
    if ARenderSelf then
    begin
      UseDefaultProgram := true;
      // if runtime
      if not(csDesigning in ComponentState) then
      begin
        if Assigned(FShader) then
          UseDefaultProgram := False;
      end
      else
        UseDefaultProgram := true;

      if UseDefaultProgram and (DefaultProgram.Handle = 0) then
      begin
        DefaultProgram.AllocateHandle;
        with DefaultProgram do
        begin
//          if GL_VERSION_3_2 then
//          begin
            AddShader(TDGLVertexShaderHandle, string(DefaultShader_vp), true);
            AddShader(TDGLFragmentShaderHandle, string(DefaultShader_fp), true);
//          end
//          else
//          begin
//            AddShader(TGLVertexShaderHandle, string(DefaultShader_vp120), true);
//            AddShader(TGLFragmentShaderHandle, string(DefaultShader_fp120), true);
//          end;
          if not LinkProgram then
            UseDefaultProgram := False;
          if not ValidateProgram then
            UseDefaultProgram := False;
        end;
      end;

      if UseDefaultProgram then
        with DefaultProgram do
        begin
          UseProgramObject;
          SB := TDGLSceneBuffer(ARci.buffer);
          if Assigned(SB) then
          begin
            UniformMatrix4fv['ModelMatrix']          := SB.ModelMatrix;
            UniformMatrix4fv['ViewProjectionMatrix'] := MatrixMultiply(SB.ViewMatrix, SB.ProjectionMatrix);
          end
          else
          begin
            if ocTransformation in Changes then RebuildMatrix;
            ModelMatrix                              := LocalMatrix^;
            UniformMatrix4fv['ModelMatrix']          := ModelMatrix;
            UniformMatrix4fv['ViewProjectionMatrix'] := MatrixMultiply(vDefaultViewMatrix, vDefaultProjectionMatrix);
          end;
          if Assigned(ARci.lights) then
          begin
            LS := TDGLLightSource(ARci.lights.First);
            if Assigned(LS) then
              Uniform4f['LightSourcePos'] := LS.AbsolutePosition;
          end
          else
            Uniform4f['LightSourcePos'] := vDefaultLightSourcePosition;
        end
      else
        FShader.Apply(ARci);

      vManager := FBuiltProperties.Manager;
      if (osBuiltStage in ObjectStyle) or (vManager is TDGLDynamicRenderManager) then
      begin
        try
          Self.BuildList(ARci);
        except
          vManager.Discard;
          Self.Visible := False;
        end;
      end
      else
        vManager.RenderClient(FBuiltProperties, ARci);

      if UseDefaultProgram then
        DefaultProgram.EndUseProgramObject
      else
        FShader.UnApply(ARci);
    end;
  end;

  if ARenderChildren then
    Self.renderChildren(0, Count - 1, ARci);
end;

procedure TDGLAbstractBaseSceneObject.BuildList(var rci: TRenderContextInfo);
begin
  ObjectStyle := ObjectStyle - [osBuiltStage];
end;

procedure TDGLAbstractBaseSceneObject.SetBuiltProperties(const Value: TDGLBuiltProperties);
begin
  FBuiltProperties.Assign(Value);
end;

procedure TDGLAbstractBaseSceneObject.SetShader(const Value: TDGLLibShader);
begin
  FShader.Assign(Value);
end;

procedure TDGLAbstractBaseSceneObject.StructureChanged;
begin
  inherited;
  if FBuiltProperties.Usage = buStream then
    ObjectStyle := ObjectStyle + [osBuiltStage];
end;

procedure TDGLAbstractBaseSceneObject.NotifyChange(Sender: TObject);
begin
  inherited NotifyChange(Sender);
  if Sender is TDGLBuiltProperties then
    StructureChanged;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TDGLPlane ------------------
// ------------------

constructor TDGLPlane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth  := 1;
  FHeight := 1;
  FXTiles := 1;
  FYTiles := 1;
  FXScope := 1;
  FYScope := 1;
  FStyle  := [psSingleQuad, psTileTexture];
end;

// BuildList
//

procedure TDGLPlane.BuildList(var rci: TRenderContextInfo);
var
  hw, hh, posXFact, posYFact, pX, pY0, pY1: TGLFloat;
  tx0, tx1, ty0, ty1, texSFact, texTFact:   TGLFloat;
  texS, texT0, texT1:                       TGLFloat;
  x, y:                                     Integer;
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
  with FBuiltProperties.Manager do
  begin
    BeginObject(FBuiltProperties);
    Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, 1);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    if psSingleQuad in FStyle then
    begin
      BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, tx1, ty1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, 0);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, tx0, ty1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh, 0);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, tx1, ty0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh, 0);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, tx0, ty0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, 0);
      EmitVertex;
      EndPrimitives;
    end
    else
    begin
      // multi-quad plane (actually built from tri-strips)
      texSFact := (tx1 - tx0) / FXTiles;
      texTFact := (ty1 - ty0) / FYTiles;
      posXFact := FWidth / FXTiles;
      posYFact := FHeight / FYTiles;
      texT0    := 0;
      pY0      := -hh;
      BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
      for y := 0 to FYTiles - 1 do
      begin
        texT1 := (y + 1) * texTFact;
        pY1   := (y + 1) * posYFact - hh;
        for x := 0 to FXTiles do
        begin
          texS := tx0 + x * texSFact;
          pX   := x * posXFact - hw;
          Attribute2f(RenderingContext.GLStates.attrTexCoord0, texS, texT1);
          Attribute3f(RenderingContext.GLStates.attrPosition, pX, pY1, 0);
          EmitVertex;
          Attribute2f(RenderingContext.GLStates.attrTexCoord0, texS, texT0);
          Attribute3f(RenderingContext.GLStates.attrPosition, pX, pY0, 0);
          EmitVertex;
        end;
        RestartStrip;
        texT0 := texT1;
        pY0   := pY1;
      end;
      EndPrimitives;
    end;
    EndObject(rci);
  end;
  inherited;
end;

// Assign
//

procedure TDGLPlane.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TDGLPlane) then
  begin
    FWidth  := TDGLPlane(Source).FWidth;
    FHeight := TDGLPlane(Source).FHeight;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TDGLPlane.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  Result.Z := 0;
end;

// SetWidth
//

procedure TDGLPlane.SetWidth(const aValue: Single);
begin

  if aValue <> FWidth then
  begin
    FWidth := aValue;
    StructureChanged;
  end;
end;

// ScreenRect
//

function TDGLPlane.ScreenRect(aBuffer: TDGLSceneBuffer): TDGLRect;
var
  v:      array [0 .. 3] of TVector;
  buf:    TDGLSceneBuffer;
  hw, hh: Single;
begin
  buf := aBuffer;
  if Assigned(buf) then
  begin
    hw   := FWidth * 0.5;
    hh   := FHeight * 0.5;
    v[0].X := LocalToAbsolute(PointMake(-hw, -hh, 0)).X;
    v[1].Y := LocalToAbsolute(PointMake(hw, -hh, 0)).Y;
    v[2].Z := LocalToAbsolute(PointMake(hw, hh, 0)).Z;
    v[3].W := LocalToAbsolute(PointMake(-hw, hh, 0)).W;
    buf.WorldToScreen(@v[0], 4);
    Result.Left   := Round(MinFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Right  := Round(MaxFloat([v[0].X, v[1].X, v[2].X, v[3].X]));
    Result.Top    := Round(MinFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
    Result.Bottom := Round(MaxFloat([v[0].Y, v[1].Y, v[2].Y, v[3].Y]));
  end
  else
    FillChar(Result, SizeOf(TDGLRect), 0);
end;

// PointDistance
//

function TDGLPlane.PointDistance(const aPoint: TVector): Single;
begin
  Result := VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition), AbsoluteDirection);
end;

// SetHeight
//

procedure TDGLPlane.SetHeight(const aValue: Single);
begin

  if aValue <> FHeight then
  begin
    FHeight := aValue;
    StructureChanged;
  end;
end;

// SetXOffset
//

procedure TDGLPlane.SetXOffset(const Value: TGLFloat);
begin

  if Value <> FXOffset then
  begin
    FXOffset := Value;
    StructureChanged;
  end;
end;

// SetXScope
//

procedure TDGLPlane.SetXScope(const Value: TGLFloat);
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

function TDGLPlane.StoreXScope: Boolean;
begin
  Result := (FXScope <> 1);
end;

// SetXTiles
//

procedure TDGLPlane.SetXTiles(const Value: Cardinal);
begin

  if Value <> FXTiles then
  begin
    FXTiles := Value;
    StructureChanged;
  end;
end;

// SetYOffset
//

procedure TDGLPlane.SetYOffset(const Value: TGLFloat);
begin

  if Value <> FYOffset then
  begin
    FYOffset := Value;
    StructureChanged;
  end;
end;

// SetYScope
//

procedure TDGLPlane.SetYScope(const Value: TGLFloat);
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

function TDGLPlane.StoreYScope: Boolean;
begin
  Result := (FYScope <> 1);
end;

// SetYTiles
//

procedure TDGLPlane.SetYTiles(const Value: Cardinal);
begin

  if Value <> FYTiles then
  begin
    FYTiles := Value;
    StructureChanged;
  end;
end;

// SetStyle
//

procedure TDGLPlane.SetStyle(const val: TDGLPlaneStyles);
begin

  if val <> FStyle then
  begin
    FStyle := val;
    StructureChanged;
  end;
end;

{$IFDEF GLS_REGION}{$ENDREGION}{$ENDIF}

// ------------------
// ------------------ TDGLSprite ------------------
// ------------------

// Create
//

constructor TDGLSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osNoVisibilityCulling];
  FWidth      := 1;
  FHeight     := 1;
end;

// Assign
//

procedure TDGLSprite.Assign(Source: TPersistent);
begin

  if Source is TDGLSprite then
  begin
    FWidth    := TDGLSprite(Source).FWidth;
    FHeight   := TDGLSprite(Source).FHeight;
    FRotation := TDGLSprite(Source).FRotation;
  end;
  inherited Assign(Source);
end;

function TDGLSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := 0.5 * Abs(FWidth);
  Result.Y := 0.5 * Abs(FHeight);
  Result.Z := 0.5 * Abs(FWidth);
end;

// BuildList
//

procedure TDGLSprite.BuildList(var rci: TRenderContextInfo);
var
  vx, vy:         TAffineVector;
  w, h:           Single;
  mat:            TMatrix;
  u0, v0, u1, v1: Integer;
begin
  mat   := TDGLSceneBuffer(rci.buffer).ModelMatrix;
  w     := FWidth * 0.5;
  h     := FHeight * 0.5;
  vx.X := mat.X.X;
  vy.X := mat.X.Y;
  vx.Y := mat.Y.X;
  vy.Y := mat.Y.Y;
  vx.Z := mat.Z.X;
  vy.Z := mat.Z.Y;
  ScaleVector(vx, w / VectorLength(vx));
  ScaleVector(vy, h / VectorLength(vy));
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
    mat := CreateRotationMatrix(ZVector, FRotation);
    vx  := VectorTransform(vx, mat);
    vy  := VectorTransform(vy, mat);
  end;

  with FBuiltProperties.Manager do
  begin
    BeginObject(FBuiltProperties);
   // Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrPosition,0,0,0);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, u1, v1);
    Attribute3f(RenderingContext.GLStates.attrPosition, vx.X + vy.X, vx.Y + vy.Y, vx.Z + vy.Z);
    EmitVertex;
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, u0, v1);
    Attribute3f(RenderingContext.GLStates.attrPosition, -vx.X + vy.X, -vx.Y + vy.Y, -vx.Z + vy.Z);
    EmitVertex;
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, u1, v0);
    Attribute3f(RenderingContext.GLStates.attrPosition, vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
    EmitVertex;
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, u0, v0);
    Attribute3f(RenderingContext.GLStates.attrPosition, -vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
    EmitVertex;
    EndPrimitives;
    EndObject(rci);
  end;
  inherited;
end;

// SetWidth
//

procedure TDGLSprite.SetWidth(const val: TGLFloat);
begin

  if FWidth <> val then
  begin
    FWidth := val;
    NotifyChange(Self);
  end;
end;

// SetHeight
//

procedure TDGLSprite.SetHeight(const val: TGLFloat);
begin

  if FHeight <> val then
  begin
    FHeight := val;
    NotifyChange(Self);
  end;
end;

// SetRotation
//

procedure TDGLSprite.SetRotation(const val: TGLFloat);
begin

  if FRotation <> val then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TDGLSprite.SetMirrorU(const val: Boolean);
begin

  FMirrorU := val;
  NotifyChange(Self);
end;

// SetMirrorV
//

procedure TDGLSprite.SetMirrorV(const val: Boolean);
begin

  FMirrorV := val;
  NotifyChange(Self);
end;

// SetSize
//

procedure TDGLSprite.SetSize(const Width, Height: TGLFloat);
begin

  FWidth  := Width;
  FHeight := Height;
  NotifyChange(Self);
end;

// SetSquareSize
//

procedure TDGLSprite.SetSquareSize(const size: TGLFloat);
begin

  FWidth  := size;
  FHeight := size;
  NotifyChange(Self);
end;


// ------------------
// ------------------ TDGLCube ------------------
// ------------------

// Create
//

constructor TDGLCube.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCubeSize        := XYZVector;
  FParts           := [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection := ndOutside;
end;

// BuildList
//

procedure TDGLCube.BuildList(var rci: TRenderContextInfo);
var
  hw, hh, hd, nd: TGLFloat;
begin
  with FBuiltProperties.Manager do
  begin
    if FNormalDirection = ndInside then
      nd := -1
    else
      nd := 1;
    hw   := FCubeSize.X * 0.5;
    hh   := FCubeSize.Y * 0.5;
    hd   := FCubeSize.Z * 0.5;
    BeginObject(FBuiltProperties);
    Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, 0);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLES);
    if cpFront in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, nd);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw * nd, hh * nd, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw * nd, -hh * nd, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
    end;
    if cpBack in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, -nd);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw * nd, -hh * nd, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw * nd, hh * nd, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, -hd);
      EmitVertex;
    end;
    if cpLeft in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, -nd, 0, 0);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh * nd, -hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh * nd, hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh, hd);
      EmitVertex;
    end;
    if cpRight in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, nd, 0, 0);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh * nd, hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh, -hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh * nd, -hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
    end;
    if cpTop in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, nd, 0);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw * nd, hh, hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw * nd, hh, -hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, hh, -hd);
      EmitVertex;
    end;
    if cpBottom in FParts then
    begin
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, -nd, 0);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw * nd, -hh, -hd * nd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 1, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh, hd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrPosition, hw, -hh, hd);
      EmitVertex;
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 1);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw * nd, -hh, hd * nd);
      EmitVertex;
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, -nd, 0);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
      Attribute3f(RenderingContext.GLStates.attrPosition, -hw, -hh, -hd);
      EmitVertex;
    end;
    EndPrimitives;
    EndObject(rci);
  end;
  inherited;
end;

// GenerateSilhouette
//

function TDGLCube.GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette;
var
  hw, hh, hd:   TGLFloat;
  connectivity: TConnectivity;
  sil:         TDGLSilhouette;
begin
  connectivity := TConnectivity.Create(true);

  hw := FCubeSize.X * 0.5;
  hh := FCubeSize.Y * 0.5;
  hd := FCubeSize.Z * 0.5;

  if cpFront in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd), AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, -hh, hd), AffineVectorMake(hw, -hh, hd));
  end;
  if cpBack in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, -hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(-hw, hh, -hd));
  end;
  if cpLeft in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, hd), AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(-hw, -hh, hd));
  end;
  if cpRight in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(hw, hh, hd), AffineVectorMake(hw, -hh, hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, hh, -hd));
  end;
  if cpTop in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, hh, -hd), AffineVectorMake(-hw, hh, hd), AffineVectorMake(hw, hh, hd), AffineVectorMake(hw, hh, -hd));
  end;
  if cpBottom in FParts then
  begin
    connectivity.AddQuad(AffineVectorMake(-hw, -hh, -hd), AffineVectorMake(hw, -hh, -hd), AffineVectorMake(hw, -hh, hd), AffineVectorMake(-hw, -hh, hd));
  end;

  sil := nil;
  connectivity.CreateSilhouette(silhouetteParameters, sil, False);

  Result := sil;

  connectivity.Free;
end;



function TDGLCube.GetCubeSize(const Index: Integer):Single;
begin
  result:=FCubeSize.V[Index];
end;

// SetCubeDepth
//

procedure TDGLCube.SetCubeSize(const Index:Integer;const aValue: Single);
begin

  if aValue <> FCubeSize.V[Index] then
  begin
    FCubeSize.V[Index] := aValue;
    StructureChanged;
  end;
end;

// SetParts
//

procedure TDGLCube.SetParts(aValue: TCubeParts);
begin

  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

// SetNormalDirection
//

procedure TDGLCube.SetNormalDirection(aValue: TNormalDirection);
begin

  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TDGLCube.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TDGLCube) then
  begin
    FCubeSize        := TDGLCube(Source).FCubeSize;
    FParts           := TDGLCube(Source).FParts;
    FNormalDirection := TDGLCube(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TDGLCube.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := FCubeSize.X * 0.5;
  Result.Y := FCubeSize.Y * 0.5;
  Result.Z := FCubeSize.Z * 0.5;
  Result.W := 0;
end;

// RayCastIntersect
//

function TDGLCube.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  p:     array [0 .. 5] of TVector;
  rv:    TVector;
  rs, r: TVector;
  i:     Integer;
  t, e:  Single;
  eSize: TAffineVector;
begin
  rs := AbsoluteToLocal(rayStart);
  SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
  e        := 0.5 + 0.0001; // Small value for floating point imprecisions
  eSize.X := FCubeSize.X * e;
  eSize.Y := FCubeSize.Y * e;
  eSize.Z := FCubeSize.Z * e;
  p[0]     := XHmgVector;
  p[1]     := YHmgVector;
  p[2]     := ZHmgVector;
  SetVector(p[3], -1, 0, 0);
  SetVector(p[4], 0, -1, 0);
  SetVector(p[5], 0, 0, -1);
  for i := 0 to 5 do
  begin
    if VectorDotProduct(p[i], rv) > 0 then
    begin
      t := -(p[i].X * rs.X + p[i].Y * rs.Y + p[i].Z * rs.Z + 0.5 * FCubeSize.V[i mod 3]) / (p[i].X * rv.X + p[i].Y * rv.Y + p[i].Z * rv.Z);
      MakePoint(r, rs.X + t * rv.X, rs.Y + t * rv.Y, rs.Z + t * rv.Z);
      if (Abs(r.X) <= eSize.X) and (Abs(r.Y) <= eSize.Y) and (Abs(r.Z) <= eSize.Z) and (VectorDotProduct(VectorSubtract(r, rs), rv) > 0) then
      begin
        if Assigned(intersectPoint) then
          MakePoint(intersectPoint^, LocalToAbsolute(r));
        if Assigned(intersectNormal) then
          MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
        Result := true;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

// DefineProperties
//

procedure TDGLCube.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData, (FCubeSize.X <> 1) or (FCubeSize.Y <> 1) or (FCubeSize.Z <> 1));
end;

// ReadData
//

procedure TDGLCube.ReadData(Stream: TStream);
begin
  Stream.Read(FCubeSize, SizeOf(TAffineVector));
end;

// WriteData
//

procedure TDGLCube.WriteData(Stream: TStream);
begin
  Stream.Write(FCubeSize, SizeOf(TAffineVector));
end;

// ------------------
// ------------------ TDGLSphere ------------------
// ------------------

// Create
//

constructor TDGLSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FSlices := 16;
  FStacks := 16;
  FTop    := 90;
  FBottom := -90;
  FStart  := 0;
  FStop   := 360;
end;

// BuildList
//

procedure TDGLSphere.BuildList(var rci: TRenderContextInfo);
var
  v1, V2, N1:                                                TAffineVector;
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH:        Extended;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta:    Extended;
  uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1: Single;
  i, J:                                                      Integer;
  NeedEnd:                                                   Boolean;
begin
  // common settings
  AngTop    := DegToRad(1.0 * FTop);
  AngBottom := DegToRad(1.0 * FBottom);
  AngStart  := DegToRad(1.0 * FStart);
  AngStop   := DegToRad(1.0 * FStop);
  StepH     := (AngStop - AngStart) / FSlices;
  StepV     := (AngTop - AngBottom) / FStacks;

  with FBuiltProperties.Manager do
  begin
    BeginObject(FBuiltProperties);
    Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, 0);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    // top cap
    NeedEnd := False;
    if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then
    begin
      BeginPrimitives(GLVBOM_TRIANGLE_FAN);
      SinCos(AngTop, SinP, CosP);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0.5, 0.5);
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, 1, 0);
      if FTopCap = ctCenter then
        Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0)
      else
      begin
        Attribute3f(RenderingContext.GLStates.attrPosition, 0, SinP * Radius, 0);
        N1 := YVector;
      end;
      EmitVertex;
      v1.Y := SinP;
      Theta := AngStart;
      for i := 0 to FSlices do
      begin
        SinCos(Theta, SinT, CosT);
        v1.X := CosP * SinT;
        v1.Z := CosP * CosT;
        if FTopCap = ctCenter then
          N1 := VectorPerpendicular(YVector, v1);
        Attribute2f(RenderingContext.GLStates.attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
        Attribute3f(RenderingContext.GLStates.attrNormal, N1.X, N1.Y, N1.Z);
        Attribute3f(RenderingContext.GLStates.attrPosition, v1.X * Radius, v1.Y * Radius, v1.Z * Radius);
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
        BeginPrimitives(GLVBOM_TRIANGLE_FAN);
      SinCos(AngBottom, SinP, CosP);
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0.5, 0.5);
      Attribute3f(RenderingContext.GLStates.attrNormal, 0, -1, 0);
      if FBottomCap = ctCenter then
        Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0)
      else
      begin
        Attribute3f(RenderingContext.GLStates.attrPosition, 0, SinP * Radius, 0);
        N1 := YVector;
      end;
      EmitVertex;
      v1.Y := SinP;
      Theta := AngStop;
      for i := 0 to FSlices do
      begin
        SinCos(Theta, SinT, CosT);
        v1.X := CosP * SinT;
        v1.Z := CosP * CosT;
        if FTopCap = ctCenter then
          N1 := VectorPerpendicular(AffineVectorMake(0, -1, 0), v1);
        Attribute2f(RenderingContext.GLStates.attrTexCoord0, SinT * 0.5 + 0.5, CosT * 0.5 + 0.5);
        Attribute3f(RenderingContext.GLStates.attrNormal, N1.X, N1.Y, N1.Z);
        Attribute3f(RenderingContext.GLStates.attrPosition, v1.X * Radius, v1.Y * Radius, v1.Z * Radius);
        EmitVertex;
        Theta := Theta - StepH;
      end;
      NeedEnd := true;
    end;
    if NeedEnd then
      EndPrimitives;

    // main body
    Phi  := AngTop;
    Phi2 := Phi - StepV;

    uTexFactor := 1 / FSlices;
    vTexFactor := 1 / FStacks;

    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    for J := 0 to FStacks - 1 do
    begin
      Theta := AngStart;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      v1.Y      := SinP;
      V2.Y      := SinP2;
      vTexCoord0 := 1 - J * vTexFactor;
      vTexCoord1 := 1 - (J + 1) * vTexFactor;

      for i := 0 to FSlices do
      begin
        SinCos(Theta, SinT, CosT);
        v1.X := CosP * SinT;
        V2.X := CosP2 * SinT;
        v1.Z := CosP * CosT;
        V2.Z := CosP2 * CosT;

        uTexCoord := i * uTexFactor;
        Attribute2f(RenderingContext.GLStates.attrTexCoord0, uTexCoord, vTexCoord0);
        Attribute3f(RenderingContext.GLStates.attrNormal, v1.X, v1.Y, v1.Z);
        Attribute3f(RenderingContext.GLStates.attrPosition, v1.X * Radius, v1.Y * Radius, v1.Z * Radius);
        EmitVertex;

        Attribute2f(RenderingContext.GLStates.attrTexCoord0, uTexCoord, vTexCoord1);
        Attribute3f(RenderingContext.GLStates.attrNormal, V2.X, V2.Y, V2.Z);
        Attribute3f(RenderingContext.GLStates.attrPosition, V2.X * Radius, V2.Y * Radius, V2.Z * Radius);
        EmitVertex;

        Theta := Theta + StepH;
      end;
      RestartStrip;
      Phi  := Phi2;
      Phi2 := Phi2 - StepV;
    end;
    EndPrimitives;
    EndObject(rci);
  end;
  inherited;
end;

// RayCastIntersect
//

function TDGLSphere.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2:                  TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1, i2) > 0 then
  begin
    Result := true;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//

function TDGLSphere.GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette;
var
  i, J:              Integer;
  s, c, angleFactor: Single;
  sVec, tVec:        TAffineVector;
  Segments:          Integer;
begin
  Segments := MaxInteger(FStacks, FSlices);

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec   := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result      :=TDGLSilhouette.Create;
  angleFactor := (2 * PI) / Segments;
  for i       := 0 to Segments - 1 do
  begin
//    SinCos(i * angleFactor, FRadius, s, c);
    SinCos(i * angleFactor,  s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    J := (i + 1) mod Segments;
    Result.Indices.Add(i, J);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(Segments, i, J)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

// SetBottom
//

procedure TDGLSphere.SetBottom(aValue: TAngleLimit1);
begin

  if FBottom <> aValue then
  begin
    FBottom := aValue;
    StructureChanged;
  end;
end;

// SetBottomCap
//

procedure TDGLSphere.SetBottomCap(aValue: TCapType);
begin

  if FBottomCap <> aValue then
  begin
    FBottomCap := aValue;
    StructureChanged;
  end;
end;

// SetRadius
//

procedure TDGLSphere.SetRadius(const aValue: TGLFloat);
begin

  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TDGLSphere.SetSlices(aValue: Integer);
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

procedure TDGLSphere.SetStacks(aValue: TGLInt);
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

procedure TDGLSphere.SetStart(aValue: TAngleLimit2);
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

procedure TDGLSphere.SetStop(aValue: TAngleLimit2);
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

procedure TDGLSphere.SetTop(aValue: TAngleLimit1);
begin

  if FTop <> aValue then
  begin
    FTop := aValue;
    StructureChanged;
  end;
end;

// SetTopCap
//

procedure TDGLSphere.SetTopCap(aValue: TCapType);
begin

  if FTopCap <> aValue then
  begin
    FTopCap := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TDGLSphere.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TDGLSphere) then
  begin
    FRadius := TDGLSphere(Source).FRadius;
    FSlices := TDGLSphere(Source).FSlices;
    FStacks := TDGLSphere(Source).FStacks;
    FBottom := TDGLSphere(Source).FBottom;
    FTop    := TDGLSphere(Source).FTop;
    FStart  := TDGLSphere(Source).FStart;
    FStop   := TDGLSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TDGLSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// ------------------
// ------------------ TDGLGeoSphere ------------------
// ------------------
// Create
//

constructor TDGLGeoSphere.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRadius := 0.5;
  FLevel  := 1;
end;

// BuildList
//

procedure TDGLGeoSphere.BuildList(var rci: TRenderContextInfo);
var
  pivot, dir1, dir2, v, B:                                            TAffineVector;
  level, numLayers:                                                   Integer;
  dt, t_top, t_bot, ds_top, ds_bot, s_top, s_bot, smax_top, smax_bot: Single;
  offset:                                                             Boolean;

  procedure ProduceVertex(s, t, SB, tb: Single; uprow: Boolean);
  var
    XZR: Single;
  begin
    v.X := pivot.X + s * dir1.X + t * dir2.X;
    v.Y := pivot.Y + s * dir1.Y + t * dir2.Y;
    v.Z := pivot.Z + s * dir1.Z + t * dir2.Z;
    B.X := pivot.X + SB * dir1.X + tb * dir2.X;
    B.Y := pivot.Y + SB * dir1.Y + tb * dir2.Y;
    B.Z := pivot.Z + SB * dir1.Z + tb * dir2.Z;
    NormalizeVector(v);
    NormalizeVector(B);

    if uprow then
    begin
      // Poles
      XZR := sqrt(B.X * B.X + B.Z * B.Z);
      s   := v.X;
      t   := v.Z;
      if XZR > 0 then
      begin
        s := s / XZR;
        t := t / XZR;
      end;
      s := 0.5 + (1 / 6) * s;
      t := (1 / 6) * (t + 1);
      if v.Y > 0 then
        t := 1 - t;
    end
    else
    begin
      // Equator
      s := 0.5 + 0.5 * arctan2(v.X, v.Z) / PI;
      if offset and (s > 0.99) then
        s := 0;

      t := 0.5;
      if B.Y <> 0 then
        t := t + (1 / 6) * v.Y / Abs(B.Y);
    end;

    with FBuiltProperties.Manager do
    begin
      Attribute2f(RenderingContext.GLStates.attrTexCoord0, s, t);
      Attribute3f(RenderingContext.GLStates.attrNormal, v.X, v.Y, v.Z);
      ScaleVector(v, FRadius);
      Attribute3f(RenderingContext.GLStates.attrPosition, v.X, v.Y, v.Z);
      EmitVertex;
    end;
  end;

  procedure ProduceOctant;
  var
    i, J, caps:            Integer;
    t_border, smax_border: Single;
  begin
    t_top  := 1;
    ds_top := 0;
    caps   := numLayers div 2;

    t_border    := 1 - caps * dt;
    smax_border := 1 - t_border;

    for i := 0 to numLayers - 1 do
    begin
      t_bot    := t_top - dt;
      smax_top := 1 - t_top;
      smax_bot := 1 - t_bot;
      if i > 0 then
        ds_top := smax_top / i;
      ds_bot   := smax_bot / (i + 1);
      s_top    := 0;
      s_bot    := 0;
      for J    := 0 to i do
      begin
        ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i < caps);
        ProduceVertex(s_top, t_top, s_top * smax_border / smax_top, t_border, i < caps);
        s_top := s_top + ds_top;
        s_bot := s_bot + ds_bot;
      end;
      ProduceVertex(s_bot, t_bot, s_bot * smax_border / smax_bot, t_border, i < caps);
      FBuiltProperties.Manager.RestartStrip;
      t_top := t_bot;
    end;
  end;

begin
  level     := FLevel;
  numLayers := 1 shl level;
  dt        := 1 / numLayers;

  with FBuiltProperties.Manager do
  begin
    BeginObject(FBuiltProperties);
    Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, 0);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);

    offset := False;
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

    EndPrimitives;
    EndObject(rci);
  end;
  inherited;
end;

// Assign
//

procedure TDGLGeoSphere.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TDGLGeoSphere) then
  begin
    FRadius := TDGLGeoSphere(Source).FRadius;
    FLevel  := TDGLGeoSphere(Source).FLevel;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TDGLGeoSphere.AxisAlignedDimensionsUnscaled: TVector;
begin
  Result.X := Abs(FRadius);
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

// RayCastIntersect
//

function TDGLGeoSphere.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  i1, i2:                  TVector;
  localStart, localVector: TVector;
begin
  // compute coefficients of quartic polynomial
  SetVector(localStart, AbsoluteToLocal(rayStart));
  SetVector(localVector, AbsoluteToLocal(rayVector));
  NormalizeVector(localVector);
  if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1, i2) > 0 then
  begin
    Result := true;
    if Assigned(intersectPoint) then
      SetVector(intersectPoint^, LocalToAbsolute(i1));
    if Assigned(intersectNormal) then
    begin
      i1.W := 0; // vector transform
      SetVector(intersectNormal^, LocalToAbsolute(i1));
    end;
  end
  else
    Result := False;
end;

// GenerateSilhouette
//

function TDGLGeoSphere.GenerateSilhouette(const silhouetteParameters:TDGLSilhouetteParameters):TDGLSilhouette;
var
  i, J:              Integer;
  s, c, angleFactor: Single;
  sVec, tVec:        TAffineVector;
begin

  // determine a local orthonormal matrix, viewer-oriented
  sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
  if VectorLength(sVec) < 1E-3 then
    sVec := VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
  tVec   := VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
  NormalizeVector(sVec);
  NormalizeVector(tVec);
  // generate the silhouette (outline and capping)
  Result      :=TDGLSilhouette.Create;
  angleFactor := (2 * PI) / FLevel;
  for i       := 0 to FLevel - 1 do
  begin
//    SinCos(i * angleFactor, FRadius, s, c);
     SinCos(i * angleFactor, s, c);
    Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
    J := (i + 1) mod FLevel;
    Result.Indices.Add(i, J);
    if silhouetteParameters.CappingRequired then
      Result.CapIndices.Add(FLevel, i, J)
  end;
  if silhouetteParameters.CappingRequired then
    Result.Vertices.Add(NullHmgPoint);
end;

// SetRadius
//

procedure TDGLGeoSphere.SetRadius(const Value: TGLFloat);
begin

  if Value <> FRadius then
  begin
    FRadius := Value;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TDGLGeoSphere.SetSubdivisionLevel(Value: Integer);
begin

  if Value <> FLevel then
  begin
    if Value < 0 then
      Value := 0;
    if Value > 4 then
      Value := 4;
    FLevel  := Value;
    StructureChanged;
  end;
end;

// ------------------
// ------------------ TDGLDisk ------------------
// ------------------

// Create
//

constructor TDGLDisk.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius := 0.5;
  FInnerRadius := 0;
  FSlices      := 16;
  FLoops       := 2;
  FStartAngle  := 0;
  FSweepAngle  := 360;
end;

// BuildList
//

procedure TDGLDisk.BuildList(var rci: TRenderContextInfo);
var
  i, J:                           Integer;
  Astart, Astep, angle, Rstep, r: Single;
  s, c:                           Single;
begin
  Astart := DegToRad(FStartAngle);
  Astep  := DegToRad(FSweepAngle) / FSlices;
  Rstep  := (FOuterRadius - FInnerRadius) / FLoops;
  with FBuiltProperties.Manager do
  begin
    BeginObject(FBuiltProperties);
    Attribute3f(RenderingContext.GLStates.attrPosition, 0, 0, 0);
    Attribute3f(RenderingContext.GLStates.attrNormal, 0, 0, 1);
    Attribute2f(RenderingContext.GLStates.attrTexCoord0, 0, 0);
    BeginPrimitives(GLVBOM_TRIANGLE_STRIP);
    for J := 0 to FLoops - 1 do
    begin
      angle := Astart;
      for i := 0 to FSlices do
      begin
        SinCos(angle, s, c);
        Attribute2f(RenderingContext.GLStates.attrTexCoord0, i / FSlices, J / FLoops);
        r := FInnerRadius + J * Rstep;
        Attribute3f(RenderingContext.GLStates.attrPosition, c * r, s * r, 0);
        EmitVertex;
        Attribute2f(RenderingContext.GLStates.attrTexCoord0, i / FSlices, (J + 1) / FLoops);
        r := FInnerRadius + (J + 1) * Rstep;
        Attribute3f(RenderingContext.GLStates.attrPosition, c * r, s * r, 0);
        EmitVertex;
        angle := angle + Astep;
      end;
    end;
    EndPrimitives;
    EndObject(rci);
  end;
  inherited;
end;

// SetOuterRadius
//

procedure TDGLDisk.SetOuterRadius(const aValue: Single);
begin

  if aValue <> FOuterRadius then
  begin
    FOuterRadius := aValue;
    StructureChanged;
  end;
end;

// SetInnerRadius
//

procedure TDGLDisk.SetInnerRadius(const aValue: Single);
begin

  if aValue <> FInnerRadius then
  begin
    FInnerRadius := aValue;
    StructureChanged;
  end;
end;

// SetSlices
//

procedure TDGLDisk.SetSlices(aValue: Integer);
begin

  if aValue <> FSlices then
  begin
    FSlices := aValue;
    StructureChanged;
  end;
end;

// SetLoops
//

procedure TDGLDisk.SetLoops(aValue: Integer);
begin

  if aValue <> FLoops then
  begin
    FLoops := aValue;
    StructureChanged;
  end;
end;

// SetStartAngle
//

procedure TDGLDisk.SetStartAngle(const aValue: Single);
begin

  if aValue <> FStartAngle then
  begin
    FStartAngle := aValue;
    StructureChanged;
  end;
end;

// SetSweepAngle
//

procedure TDGLDisk.SetSweepAngle(const aValue: Single);
begin

  if aValue <> FSweepAngle then
  begin
    FSweepAngle := aValue;
    StructureChanged;
  end;
end;

// Assign
//

procedure TDGLDisk.Assign(Source: TPersistent);
begin

  if Assigned(Source) and (Source is TDGLDisk) then
  begin
    FOuterRadius := TDGLDisk(Source).FOuterRadius;
    FInnerRadius := TDGLDisk(Source).FInnerRadius;
    FSlices      := TDGLDisk(Source).FSlices;
    FLoops       := TDGLDisk(Source).FLoops;
    FStartAngle  := TDGLDisk(Source).FStartAngle;
    FSweepAngle  := TDGLDisk(Source).FSweepAngle;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//

function TDGLDisk.AxisAlignedDimensionsUnscaled: TVector;
var
  r: TGLFloat;
begin
  r      := Abs(FOuterRadius);
  Result := VectorMake(r, r, 0);
end;

// RayCastIntersect
//

function TDGLDisk.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  ip:                          TVector;
  d:                           Single;
  angle, beginAngle, endAngle: Single;
  localIntPoint:               TVector;
begin
  Result := False;
  if SweepAngle > 0 then
    if RayCastPlaneIntersect(rayStart, rayVector, AbsolutePosition, AbsoluteDirection, @ip) then
    begin
      if Assigned(intersectPoint) then
        SetVector(intersectPoint^, ip);
      localIntPoint := AbsoluteToLocal(ip);
      d             := VectorNorm(localIntPoint);
      if (d >= Sqr(InnerRadius)) and (d <= Sqr(OuterRadius)) then
      begin
        if SweepAngle >= 360 then
          Result := true
        else
        begin
          // arctan2 returns results between -pi and +pi, we want between 0 and 360
          angle := 180 / PI * arctan2(localIntPoint.X, localIntPoint.Y);
          if angle < 0 then
            angle := angle + 360;
          // we also want StartAngle and StartAngle+SweepAngle to be in this range
          beginAngle := Trunc(StartAngle) mod 360;
          endAngle   := Trunc(StartAngle + SweepAngle) mod 360;
          // If beginAngle>endAngle then area crosses the boundary from 360=>0 degrees
          // therefore have 2 valid regions  (beginAngle to 360) & (0 to endAngle)
          // otherwise just 1 valid region (beginAngle to endAngle)
          if beginAngle > endAngle then
          begin
            if (angle > beginAngle) or (angle < endAngle) then
              Result := true;
          end
          else if (angle > beginAngle) and (angle < endAngle) then
            Result := true;
        end;
      end;
    end;
  if Result = true then
    if Assigned(intersectNormal) then
      SetVector(intersectNormal^, AbsoluteUp);
end;


// ------------------
// ------------------ TDGLBilletMesh ------------------
// ------------------

// Create
//

constructor TDGLBilletMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBuiltProperties.Usage := buStream;
  FPrimitiveType         := bmpPoint;
  FVertexNumber          := 1;
  FIndexed               := False;
end;

// BuildList
//

procedure TDGLBilletMesh.BuildList(var rci: TRenderContextInfo);
const
  cPrimitives: array [TBilletMeshPrimitive] of TGLVBOMEnum = (GLVBOM_POINTS, GLVBOM_LINES, GLVBOM_TRIANGLES);
//var
//  i:         Integer;
//  AttrArray: TDGLSLAttributeArray;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FShader) then
  begin
    //if not FShader.ShaderModel.GetAttributes(AttrArray) then Exit;
    // Create empty graphic buffers
    with FBuiltProperties.Manager do
    begin
      BeginObject(FBuiltProperties);
//      for i := 0 to High(AttrArray) do
//        if AttrArray[i].ID > 0 then
//        begin
//          case AttrArray[i].DataType of
//            GLSLType1F:
//              Attribute1f(AttrArray[i], 0);
//            GLSLType2F:
//              Attribute2f(AttrArray[i], 0, 0);
//            GLSLType3F:
//              Attribute3f(AttrArray[i], 0, 0, 0);
//            GLSLType4F:
//              Attribute4f(AttrArray[i], 0, 0, 0, 0);
//            GLSLType1I:
//              Attribute1i(AttrArray[i], 0);
//            GLSLType2I:
//              Attribute2i(AttrArray[i], 0, 0);
//            GLSLType3I:
//              Attribute3i(AttrArray[i], 0, 0, 0);
//            GLSLType4I:
//              Attribute4i(AttrArray[i], 0, 0, 0, 0);
//            GLSLType4UB:
//              Attribute4ub(AttrArray[i], 0, 0, 0, 0);
//          end;
//        end;
      BeginPrimitives(cPrimitives[FPrimitiveType]);
      EmitVertices(FVertexNumber, FIndexed);
      // Call primitive factory
//      if Assigned(FFactory) then
//      begin
//        FFactory.Produce(Self, rci);
      RenderClient(FBuiltProperties, rci);
//      end;
    end;
  end;
  inherited;
end;

procedure TDGLBilletMesh.Assign(Source: TPersistent);
begin
  if Source is TDGLBilletMesh then
  begin
//    SetFactory(TDGLBilletMesh(Source).Factory);
  end;
  inherited;
end;

//procedure TDGLBilletMesh.SetFactory(Value: TDGLBaseFactory);
//begin
//  if Value <> FFactory then
//  begin
//    if Assigned(FFactory) then
//      FFactory.RemoveFreeNotification(Self);
//    FFactory := Value;
//    if Assigned(FFactory) then
//      FFactory.FreeNotification(Self);
//    StructureChanged;
//  end;
//end;

procedure TDGLBilletMesh.SetPrimitiveType(Value: TBilletMeshPrimitive);
begin
  if Value <> FPrimitiveType then
  begin
    FPrimitiveType := Value;
    StructureChanged;
  end;
end;

procedure TDGLBilletMesh.SetVertexNumber(Value: Integer);
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

procedure TDGLBilletMesh.SetIndexed(Value: Boolean);
begin
  if Value <> FIndexed then
  begin
    FIndexed := Value;
    StructureChanged;
  end;
end;

procedure TDGLBilletMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
//  if (Operation = opRemove) and (AComponent = FFactory) then
//    FFactory := nil;
  inherited;
end;

initialization

RegisterClasses([TDGLPlane]);
//TDGLSprite, TDGLCube, TDGLSphere, TDGLGeoSphere, TDGLDisk, TDGLBilletMesh]);

DefaultProgram := TDGLProgramHandle.Create;

finalization

DefaultProgram.Destroy;
DefaultProgram := nil;

end.
