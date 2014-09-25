{ : GLMisc<p>

  Miscellaneous support routines & classes.<p>

  <b>History : </b><font size=-1><ul>
  <li>14/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>09/03/07 - DaStr - Removed obsolete FPC IFDEF's
  (thanks Burkhard Carstens) (Bugtracker ID = 1678642)
  <li>29/01/07 - DaStr - TGLCustomCoordinates.SetVector - Added default value
  to one of the procedure's parameters
  Added TGLCustomCoordinates.AsPoint2D property
  <li>14/01/07 - DaStr - Added IGLCoordinatesUpdateAble
  (abstracted from TGLCoordinatesUpdateAbleComponent)
  TGLCoordinates.SetVector/SetPoint - fixed assertions
  and added descriptions for them (BugTrackerID=1588388)
  TGLCustomCoordinates abstracted
  TGLCoordinates2 added
  Added csPoint2D to TGLCoordinatesStyle
  <li>02/08/04 - LR, YHC - BCB corrections: use record instead array
  Replace direct access of some properties by
  a getter and a setter
  Added VectorTypes Unit
  <li>05/09/03 - EG - Some GLScene types and helper functions moved
  to new GLState and GLUtils units
  <li>10/25/03 - Dave - Added TGLCoordinates.SetVector (TAffineVector)
  Added TGLCoordinates.SetVector (TVector)
  Added TGLCoordinates.SetPoint (TAffineVector)
  Added TGLCoordinates.SetPoint (TVector)
  <li>05/09/03 - EG - TNotifyCollection moved in from GLMultiPolygon
  <li>21/08/03 - EG - Added osRenderNearestFirst
  <li>17/06/03 - EG - New TryStrToFloat, updated StrToFloatDef
  <li>05/06/03 - EG - TDataFile moved out to ApplicationFileIO,
  added Silhouette classes
  <li>13/03/03 - Dave - Added TGLCoordinates.SetToZero
  <li>23/10/02 - EG - Added ParseFloat
  <li>22/10/02 - EG - Added ParseInteger
  <li>03/07/02 - EG - Added TGLNodes.Normal
  <li>17/03/02 - EG - Added First/Last to TGLNodes
  <li>24/01/02 - EG - Added vUseDefaultSets mechanism
  <li>07/01/02 - EG - TGLNodes.Barycenter fix (thx Bob)
  <li>15/12/01 - EG - Added support for cube maps
  <li>14/09/01 - EG - Addition of vFileStreamClass
  <li>04/09/01 - EG - SetGLCurrentTexture stuff
  <li>18/07/01 - EG - Added TGLVisibilityCulling
  <li>08/07/01 - EG - Changes in TGLNodes based on code from Uwe Raabe
  <li>19/06/01 - EG - Added StrToFloatDef
  <li>16/03/01 - EG - Added Capabilities to TDataFile
  <li>21/02/01 - EG - Now XOpenGL based (multitexture)
  <li>05/02/01 - EG - Faster SetGLMaterialColors
  <li>15/01/01 - EG - Added SizeOfFile
  <li>04/01/00 - EG - Added AsAffineVector to TGLNode
  <li>22/12/00 - EG - Fixed TGLNodes.Vector when there is only one node
  <li>03/11/00 - EG - Added TGLCoordinates.AsAffineVector
  <li>08/10/00 - EG - Added "Style" to TGLCoordinates to detect some misuses
  <li>06/08/00 - EG - TGLCoordinates moved in, added TextureMatrix stuff,
  added TGLNodes.AddXYArc
  <li>19/07/00 - EG - Improvements to TGLNodes (tessellation, scaling...)
  <li>16/07/00 - EG - Added "Managers" support classes,
  Added TDataFile
  <li>11/07/00 - EG - Added 'Sender' to MotifyChange
  <li>05/07/00 - EG - Added Begin/EndUpdate to TGLNodes
  <li>23/06/00 - EG - Added Read/WriteCRLFString
  <li>18/06/00 - EG - Added update control to TGLUpdateAbleObject
  <li>09/06/00 - EG - Added TGLCadenceAbleComponent
  <li>07/06/00 - EG - Added RemoveFreeNotification for Delphi 4
  <li>29/05/00 - EG - Added TGLNode/TGLNodes
  <li>26/05/00 - EG - TMeshMode & TVertexMode moved in
  <li>22/03/00 - EG - Added SetGLState/UnSetGLState
  <li>21/03/00 - EG - Added SaveStringToFile/LoadStringFromFile
  <li>18/03/00 - EG - Added GetSqrt255Array
  <li>06/02/00 - EG - Javadocisation, RoundUpToPowerOf2,
  RoundDownToPowerOf2 and IsPowerOf2 moved in
  </ul></font>

  TODO : separating misc stuff from base classes and OpenGL support

}
unit GLMisc;

// GLMisc      - miscellaneous support routines
// version     - 0.1.0
// last change - 31. January 1999
// for more information see help file

interface

uses Classes, VectorGeometry, OpenGL1x, Spline, VectorLists, VectorTypes;

{$I GLScene.inc}

type
  TMeshMode = (MmTriangleStrip, MmTriangleFan, MmTriangles, MmQuadStrip,
    MmQuads, MmPolygon);
  TVertexMode = (VmV, VmVN, VmVNC, VmVNCT, VmVNT, VmVT);

const
  CMeshModeToGLEnum: array [Low(TMeshMode) .. High(TMeshMode)
    ] of TGLEnum = (GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
    GL_QUAD_STRIP, GL_QUADS, GL_POLYGON);
  CVertexModeToGLEnum: array [Low(TVertexMode) .. High(TVertexMode)
    ] of TGLEnum = (GL_V3F, GL_N3F_V3F, GL_C4F_N3F_V3F, GL_T2F_C4F_N3F_V3F,
    GL_T2F_N3F_V3F, GL_T2F_V3F);

type

  // TProgressTimes
  //
  TProgressTimes = record
    DeltaTime, NewTime: Double end;

    // TGLObjectsSorting
    //
    { : Determines if objects are sorted, and how.<p>
      Sorting is done level by level (and not for all entities), values are :<ul>
      <li>osInherited : use inherited sorting mode, defaults to osRenderFarthestFirst
      <li>osNone : do not sort objects.
      <li>osRenderFarthestFirst : render objects whose Position is the farthest from
      the camera first.
      <li>osRenderBlendedLast : opaque objects are not sorted and rendered
      first, blended ones are rendered afterwards and depth sorted.
      <li>osRenderNearestFirst : render objects whose Position is the nearest to
      the camera first.
      </ul> }
    TGLObjectsSorting = (OsInherited, OsNone, OsRenderFarthestFirst,
      OsRenderBlendedLast, OsRenderNearestFirst);

    // TGLVisibilityCulling
    //
    { : Determines the visibility culling mode.
      Culling is done level by level, allowed values are:<ul>
      <li>vcInherited : use inherited culling value, if selected for the root
      level, defaults to vcNone
      <li>vcNone : no visibility culling is performed
      <li>vcObjectBased : culling is done on a per-object basis, each object may
      or may not be culled base on its own AxisAlignedDimensions,
      culling has no impact on the visibility of its children
      <li>vcHierarchical : culling is performed hierarchically, using hierarchical
      bounding boxes, if a parent is culled, all of its children, whatever their
      culling options are invisible.
      <li><br>Depending on the structure of your scene the most efficient culling
      method will be either vcObjectBased or vcHierarchical. Also note that if
      you use many objects with "static" geometry and have a T&amp;L graphics
      board, it may be faster not to cull at all (ie. leave this to the hardware). }
    TGLVisibilityCulling = (VcInherited, VcNone, VcObjectBased, VcHierarchical);

    // TGLUpdateAbleObject
    //
    { : An abstract class describing the "update" interface.<p> }
    TGLUpdateAbleObject = class(TPersistent)private
    { Private Declarations }
      FOwner: TPersistent;
    FUpdating: Integer;
    FOnNotifyChange: TNotifyEvent;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); virtual;

    procedure NotifyChange(Sender: TObject); virtual;
    function GetOwner: TPersistent; override;

    property Updating: Integer read FUpdating;
    procedure BeginUpdate;
    procedure EndUpdate;

    property Owner: TPersistent read FOwner;
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange
      write FOnNotifyChange;
  end;

  // TGLCadenceAbleComponent
  //
  { : A base class describing the "cadenceing" interface.<p> }
  TGLCadenceAbleComponent = class(TComponent)
  public
    { Public Declarations }
{$IFNDEF GLS_DELPHI_5_UP}
    procedure RemoveFreeNotification(AComponent: TComponent);
{$ENDIF}
    procedure DoProgress(const ProgressTime: TProgressTimes); virtual;
  end;

  // TGLUpdateAbleComponent
  //
  { : A base class describing the "update" interface.<p> }
  TGLUpdateAbleComponent = class(TGLCadenceAbleComponent)
  public
    { Public Declarations }
    procedure NotifyChange(Sender: TObject); virtual;
  end;

  // TGLCoordinatesStyle
  //
  { : Identifie le type de données stockées au sein d'un TGLCustomCoordinates.<p>
    <ul><li>csPoint2D : a simple 2D point (Z=0, W=0)
    <ul><li>csPoint : un point (W=1)
    <li>csVector : un vecteur (W=0)
    <li>csUnknown : aucune contrainte
    </ul> }
  TGLCoordinatesStyle = (CsPoint2D, CsPoint, CsVector, CsUnknown);

  // TGLCustomCoordinates
  //
  { : Stores and homogenous vector.<p>
    This class is basicly a container for a TVector, allowing proper use of
    delphi property editors and editing in the IDE. Vector/Coordinates
    manipulation methods are only minimal.<br>
    Handles dynamic default values to save resource file space.<p> }
  TGLCustomCoordinates = class(TGLUpdateAbleObject)
  private
    { Private Declarations }
    FCoords: TVector;
    FStyle: TGLCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PVector;
    procedure SetAsPoint2D(const Value: TVector2f);
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    function GetAsPoint2D: TVector2f;
    procedure SetCoordinate(Index: Integer; const AValue: TGLFloat);
    function GetCoordinate(const Index: Integer): TGLFloat;
    function GetDirectCoordinate(const Index: Integer): TGLFloat;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: TGLFloat);
    function GetAsString: String;
  protected
    { Protected Declarations }
    procedure SetDirectVector(const V: TVector);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    { Public Declarations }
    constructor CreateInitialized(AOwner: TPersistent; const AValue: TVector;
      const AStyle: TGLCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);

    procedure Initialize(const Value: TVector);
    procedure NotifyChange(Sender: TObject); override;

    { : Identifies the coordinates styles.<p>
      The property is NOT persistent, csUnknown by default, and should be
      managed by owner object only (internally).<p>
      It is used by the TGLCustomCoordinates for internal "assertion" checks
      to detect "misuses" or "misunderstandings" of what the homogeneous
      coordinates system implies. }
    property Style: TGLCoordinatesStyle read FStyle write FStyle;

    procedure Translate(const TranslationVector: TVector); overload;
    procedure Translate(const TranslationVector: TAffineVector); overload;
    procedure AddScaledVector(const Factor: Single;
      const TranslationVector: TVector); overload;
    procedure AddScaledVector(const Factor: Single;
      const TranslationVector: TAffineVector); overload;
    procedure Rotate(const AnAxis: TAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TVector; AnAngle: Single); overload;
    procedure Normalize;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: TGLFloat;
    function VectorNorm: TGLFloat;
    function MaxXYZ: Single;
    function Equals(const AVector: TVector): Boolean;

    procedure SetVector(const X, Y: Single; Z: Single = 0); overload;
    procedure SetVector(const X, Y, Z, W: Single); overload;
    procedure SetVector(const V: TAffineVector); overload;
    procedure SetVector(const V: TVector); overload;

    procedure SetPoint(const X, Y, Z: Single); overload;
    procedure SetPoint(const V: TAffineVector); overload;
    procedure SetPoint(const V: TVector); overload;

    procedure SetPoint2D(const X, Y: Single); overload;
    procedure SetPoint2D(const V: TAffineVector); overload;
    procedure SetPoint2D(const V: TVector); overload;
    procedure SetPoint2D(const V: TVector2f); overload;

    procedure SetToZero;
    function AsAddress: PGLFloat;

    { : The coordinates viewed as a vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsVector: TVector read FCoords write SetAsVector;

    { : The coordinates viewed as an affine vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.<br>
      The W component is automatically adjustes depending on style. }
    property AsAffineVector: TAffineVector read GetAsAffineVector
      write SetAsAffineVector;

    { : The coordinates viewed as a 2D point.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsPoint2D: TVector2f read GetAsPoint2D write SetAsPoint2D;

    property X: TGLFloat index 0 read GetCoordinate write SetCoordinate;
    property Y: TGLFloat index 1 read GetCoordinate write SetCoordinate;
    property Z: TGLFloat index 2 read GetCoordinate write SetCoordinate;
    property W: TGLFloat index 3 read GetCoordinate write SetCoordinate;

    { : The coordinates, in-between brackets, separated by semi-colons. }
    property AsString: String read GetAsString;

    // : Similar to AsVector but does not trigger notification events
    property DirectVector: TVector read FCoords write SetDirectVector;
    property DirectX: TGLFloat index 0 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectY: TGLFloat index 1 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectZ: TGLFloat index 2 read GetDirectCoordinate
      write SetDirectCoordinate;
    property DirectW: TGLFloat index 3 read GetDirectCoordinate
      write SetDirectCoordinate;
  end;

  { : A TGLCustomCoordinates that publishes X, Y properties. }
  TGLCoordinates2 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  { : A TGLCustomCoordinates that publishes X, Y, Z properties. }
  TGLCoordinates3 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  // TGLCoordinates4
  //
  { : A TGLCustomCoordinates that publishes X, Y, Z, W properties. }
  TGLCoordinates4 = class(TGLCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property W stored False;
  end;

  // TGLCoordinates
  //
  TGLCoordinates = TGLCoordinates3;

  // Actually Sender should be TGLCustomCoordinates, but that would require
  // changes in a some other GLScene units and some other projects that use
  // TGLCoordinatesUpdateAbleComponent
  IGLCoordinatesUpdateAble = interface
    ['{ACB98D20-8905-43A7-AFA5-225CF5FA6FF5}']
    procedure CoordinateChanged(Sender: TGLCoordinates);
  end;

  // TGLCoordinatesUpdateAbleComponent
  //
  TGLCoordinatesUpdateAbleComponent = class(TGLUpdateAbleComponent,
    IGLCoordinatesUpdateAble)
  public
    { Public Declarations }
    procedure CoordinateChanged(Sender: TGLCoordinates); virtual; abstract;
  end;

  // TGLNode
  //
  TGLNode = class(TCollectionItem)
  private
    { Private Declarations }
    FCoords: TVector;
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    procedure SetCoordinate(Index: Integer; AValue: TGLFloat);
    function GetCoordinate(const Index: Integer): TGLFloat;

  protected
    { Protected Declarations }
    function StoreCoordinate(Index: Integer): Boolean;

    function GetDisplayName: String; override;

  public
    { Public Declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function AsAddress: PGLFloat;
    { : The coordinates viewed as a vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. }
    property AsVector: TVector read FCoords write SetAsVector;
    { : The coordinates viewed as an affine vector.<p>
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.<br>
      The W component is automatically adjustes depending on style. }
    property AsAffineVector: TAffineVector read GetAsAffineVector
      write SetAsAffineVector;

    property W: TGLFloat index 3 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;

  published
    { Published Declarations }
    property X: TGLFloat index 0 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Y: TGLFloat index 1 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Z: TGLFloat index 2 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
  end;

  // TGLNodes
  //
  TGLNodes = class(TOwnedCollection)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure SetItems(Index: Integer; const Val: TGLNode);
    function GetItems(Index: Integer): TGLNode;
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent;
      ItemClass: TCollectionItemClass = nil);
    function CreateCopy(AOwner: TPersistent): TGLNodes;

    function Add: TGLNode;
    function FindItemID(ID: Integer): TGLNode;
    property Items[index: Integer]: TGLNode read GetItems
      write SetItems; default;
    function First: TGLNode;
    function Last: TGLNode;

    procedure NotifyChange; virtual;
    procedure EndUpdate; override;

    procedure AddNode(const Coords: TGLCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: TGLfloat); overload;
    procedure AddNode(const Value: TVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
    procedure AddXYArc(XRadius, YRadius: Single; StartAngle, StopAngle: Single;
      NbSegments: Integer; const Center: TAffineVector);

    // : Calculates and returns the barycenter of the nodes
    function Barycenter: TAffineVector;
    { : Computes normal based on the 1st three nodes.<p>
      Returns NullVector if there are less than 3 nodes. }
    function Normal: TAffineVector;
    // : Returns normalized vector Nodes[i+1]-Nodes[i]
    function Vector(I: Integer): TAffineVector;

    { : Calculates the extents of the nodes (min-max for all coordinates).<p>
      The returned values are also the two corners of the axis-aligned
      bounding box. }
    procedure GetExtents(var Min, Max: TAffineVector);
    // : Translate all nodes
    procedure Translate(const Tv: TAffineVector);
    // : Scale all node coordinates
    procedure Scale(const Fv: TAffineVector); overload;
    // : Scale all node coordinates
    procedure Scale(F: Single); overload;
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundX(Angle: Single);
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundY(Angle: Single);
    // : Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundZ(Angle: Single);

    procedure RenderTesselatedPolygon(Textured: Boolean;
      Normal: PAffineVector = nil; SplineDivisions: Integer = 1;
      InvertNormals: Boolean = False);

    function CreateNewCubicSpline: TCubicSpline;

  end;

  TGLNodesClass = class of TGLNodes;

  // TNotifyCollection
  //
  TNotifyCollection = class(TOwnedCollection)
  private
    { Private Declarations }
    FOnNotifyChange: TNotifyEvent;

  protected
    { Protected Declarations }
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property OnNotifyChange: TNotifyEvent read FOnNotifyChange
      write FOnNotifyChange;
  end;

procedure RegisterManager(AManager: TComponent);
procedure DeRegisterManager(AManager: TComponent);
function FindManager(ClassType: TComponentClass; const ManagerName: String)
  : TComponent;

var
  // Specifies if TGLCustomCoordinates, TGLColor, etc. should allocate memory for
  // their default values (ie. design-time) or not (run-time)
  VUseDefaultSets: Boolean = False;

  // ------------------------------------------------------
  // ------------------------------------------------------
  // ------------------------------------------------------
implementation

// ------------------------------------------------------
// ------------------------------------------------------
// ------------------------------------------------------

uses SysUtils, XOpenGL;

const
  CsVectorHelp =
    'If you are getting assertions here, consider using the SetPoint procedure';
  CsPointHelp =
    'If you are getting assertions here, consider using the SetVector procedure';
  CsPoint2DHelp =
    'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';

var
  VManagers: TList;

  // RegisterManager
  //
procedure RegisterManager(AManager: TComponent);
begin
  if not Assigned(VManagers) then
    VManagers := TList.Create;
  if VManagers.IndexOf(AManager) < 0 then
    VManagers.Add(AManager);
end;

// DeRegisterManager
//
procedure DeRegisterManager(AManager: TComponent);
begin
  if Assigned(VManagers) then
    VManagers.Remove(AManager);
end;

// FindManager
//
function FindManager(ClassType: TComponentClass; const ManagerName: String)
  : TComponent;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(VManagers) then
    for I := 0 to VManagers.Count - 1 do
      with TComponent(VManagers[I]) do
        if InheritsFrom(ClassType) and (Name = ManagerName) then
        begin
          Result := TComponent(VManagers[I]);
          Break;
        end;
end;

// ---------------------- TGLUpdateAbleObject -----------------------------------------

// Create
//
constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

// NotifyChange
//
procedure TGLUpdateAbleObject.NotifyChange(Sender: TObject);
begin
  if (FUpdating = 0) and Assigned(Owner) then
  begin
    if Owner is TGLUpdateAbleObject then
      TGLUpdateAbleObject(Owner).NotifyChange(Self)
    else if Owner is TGLUpdateAbleComponent then
      TGLUpdateAbleComponent(Owner).NotifyChange(Self);
    if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
  end;
end;

// GetOwner
//
function TGLUpdateAbleObject.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// BeginUpdate
//
procedure TGLUpdateAbleObject.BeginUpdate;
begin
  Inc(FUpdating);
end;

// EndUpdate
//
procedure TGLUpdateAbleObject.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating <= 0 then
  begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  end;
end;

// ------------------
// ------------------ TGLCadenceAbleComponent ------------------
// ------------------

{$IFNDEF GLS_DELPHI_5_UP}

// RemoveFreeNotification
//
procedure TGLCadenceAbleComponent.RemoveFreeNotification
  (AComponent: TComponent);
begin
  Notification(AComponent, OpRemove);
end;
{$ENDIF}

// DoProgress
//
procedure TGLCadenceAbleComponent.DoProgress(const ProgressTime
  : TProgressTimes);
begin
  // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

// NotifyChange
//
procedure TGLUpdateAbleComponent.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if (Owner is TGLUpdateAbleComponent) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TGLCustomCoordinates ------------------
// ------------------

// CreateInitialized
//
constructor TGLCustomCoordinates.CreateInitialized(AOwner: TPersistent;
  const AValue: TVector; const AStyle: TGLCoordinatesStyle = CsUnknown);
begin
  Create(AOwner);
  Initialize(AValue);
  FStyle := AStyle;
end;

// Destroy
//
destructor TGLCustomCoordinates.Destroy;
begin
  if Assigned(FPDefaultCoords) then
    Dispose(FPDefaultCoords);
  inherited;
end;

// Initialize
//
procedure TGLCustomCoordinates.Initialize(const Value: TVector);
begin
  FCoords := Value;
  if VUseDefaultSets then
  begin
    if not Assigned(FPDefaultCoords) then
      New(FPDefaultCoords);
    FPDefaultCoords^ := Value;
  end;
end;

// Assign
//
procedure TGLCustomCoordinates.Assign(Source: TPersistent);
begin
  if Source is TGLCustomCoordinates then
    FCoords := TGLCustomCoordinates(Source).FCoords
  else
    inherited;
end;

// WriteToFiler
//
procedure TGLCustomCoordinates.WriteToFiler(Writer: TWriter);
var
  WriteCoords: Boolean;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    if VUseDefaultSets then
      WriteCoords := not VectorEquals(FPDefaultCoords^, FCoords)
    else
      WriteCoords := True;
    WriteBoolean(WriteCoords);
    if WriteCoords then
      Write(FCoords.Coord[0], SizeOf(FCoords));
  end;
end;

// ReadFromFiler
//
procedure TGLCustomCoordinates.ReadFromFiler(Reader: TReader);
var
  N: Integer;
begin
  with Reader do
  begin
    ReadInteger; // Ignore ArchiveVersion
    if ReadBoolean then
    begin
      N := SizeOf(FCoords);
      Assert(N = 4 * SizeOf(Single));
      Read(FCoords.Coord[0], N);
    end
    else if Assigned(FPDefaultCoords) then
      FCoords := FPDefaultCoords^;
  end;
end;

// DefineProperties
//
procedure TGLCustomCoordinates.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Coordinates', ReadData, WriteData,
    not(Assigned(FPDefaultCoords) and VectorEquals(FPDefaultCoords^, FCoords)));
end;

// ReadData
//
procedure TGLCustomCoordinates.ReadData(Stream: TStream);
begin
  Stream.Read(FCoords, SizeOf(FCoords));
end;

// WriteData
//
procedure TGLCustomCoordinates.WriteData(Stream: TStream);
begin
  Stream.Write(FCoords, SizeOf(FCoords));
end;

// NotifyChange
//
procedure TGLCustomCoordinates.NotifyChange(Sender: TObject);
var
  Int: IGLCoordinatesUpdateAble;
begin
  if Supports(Owner, IGLCoordinatesUpdateAble, Int) then
    Int.CoordinateChanged(TGLCoordinates(Self))
  else
    inherited NotifyChange(Sender);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const TranslationVector: TVector);
begin
  FCoords.Coord[0] := FCoords.Coord[0] + TranslationVector.Coord[0];
  FCoords.Coord[1] := FCoords.Coord[1] + TranslationVector.Coord[1];
  FCoords.Coord[2] := FCoords.Coord[2] + TranslationVector.Coord[2];
  NotifyChange(Self);
end;

// Translate
//
procedure TGLCustomCoordinates.Translate(const TranslationVector
  : TAffineVector);
begin
  FCoords.Coord[0] := FCoords.Coord[0] + TranslationVector.Coord[0];
  FCoords.Coord[1] := FCoords.Coord[1] + TranslationVector.Coord[1];
  FCoords.Coord[2] := FCoords.Coord[2] + TranslationVector.Coord[2];
  NotifyChange(Self);
end;

// AddScaledVector (hmg)
//
procedure TGLCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// AddScaledVector (affine)
//
procedure TGLCustomCoordinates.AddScaledVector(const Factor: Single;
  const TranslationVector: TAffineVector);
var
  F: Single;
begin
  F := Factor;
  CombineVector(FCoords, TranslationVector, F);
  NotifyChange(Self);
end;

// Rotate (affine)
//
procedure TGLCustomCoordinates.Rotate(const AnAxis: TAffineVector;
  AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Rotate (hmg)
//
procedure TGLCustomCoordinates.Rotate(const AnAxis: TVector; AnAngle: Single);
begin
  RotateVector(FCoords, AnAxis, AnAngle);
  NotifyChange(Self);
end;

// Normalize
//
procedure TGLCustomCoordinates.Normalize;
begin
  NormalizeVector(FCoords);
  NotifyChange(Self);
end;

// Invert
//
procedure TGLCustomCoordinates.Invert;
begin
  NegateVector(FCoords);
  NotifyChange(Self);
end;

// Scale
//
procedure TGLCustomCoordinates.Scale(Factor: Single);
begin
  ScaleVector(PAffineVector(@FCoords)^, Factor);
  NotifyChange(Self);
end;

// VectorLength
//
function TGLCustomCoordinates.VectorLength: TGLFloat;
begin
  Result := VectorGeometry.VectorLength(FCoords);
end;

// VectorNorm
//
function TGLCustomCoordinates.VectorNorm: TGLFloat;
begin
  Result := VectorGeometry.VectorNorm(FCoords);
end;

// MaxXYZ
//
function TGLCustomCoordinates.MaxXYZ: Single;
begin
  Result := VectorGeometry.MaxXYZComponent(FCoords);
end;

// Equals
//
function TGLCustomCoordinates.Equals(const AVector: TVector): Boolean;
begin
  Result := VectorEquals(FCoords, AVector);
end;

// SetVector (affine)
//
procedure TGLCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VectorGeometry.SetVector(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetVector (TAffineVector)
//
procedure TGLCustomCoordinates.SetVector(const V: TAffineVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (TVector)
//
procedure TGLCustomCoordinates.SetVector(const V: TVector);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VectorGeometry.SetVector(FCoords, V);
  NotifyChange(Self);
end;

// SetVector (hmg)
//
procedure TGLCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  Assert(FStyle = CsVector, CsVectorHelp);
  VectorGeometry.SetVector(FCoords, X, Y, Z, W);
  NotifyChange(Self);
end;

// SetDirectVector
//
procedure TGLCustomCoordinates.SetDirectVector(const V: TVector);
begin
  FCoords.Coord[0] := V.Coord[0];
  FCoords.Coord[1] := V.Coord[1];
  FCoords.Coord[2] := V.Coord[2];
  FCoords.Coord[3] := V.Coord[3];
end;

// SetToZero
//
procedure TGLCustomCoordinates.SetToZero;
begin
  FCoords.Coord[0] := 0;
  FCoords.Coord[1] := 0;
  FCoords.Coord[2] := 0;
  if FStyle = CsPoint then
    FCoords.Coord[3] := 1
  else
    FCoords.Coord[3] := 0;
  NotifyChange(Self);
end;

// SetPoint
//
procedure TGLCustomCoordinates.SetPoint(const X, Y, Z: Single);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VectorGeometry.MakePoint(FCoords, X, Y, Z);
  NotifyChange(Self);
end;

// SetPoint (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint(const V: TAffineVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VectorGeometry.MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint (TVector)
//
procedure TGLCustomCoordinates.SetPoint(const V: TVector);
begin
  Assert(FStyle = CsPoint, CsPointHelp);
  VectorGeometry.MakePoint(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint2D
//
procedure TGLCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, X, Y, 0);
  NotifyChange(Self);
end;

// SetPoint2D (TAffineVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const V: TAffineVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint2D (TVector)
//
procedure TGLCustomCoordinates.SetPoint2D(const V: TVector);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, V);
  NotifyChange(Self);
end;

// SetPoint2D (TVector2f)
//
procedure TGLCustomCoordinates.SetPoint2D(const V: TVector2f);
begin
  Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  VectorGeometry.MakeVector(FCoords, V.Coord[0], V.Coord[1], 0);
  NotifyChange(Self);
end;

// AsAddress
//
function TGLCustomCoordinates.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//
procedure TGLCustomCoordinates.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  case FStyle of
    CsPoint2D:
      begin
        FCoords.Coord[2] := 0;
        FCoords.Coord[3] := 0;
      end;
    CsPoint:
      FCoords.Coord[3] := 1;
    CsVector:
      FCoords.Coord[3] := 0;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// SetAsAffineVector
//
procedure TGLCustomCoordinates.SetAsAffineVector(const Value: TAffineVector);
begin
  case FStyle of
    CsPoint2D:
      MakeVector(FCoords, Value);
    CsPoint:
      MakePoint(FCoords, Value);
    CsVector:
      MakeVector(FCoords, Value);
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// SetAsPoint2D
//
procedure TGLCustomCoordinates.SetAsPoint2D(const Value: TVector2f);
begin
  case FStyle of
    CsPoint2D, CsPoint, CsVector:
      begin
        FCoords.Coord[0] := Value.Coord[0];
        FCoords.Coord[1] := Value.Coord[1];
        FCoords.Coord[2] := 0;
        FCoords.Coord[3] := 0;
      end;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

// GetAsAffineVector
//
function TGLCustomCoordinates.GetAsAffineVector: TAffineVector;
begin
  VectorGeometry.SetVector(Result, FCoords);
end;

// GetAsPoint2D
//
function TGLCustomCoordinates.GetAsPoint2D: TVector2f;
begin
  Result.Coord[0] := FCoords.Coord[0];
  Result.Coord[1] := FCoords.Coord[1];
end;

// SetCoordinate
//
procedure TGLCustomCoordinates.SetCoordinate(Index: Integer;
  const AValue: TGLFloat);
begin
  FCoords.Coord[index] := AValue;
  NotifyChange(Self);
end;

function TGLCustomCoordinates.GetCoordinate(const Index: Integer): TGLFloat;
begin
  Result := FCoords.Coord[index];
end;

function TGLCustomCoordinates.GetDirectCoordinate(const Index: Integer)
  : TGLFloat;
begin
  Result := FCoords.Coord[index]
end;

procedure TGLCustomCoordinates.SetDirectCoordinate(const Index: Integer;
  const AValue: TGLFloat);
begin
  FCoords.Coord[index] := AValue;
end;

// GetAsString
//
function TGLCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.Coord[0], FCoords.Coord[1]]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.Coord[0], FCoords.Coord[1],
        FCoords.Coord[2]]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.Coord[0], FCoords.Coord[1],
        FCoords.Coord[2], FCoords.Coord[3]]);
  else
    Assert(False);
  end;
end;

// ------------------
// ------------------ TGLNode ------------------
// ------------------

// Create
//
constructor TGLNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  // nothing, yet
end;

// Destroy
//
destructor TGLNode.Destroy;
begin
  // nothing, yet
  inherited Destroy;
end;

// Assign
//
procedure TGLNode.Assign(Source: TPersistent);
begin
  if Source is TGLNode then
  begin
    FCoords := TGLNode(Source).FCoords;
  end
  else
    inherited;
end;

// GetDisplayName
//
function TGLNode.GetDisplayName: String;
begin
  Result := Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//
function TGLNode.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//
procedure TGLNode.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  (Collection as TGLNodes).NotifyChange;
end;

// SetAsAffineVector
//
procedure TGLNode.SetAsAffineVector(const Value: TAffineVector);
begin
  VectorGeometry.SetVector(FCoords, Value);
  (Collection as TGLNodes).NotifyChange;
end;

// GetAsAffineVector
//
function TGLNode.GetAsAffineVector: TAffineVector;
begin
  VectorGeometry.SetVector(Result, FCoords);
end;

// SetCoordinate
//
procedure TGLNode.SetCoordinate(Index: Integer; AValue: TGLFloat);
begin
  FCoords.Coord[Index] := AValue;
  (Collection as TGLNodes).NotifyChange;
end;

function TGLNode.GetCoordinate(const Index: Integer): TGLFloat;
begin
  Result := FCoords.Coord[Index];
end;

// StoreCoordinate
//
function TGLNode.StoreCoordinate(Index: Integer): Boolean;
begin
  Result := (FCoords.Coord[Index] <> 0);
end;

// ------------------
// ------------------ TGLNodes ------------------
// ------------------

// Create
//
constructor TGLNodes.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass = nil);
begin
  if not Assigned(ItemClass) then
    inherited Create(AOwner, TGLNode)
  else
    inherited Create(AOwner, ItemClass);
end;

// CreateCopy
//
function TGLNodes.CreateCopy(AOwner: TPersistent): TGLNodes;
begin
  if Self <> nil then
  begin
    Result := TGLNodesClass(Self.ClassType).Create(AOwner);
    Result.Assign(Self);
  end
  else
    Result := nil;
end;

// SetItems
//
procedure TGLNodes.SetItems(Index: Integer; const Val: TGLNode);
begin
  inherited Items[index] := Val;
end;

// GetItems
//
function TGLNodes.GetItems(Index: Integer): TGLNode;
begin
  Result := TGLNode(inherited Items[index]);
end;

// First
//
function TGLNodes.First: TGLNode;
begin
  if Count > 0 then
    Result := TGLNode(inherited Items[0])
  else
    Result := nil;
end;

// Last
//
function TGLNodes.Last: TGLNode;
var
  N: Integer;
begin
  N := Count - 1;
  if N >= 0 then
    Result := TGLNode(inherited Items[N])
  else
    Result := nil;
end;

// Update
//
procedure TGLNodes.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// Add
//
function TGLNodes.Add: TGLNode;
begin
  Result := (inherited Add) as TGLNode;
end;

// FindItemID
//
function TGLNodes.FindItemID(ID: Integer): TGLNode;
begin
  Result := (inherited FindItemID(ID)) as TGLNode;
end;

// NotifyChange
//
procedure TGLNodes.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and
    (GetOwner is TGLUpdateAbleComponent) then
    TGLUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//
procedure TGLNodes.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;

// AddNode (TGLCustomCoordinates)
//
procedure TGLNodes.AddNode(const Coords: TGLCustomCoordinates);
begin
  Add.AsVector := Coords.AsVector;
end;

// AddNode (floats)
//
procedure TGLNodes.AddNode(const X, Y, Z: Single);
begin
  Add.AsVector := PointMake(X, Y, Z);
end;

// AddNode (TVector)
//
procedure TGLNodes.AddNode(const Value: TVector);
begin
  Add.AsVector := Value;
end;

// AddNode (TAffineVector)
//
procedure TGLNodes.AddNode(const Value: TAffineVector);
begin
  Add.AsAffineVector := Value;
end;

// AddXYArc
//
procedure TGLNodes.AddXYArc(XRadius, YRadius: Single;
  StartAngle, StopAngle: Single; NbSegments: Integer;
  const Center: TAffineVector);
var
  I: Integer;
  F: Single;
  S, C: Single;
begin
  BeginUpdate;
  try
    StartAngle := DegToRad(StartAngle);
    StopAngle := DegToRad(StopAngle);
    F := (StopAngle - StartAngle) / NbSegments;
    for I := 0 to NbSegments do
    begin
      SinCos(I * F + StartAngle, S, C);
      SetVector(Add.FCoords, Center.Coord[0] + XRadius * C,
        Center.Coord[1] + YRadius * S, Center.Coord[2], 1);
    end;
  finally
    EndUpdate;
  end;
end;

// Barycenter
//
function TGLNodes.Barycenter: TAffineVector;
var
  I: Integer;
begin
  Result := NullVector;
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
      AddVector(Result, PAffineVector(Items[I].AsAddress)^);
    ScaleVector(Result, 1.0 / Count);
  end;
end;

// Normal
//
function TGLNodes.Normal: TAffineVector;
begin
  if Count >= 3 then
    CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords,
      Items[2].FCoords, Result)
  else
    Result := NullVector;
end;

// Vector
//
function TGLNodes.Vector(I: Integer): TAffineVector;

procedure CalcUsingPrev; forward;

  procedure CalcUsingNext;
  begin
    if I < Count - 1 then
      VectorSubtract(Items[I].AsVector, Items[I + 1].AsVector, Result)
    else
      CalcUsingPrev;
  end;

  procedure CalcUsingPrev;
  begin
    if I > 0 then
      VectorSubtract(Items[I - 1].AsVector, Items[I].AsVector, Result)
    else
      CalcUsingNext;
  end;

begin
  Assert((I >= 0) and (I < Count));
  if I = 0 then
    if I = Count - 1 then
      SetVector(Result, NullVector)
    else
      VectorSubtract(Items[I + 1].AsVector, Items[I].AsVector, Result)
  else if I = Count - 1 then
    VectorSubtract(Items[I].AsVector, Items[I - 1].AsVector, Result)
  else
    VectorSubtract(Items[I + 1].AsVector, Items[I - 1].AsVector, Result);
  if VectorNorm(Result) < 1E-5 then
    SetVector(Result, NullVector)
  else
    NormalizeVector(Result);
end;

// GetExtents
//
procedure TGLNodes.GetExtents(var Min, Max: TAffineVector);
var
  I, K: Integer;
  F: Single;
const
  CBigValue: Single = 1E50;
  CSmallValue: Single = -1E50;
begin
  SetVector(Min, CBigValue, CBigValue, CBigValue);
  SetVector(Max, CSmallValue, CSmallValue, CSmallValue);
  for I := 0 to Count - 1 do
  begin
    for K := 0 to 2 do
    begin
      F := PAffineVector(Items[I].AsAddress)^.Coord[K];
      if F < Min.Coord[K] then
        Min.Coord[K] := F;
      if F > Max.Coord[K] then
        Max.Coord[K] := F;
    end;
  end;
end;

// Translate
//
procedure TGLNodes.Translate(const Tv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(Items[I].AsAddress)^, Tv);
  NotifyChange;
end;

// Scale (vector)
//
procedure TGLNodes.Scale(const Fv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, Fv);
  NotifyChange;
end;

// Scale (single)
//
procedure TGLNodes.Scale(F: Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, F);
  NotifyChange;
end;

// RotateAroundX
//
procedure TGLNodes.RotateAroundX(Angle: Single);
var
  I: Integer;
  C, S, V2: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V2 := V^.Coord[2];
    V^.Coord[1] := C * V^.Coord[1] + S * V2;
    V^.Coord[2] := C * V2 - S * V^.Coord[1];
  end;
  NotifyChange;
end;

// RotateAroundY
//
procedure TGLNodes.RotateAroundY(Angle: Single);
var
  I: Integer;
  C, S, V0: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V0 := V^.Coord[0];
    V^.Coord[0] := C * V0 + S * V^.Coord[2];
    V^.Coord[2] := C * V^.Coord[2] - S * V0;
  end;
  NotifyChange;
end;

// RotateAroundZ
//
procedure TGLNodes.RotateAroundZ(Angle: Single);
var
  I: Integer;
  C, S, V1: Single;
  V: PAffineVector;
begin
  SinCos(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V1 := V^.Coord[1];
    V^.Coord[1] := C * V1 + S * V^.Coord[0];
    V^.Coord[0] := C * V^.Coord[0] - S * V1;
  end;
  NotifyChange;
end;

// CreateNewCubicSpline
//
function TGLNodes.CreateNewCubicSpline: TCubicSpline;
var
  I: Integer;
  Xa, Ya, Za: PFloatArray;
begin
  GetMem(Xa, SizeOf(TGLFloat) * Count);
  GetMem(Ya, SizeOf(TGLFloat) * Count);
  GetMem(Za, SizeOf(TGLFloat) * Count);
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      Xa^[I] := X;
      Ya^[I] := Y;
      Za^[I] := Z;
    end;
  Result := TCubicSpline.Create(Xa, Ya, Za, nil, Count);
  FreeMem(Xa);
  FreeMem(Ya);
  FreeMem(Za);
end;

// RenderTesselatedPolygon
//
var
  NbExtraVertices: Integer;
  NewVertices: PAffineVectorArray;

procedure TGLNodes.RenderTesselatedPolygon(Textured: Boolean;
  Normal: PAffineVector = nil; SplineDivisions: Integer = 1;
  InvertNormals: Boolean = False);
var
  I: Integer;
  Tess: PGLUTesselator;
  DblVector: TAffineDblVector;
  Spline: TCubicSpline;
  SplinePos: PAffineVector;
  F: Single;

  function AllocNewVertex: PAffineVector;
  begin
    Inc(NbExtraVertices);
    Result := @NewVertices[NbExtraVertices - 1];
  end;

  procedure TessError(Errno: TGLEnum); stdcall;
  begin
    Assert(False, IntToStr(Errno) + ': ' + GluErrorString(Errno));
  end;

  procedure TessIssueVertex(VertexData: Pointer); stdcall;
  begin
    XglTexCoord2fv(VertexData);
    GlVertex3fv(VertexData);
  end;

  procedure TessCombine(Coords: PDoubleVector; Vertex_data: Pointer;
    Weight: PGLFloat; var OutData: Pointer); stdcall;
  begin
    OutData := AllocNewVertex;
    SetVector(PAffineVector(OutData)^, Coords^[0], Coords^[1], Coords^[2]);
  end;

begin
  if Count > 2 then
  begin
    // Create and initialize the GLU tesselator
    Tess := GluNewTess;
    GluTessCallback(Tess, GLU_TESS_BEGIN, @GlBegin);
    if Textured then
      GluTessCallback(Tess, GLU_TESS_VERTEX, @TessIssueVertex)
    else
      GluTessCallback(Tess, GLU_TESS_VERTEX, @GlVertex3fv);
    GluTessCallback(Tess, GLU_TESS_END, @GlEnd);
    GluTessCallback(Tess, GLU_TESS_ERROR, @TessError);
    GluTessCallback(Tess, GLU_TESS_COMBINE, @TessCombine);
    NbExtraVertices := 0;
    // Issue normal
    if Assigned(Normal) then
    begin
      GlNormal3fv(PGLFloat(Normal));
      GluTessNormal(Tess, Normal^.Coord[0], Normal^.Coord[1], Normal^.Coord[2]);
    end;
    // Issue polygon
    GluTessBeginPolygon(Tess, nil);
    GluTessBeginContour(Tess);
    if SplineDivisions <= 1 then
    begin
      // no spline, use direct coordinates
      GetMem(NewVertices, Count * SizeOf(TAffineVector));
      if InvertNormals then
      begin
        for I := Count - 1 downto 0 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          GluTessVertex(Tess, DblVector, Items[I].AsAddress);
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          GluTessVertex(Tess, DblVector, Items[I].AsAddress);
        end;
      end;
    end
    else
    begin
      // cubic spline
      GetMem(NewVertices, 2 * SplineDivisions * Count * SizeOf(TAffineVector));
      Spline := CreateNewCubicSpline;
      F := 1.0 / SplineDivisions;
      if InvertNormals then
      begin
        for I := SplineDivisions * (Count - 1) downto 0 do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          GluTessVertex(Tess, DblVector, SplinePos);
        end;
      end
      else
      begin
        for I := 0 to SplineDivisions * (Count - 1) do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          GluTessVertex(Tess, DblVector, SplinePos);
        end;
      end;
      Spline.Free;
    end;
    GluTessEndContour(Tess);
    GluTessEndPolygon(Tess);
    // release stuff
    if Assigned(NewVertices) then
      FreeMem(NewVertices);
    GluDeleteTess(Tess);
  end;
end;

// ------------------
// ------------------ TNotifyCollection ------------------
// ------------------

// Create
//
constructor TNotifyCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
    OnNotifyChange := TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//
procedure TNotifyCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnNotifyChange) then
    FOnNotifyChange(Self);
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

initialization

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

finalization

VManagers.Free;
VManagers := nil;

end.
