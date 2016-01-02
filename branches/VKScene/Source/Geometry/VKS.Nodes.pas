//
// This unit is part of the GLScene Project   
//
{ : VKS.Nodes<p>

  Nodes are used to describe lines, polygons + more.<p>

  <b>History : </b><font size=-1><ul>
  <li>01/03/11 - Vincent - Fix a bug in TVKNodes.Vector
  <li>17/10/10 - Yar - Added TagObject property to TVKNode (thanks µAlexx)
  <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>26/11/09 - DaStr - Improved Lazarus compatibility
  (thanks Predator) (BugtrackerID = 2893580)
  <li>22/11/09 - DaStr - Improved Unix compatibility
  (thanks Predator) (BugtrackerID = 2893580)
  <li>14/07/09 - DaStr - Added $I GLScene.inc
  <li>05/10/08 - DanB - Created from GLMisc.pas split
  </ul></font>
}
unit VKS.Nodes;

interface

uses
  System.Classes, System.SysUtils,
   
  VKS.VectorGeometry, VKS.OpenGLTokens, VKS.OpenGLAdapter,
  VKS.Context, VKS.BaseClasses,  VKS.Coordinates, VKS.Spline,
  VKS.XOpenGL, VKS.VectorTypes;


{$I VKScene.inc}

type
  // TVKNode
  //
  TVKNode = class(TCollectionItem)
  private
    { Private Declarations }
    FCoords: TVector;
    FTagObject: TObject;
    procedure SetAsVector(const Value: TVector);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    procedure SetCoordinate(AIndex: Integer; AValue: TVKFloat);
    function GetCoordinate(const Index: Integer): TVKFloat;

  protected
    { Protected Declarations }
    function StoreCoordinate(AIndex: Integer): Boolean;

    function GetDisplayName: string; override;

  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
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

    property W: TVKFloat index 3 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;

    property TagObject: TObject read FTagObject write FTagObject;
  published
    { Published Declarations }
    property X: TVKFloat index 0 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Y: TVKFloat index 1 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
    property Z: TVKFloat index 2 read GetCoordinate write SetCoordinate
      stored StoreCoordinate;
  end;

  // TVKNodes
  //
  TVKNodes = class(TOwnedCollection)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure SetItems(Index: Integer; const Val: TVKNode);
    function GetItems(Index: Integer): TVKNode;
    procedure Update(Item: TCollectionItem); override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent;
      AItemClass: TCollectionItemClass = nil);
    function CreateCopy(AOwner: TPersistent): TVKNodes;

    function Add: TVKNode;
    function FindItemID(ID: Integer): TVKNode;
    property Items[index: Integer]: TVKNode read GetItems
      write SetItems; default;
    function First: TVKNode;
    function Last: TVKNode;

    procedure NotifyChange; virtual;
    procedure EndUpdate; override;

    procedure AddNode(const Coords: TVKCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: TVKfloat); overload;
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

    procedure RenderTesselatedPolygon(ATextured: Boolean;
      ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
      AInvertNormals: Boolean = False);

    function CreateNewCubicSpline: TCubicSpline;

  end;

  TVKNodesClass = class of TVKNodes;

implementation

// ------------------
// ------------------ TVKNode ------------------
// ------------------

// Create
//

constructor TVKNode.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  // nothing, yet
end;

// Destroy
//

destructor TVKNode.Destroy;
begin
  // nothing, yet
  inherited Destroy;
end;

// Assign
//

procedure TVKNode.Assign(Source: TPersistent);
begin
  if Source is TVKNode then
  begin
    FCoords := TVKNode(Source).FCoords;
  end
  else
    inherited;
end;

// GetDisplayName
//

function TVKNode.GetDisplayName: string;
begin
  Result := Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

// AsAddress
//

function TVKNode.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

// SetAsVector
//

procedure TVKNode.SetAsVector(const Value: TVector);
begin
  FCoords := Value;
  (Collection as TVKNodes).NotifyChange;
end;

// SetAsAffineVector
//

procedure TVKNode.SetAsAffineVector(const Value: TAffineVector);
begin
  VKS.VectorGeometry.SetVector(FCoords, Value);
  (Collection as TVKNodes).NotifyChange;
end;

// GetAsAffineVector
//

function TVKNode.GetAsAffineVector: TAffineVector;
begin
  VKS.VectorGeometry.SetVector(Result, FCoords);
end;

function TVKNode.GetCoordinate(const Index: Integer): TVKFloat;
begin
  Result := FCoords.V[Index];
end;

// SetCoordinate
//

procedure TVKNode.SetCoordinate(AIndex: Integer; AValue: TVKFloat);
begin
  FCoords.V[AIndex] := AValue;
  (Collection as TVKNodes).NotifyChange;
end;

// StoreCoordinate
//

function TVKNode.StoreCoordinate(AIndex: Integer): Boolean;
begin
  Result := (FCoords.V[AIndex] <> 0);
end;

// ------------------
// ------------------ TVKNodes ------------------
// ------------------

// Create
//

constructor TVKNodes.Create(AOwner: TPersistent;
  AItemClass: TCollectionItemClass = nil);
begin
  if not Assigned(AItemClass) then
    inherited Create(AOwner, TVKNode)
  else
    inherited Create(AOwner, AItemClass);
end;

// CreateCopy
//

function TVKNodes.CreateCopy(AOwner: TPersistent): TVKNodes;
begin
  if Self <> nil then
  begin
    Result := TVKNodesClass(Self.ClassType).Create(AOwner);
    Result.Assign(Self);
  end
  else
    Result := nil;
end;

// SetItems
//

procedure TVKNodes.SetItems(Index: Integer; const Val: TVKNode);
begin
  inherited Items[index] := Val;
end;

// GetItems
//

function TVKNodes.GetItems(Index: Integer): TVKNode;
begin
  Result := TVKNode(inherited Items[index]);
end;

// First
//

function TVKNodes.First: TVKNode;
begin
  if Count > 0 then
    Result := TVKNode(inherited Items[0])
  else
    Result := nil;
end;

// Last
//

function TVKNodes.Last: TVKNode;
var
  N: Integer;
begin
  N := Count - 1;
  if N >= 0 then
    Result := TVKNode(inherited Items[N])
  else
    Result := nil;
end;

// Update
//

procedure TVKNodes.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

// Add
//

function TVKNodes.Add: TVKNode;
begin
  Result := (inherited Add) as TVKNode;
end;

// FindItemID
//

function TVKNodes.FindItemID(ID: Integer): TVKNode;
begin
  Result := (inherited FindItemID(ID)) as TVKNode;
end;

// NotifyChange
//

procedure TVKNodes.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and
    (GetOwner is TVKUpdateAbleComponent) then
    TVKUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

// EndUpdate
//

procedure TVKNodes.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;

// AddNode (TVKCustomCoordinates)
//

procedure TVKNodes.AddNode(const Coords: TVKCustomCoordinates);
begin
  Add.AsVector := Coords.AsVector;
end;

// AddNode (floats)
//

procedure TVKNodes.AddNode(const X, Y, Z: Single);
begin
  Add.AsVector := PointMake(X, Y, Z);
end;

// AddNode (TVector)
//

procedure TVKNodes.AddNode(const Value: TVector);
begin
  Add.AsVector := Value;
end;

// AddNode (TAffineVector)
//

procedure TVKNodes.AddNode(const Value: TAffineVector);
begin
  Add.AsAffineVector := Value;
end;

// AddXYArc
//

procedure TVKNodes.AddXYArc(XRadius, YRadius: Single;
  StartAngle, StopAngle: Single; NbSegments: Integer;
  const Center: TAffineVector);
var
  I: Integer;
  F: Single;
  S, C: Single;
begin
  BeginUpdate;
  try
    StartAngle := DegToRadian(StartAngle);
    StopAngle := DegToRadian(StopAngle);
    F := (StopAngle - StartAngle) / NbSegments;
    for I := 0 to NbSegments do
    begin
      SinCosine(I * F + StartAngle, S, C);
      SetVector(Add.FCoords, Center.V[0] + XRadius * C,
                             Center.V[1] + YRadius * S,
                             Center.V[2], 1);
    end;
  finally
    EndUpdate;
  end;
end;

// Barycenter
//

function TVKNodes.Barycenter: TAffineVector;
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

function TVKNodes.Normal: TAffineVector;
begin
  if Count >= 3 then
    CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords,
      Items[2].FCoords, Result)
  else
    Result := NullVector;
end;

// Vector
//

function TVKNodes.Vector(I: Integer): TAffineVector;

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

var
  J: Integer;
  Vecnull: Boolean;
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
  begin
    // avoid returning null vector which generates display bugs in geometry
    J := 1;
    Vecnull := True;
    while (I + J < Count) and (Vecnull) do
    begin
      VectorSubtract(Items[I + J].AsVector, Items[I].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    J := 1;
    while (I - J > 0) and (Vecnull) do
    begin
      VectorSubtract(Items[I].AsVector, Items[I - J].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    if Vecnull then
      SetVector(Result, NullVector)
    else
      NormalizeVector(Result);
  end
  else
    NormalizeVector(Result);
end;

// GetExtents
//

procedure TVKNodes.GetExtents(var Min, Max: TAffineVector);
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
      F := PAffineVector(Items[I].AsAddress)^.V[K];
      if F < Min.V[K] then
        Min.V[K] := F;
      if F > Max.V[K] then
        Max.V[K] := F;
    end;
  end;
end;

// Translate
//

procedure TVKNodes.Translate(const Tv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(Items[I].AsAddress)^, Tv);
  NotifyChange;
end;

// Scale (vector)
//

procedure TVKNodes.Scale(const Fv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, Fv);
  NotifyChange;
end;

// Scale (single)
//

procedure TVKNodes.Scale(F: Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, F);
  NotifyChange;
end;

// RotateAroundX
//

procedure TVKNodes.RotateAroundX(Angle: Single);
var
  I: Integer;
  C, S, V2: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V2 := V^.V[2];
    V^.V[1] := C * V^.V[1] + S * V2;
    V^.V[2] := C * V2 - S * V^.V[1];
  end;
  NotifyChange;
end;

// RotateAroundY
//

procedure TVKNodes.RotateAroundY(Angle: Single);
var
  I: Integer;
  C, S, V0: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V0 := V^.V[0];
    V^.V[0] := C * V0 + S * V^.V[2];
    V^.V[2] := C * V^.V[2] - S * V0;
  end;
  NotifyChange;
end;

// RotateAroundZ
//

procedure TVKNodes.RotateAroundZ(Angle: Single);
var
  I: Integer;
  C, S, V1: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V1 := V^.V[1];
    V^.V[1] := C * V1 + S * V^.V[0];
    V^.V[0] := C * V^.V[0] - S * V1;
  end;
  NotifyChange;
end;

// CreateNewCubicSpline
//

function TVKNodes.CreateNewCubicSpline: TCubicSpline;
var
  I: Integer;
  Xa, Ya, Za: PFloatArray;
begin
  GetMem(Xa, SizeOf(TVKFloat) * Count);
  GetMem(Ya, SizeOf(TVKFloat) * Count);
  GetMem(Za, SizeOf(TVKFloat) * Count);
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

function AllocNewVertex: PAffineVector;
begin
  Inc(NbExtraVertices);
  Result := @NewVertices[NbExtraVertices - 1];
end;

procedure TessError(Errno: TVKEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(Errno) + ': ' + string(GluErrorString(Errno)));
end;

procedure TessIssueVertex(VertexData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  Xgl.TexCoord2fv(VertexData);
  GL.Vertex3fv(VertexData);
end;

procedure TessCombine(Coords: PDoubleVector; Vertex_data: Pointer;
  Weight: PGLFloat; var OutData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  OutData := AllocNewVertex;
  SetVector(PAffineVector(OutData)^, Coords^[0], Coords^[1], Coords^[2]);
end;

procedure TVKNodes.RenderTesselatedPolygon(ATextured: Boolean;
  ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
  AInvertNormals: Boolean = False);
var
  I: Integer;
  Tess: PGLUTesselator;
  DblVector: TAffineDblVector;
  Spline: TCubicSpline;
  SplinePos: PAffineVector;
  F: Single;

begin
  if Count > 2 then
  begin
    // Create and initialize the GLU tesselator
    Tess := GluNewTess;
    GluTessCallback(Tess, GLU_TESS_BEGIN, @GL.Begin_);
    if ATextured then
      GluTessCallback(Tess, GLU_TESS_VERTEX, @TessIssueVertex)
    else
      GluTessCallback(Tess, GLU_TESS_VERTEX, @GL.Vertex3fv);
    GluTessCallback(Tess, GLU_TESS_END, @GL.End_);
    GluTessCallback(Tess, GLU_TESS_ERROR, @TessError);
    GluTessCallback(Tess, GLU_TESS_COMBINE, @TessCombine);
    NbExtraVertices := 0;
    // Issue normal
    if Assigned(ANormal) then
    begin
      GL.Normal3fv(PGLFloat(ANormal));
      GluTessNormal(Tess, ANormal^.V[0], ANormal^.V[1], ANormal^.V[2]);
    end;
    // Issue polygon
    GluTessBeginPolygon(Tess, nil);
    GluTessBeginContour(Tess);
    if ASplineDivisions <= 1 then
    begin
      // no spline, use direct coordinates
      GetMem(NewVertices, Count * SizeOf(TAffineVector));
      if AInvertNormals then
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
      GetMem(NewVertices, 2 * ASplineDivisions * Count * SizeOf(TAffineVector));
      Spline := CreateNewCubicSpline;
      F := 1.0 / ASplineDivisions;
      if AInvertNormals then
      begin
        for I := ASplineDivisions * (Count - 1) downto 0 do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          GluTessVertex(Tess, DblVector, SplinePos);
        end;
      end
      else
      begin
        for I := 0 to ASplineDivisions * (Count - 1) do
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

end.
