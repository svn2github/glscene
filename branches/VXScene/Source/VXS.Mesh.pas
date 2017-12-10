//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{ 
  Raw Mesh support.

  This unit is for simple meshes and legacy support, VXS.VectorFileObjects
  implements more efficient (though more complex) mesh tools.

}
unit VXS.Mesh;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  VXS.OpenGL1x,
  VXS.XOpenGL,
  VXS.Strings,
  VXS.Context,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.State,
  VXS.Color, 
  VXS.BaseClasses,  
  VXS.RenderContextInfo, 
  VXS.VectorTypes;

type
  TMeshMode = (mmTriangleStrip, mmTriangleFan, mmTriangles, mmQuadStrip,
    mmQuads, mmPolygon);
  TVertexMode = (vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT);

const
  cMeshModeToGLEnum: array[Low(TMeshMode)..High(TMeshMode)
    ] of GLEnum = (GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
    GL_QUAD_STRIP, GL_QUADS, GL_POLYGON);
  cVertexModeToGLEnum: array[Low(TVertexMode)..High(TVertexMode)
    ] of GLEnum = (GL_V3F, GL_N3F_V3F, GL_C4F_N3F_V3F, GL_T2F_C4F_N3F_V3F,
    GL_T2F_N3F_V3F, GL_T2F_V3F);

type

  TVXVertexData = packed record
    textCoord: TTexPoint;
    color: TVector;
    normal: TAffineVector;
    coord: TVertex;
  end;

  PVKVertexData = ^TVXVertexData;
  TVXVertexDataArray = array[0..(MAXINT shr 6)] of TVXVertexData;
  PVKVertexDataArray = ^TVXVertexDataArray;

  { Stores an interlaced vertex list for direct use in OpenGL.
    Locking (hardware passthrough) is supported, see "Locked" property for details. }
  TVXVertexList = class(TVXUpdateAbleObject)
  private
    FValues: PVKVertexDataArray;
    FCount: Integer;
    FCapacity, FGrowth: Integer;
    FLockedOldValues: PVKVertexDataArray;
  protected
    procedure SetCapacity(const val: Integer);
    procedure SetGrowth(const val: Integer);
    procedure Grow;
    procedure SetVertices(index: Integer; const val: TVXVertexData);
    function GetVertices(index: Integer): TVXVertexData;
    procedure SetVertexCoord(index: Integer; const val: TAffineVector);
    function GetVertexCoord(index: Integer): TAffineVector;
    procedure SetVertexNormal(index: Integer; const val: TAffineVector);
    function GetVertexNormal(index: Integer): TAffineVector;
    procedure SetVertexTexCoord(index: Integer; const val: TTexPoint);
    function GetVertexTexCoord(index: Integer): TTexPoint;
    procedure SetVertexColor(index: Integer; const val: TVector4f);
    function GetVertexColor(index: Integer): TVector4f;
    function GetFirstEntry: PGLFloat;
    function GetFirstColor: PGLFloat;
    function GetFirstNormal: PGLFloat;
    function GetFirstVertex: PGLFloat;
    function GetFirstTexPoint: PGLFloat;
    function GetLocked: Boolean;
    procedure SetLocked(val: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CreateInterpolatedCoords(list2: TVXVertexList; lerpFactor: Single): TVXVertexList;
    { Adds a vertex to the list, fastest method. }
    procedure AddVertex(const vertexData: TVXVertexData); overload;
    { Adds a vertex to the list, fastest method for adding a triangle. }
    procedure AddVertex3(const vd1, vd2, vd3: TVXVertexData); overload;
    { Adds a vertex to the list. 
      Use the NullVector, NullHmgVector or NullTexPoint constants for
      params you don't want to set. }
    procedure AddVertex(const aVertex: TVertex; const aNormal: TAffineVector;
      const aColor: TColorVector; const aTexPoint: TTexPoint); overload;
    { Adds a vertex to the list, no texturing version.  }
    procedure AddVertex(const vertex: TVertex; const normal: TAffineVector;
      const color: TColorVector); overload;
    { Adds a vertex to the list, no texturing, not color version.  }
    procedure AddVertex(const vertex: TVertex;
      const normal: TAffineVector); overload;
    { Duplicates the vertex of given index and adds it at the end of the list. }
    procedure DuplicateVertex(index: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property Vertices[index: Integer]: TVXVertexData read GetVertices
    write SetVertices; default;
    property VertexCoord[index: Integer]: TAffineVector read GetVertexCoord
    write SetVertexCoord;
    property VertexNormal[index: Integer]: TAffineVector read GetVertexNormal
    write SetVertexNormal;
    property VertexTexCoord[index: Integer]: TTexPoint read GetVertexTexCoord
    write SetVertexTexCoord;
    property VertexColor[index: Integer]: TVector4f read GetVertexColor
    write SetVertexColor;
    property Count: Integer read FCount;
    { Capacity of the list (nb of vertex).
      Use this to allocate memory quickly before calling AddVertex. }
    property Capacity: Integer read FCapacity write SetCapacity;
    { Vertex capacity that will be added each time the list needs to grow.
      default value is 256 (chunks of approx 13 kb). }
    property Growth: Integer read FGrowth write SetGrowth;
    { Calculates the sum of all vertex coords }
    function SumVertexCoords: TAffineVector;
    { Calculates the extents of the vertice coords. }
    procedure GetExtents(var min, max: TAffineVector);
    { Normalizes all normals. }
    procedure NormalizeNormals;
    { Translate all coords by given vector }
    procedure Translate(const v: TAffineVector);
    procedure DefineOpenGLArrays;
    property FirstColor: PGLFloat read GetFirstColor;
    property FirstEntry: PGLFloat read GetFirstEntry;
    property FirstNormal: PGLFloat read GetFirstNormal;
    property FirstVertex: PGLFloat read GetFirstVertex;
    property FirstTexPoint: PGLFloat read GetFirstTexPoint;

    { Locking state of the vertex list.
      You can "Lock" a list to increase rendering performance on some
      OpenVX implementations (NVidia's). A Locked list size shouldn't be
      changed and calculations should be avoided.
      Performance can only be gained from a lock for osDirectDraw object,
      ie. meshes that are updated for each frame (the default build list
      mode is faster on static meshes).
      Be aware that the "Locked" state enforcement is not very strict
      to avoid performance hits, and VXScene may not always notify you
      that you're doing things you shouldn't on a locked list! }
    property Locked: Boolean read GetLocked write SetLocked;
    procedure EnterLockSection;
    procedure LeaveLockSection;
  end;

  // TVXMesh
  //
  { Basic mesh object. 
    Each mesh holds a set of vertices and a Mode value defines how they make
    up the mesh (triangles, strips...) }
  TVXMesh = class(TVXSceneObject)
  private
    
    FVertices: TVXVertexList;
    FMode: TMeshMode;
    FVertexMode: TVertexMode;
    FAxisAlignedDimensionsCache: TVector;

  protected
    
    procedure SetMode(AValue: TMeshMode);
    procedure SetVertices(AValue: TVXVertexList);
    procedure SetVertexMode(AValue: TVertexMode);

    procedure VerticesChanged(Sender: TObject);

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure CalcNormals(Frontface: TVXFaceWinding);
    property Vertices: TVXVertexList read FVertices write SetVertices;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure StructureChanged; override;

  published
    
    property Mode: TMeshMode read FMode write SetMode;
    property VertexMode: TVertexMode read FVertexMode write SetVertexMode
      default vmVNCT;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ----------------- TVXVertexList ------------------------------------------------

constructor TVXVertexList.Create(AOwner: TPersistent);
begin
  inherited;
  FValues := nil;
  FCount := 0;
  FCapacity := 0;
  FGrowth := 256;
end;

destructor TVXVertexList.Destroy;
begin
  Locked := False;
  FreeMem(FValues);
  inherited;
end;

function TVXVertexList.CreateInterpolatedCoords(list2: TVXVertexList;
  lerpFactor: Single): TVXVertexList;
var
  i: Integer;
begin
  Assert(Count = list2.Count);
  Result := TVXVertexList.Create(nil);
  Result.Capacity := Count;
  Move(FValues[0], Result.FValues[0], Count * SizeOf(TVXVertexData));
  // interpolate vertices
  for i := 0 to Count - 1 do
    VectorLerp(FValues^[i].coord, list2.FValues^[i].coord, lerpFactor,
      Result.FValues^[i].coord);
end;

procedure TVXVertexList.SetCapacity(const val: Integer);
begin
  Assert(not Locked, 'Cannot change locked list capacity !');
  FCapacity := val;
  if FCapacity < FCount then
    FCapacity := FCount;
  ReallocMem(FValues, FCapacity * SizeOf(TVXVertexData));
end;

procedure TVXVertexList.SetGrowth(const val: Integer);
begin
  if val > 16 then
    FGrowth := val
  else
    FGrowth := 16;
end;

procedure TVXVertexList.Grow;
begin
  Assert(not Locked, 'Cannot add to a locked list !');
  FCapacity := FCapacity + FGrowth;
  ReallocMem(FValues, FCapacity * SizeOf(TVXVertexData));
end;

function TVXVertexList.GetFirstColor: PGLFloat;
begin
  Result := @(FValues^[0].color);
end;

// GetFirstEntry
//

function TVXVertexList.GetFirstEntry: PGLFloat;
begin
  Result := Pointer(FValues);
end;

function TVXVertexList.GetFirstNormal: PGLFloat;
begin
  Result := @(FValues^[0].normal);
end;

function TVXVertexList.GetFirstVertex: PGLFloat;
begin
  Result := @(FValues^[0].coord);
end;

function TVXVertexList.GetFirstTexPoint: PGLFloat;
begin
  Result := @(FValues^[0].textCoord);
end;

function TVXVertexList.GetLocked: Boolean;
begin
  Result := Assigned(FLockedOldValues);
end;

procedure TVXVertexList.SetLocked(val: Boolean);
var
  size: Integer;
begin
  if val <> Locked then
  begin
    //! Only supported with NVidia's right now
    if GL_NV_vertex_array_range and (CurrentVXContext <> nil) then
    begin
      size := FCount * SizeOf(TVXVertexData);
      if val then
      begin
        // Lock
        FLockedOldValues := FValues;
        {$IFDEF MSWINDOWS}
        FValues := wglAllocateMemoryNV(size, 0, 0, 0.5);
        {$ENDIF}
        {$IFDEF LINUX}
        FValues := glxAllocateMemoryNV(size, 0, 0, 0.5);
        {$ENDIF}
        if FValues = nil then
        begin
          FValues := FLockedOldValues;
          FLockedOldValues := nil;
        end
        else
          Move(FLockedOldValues^, FValues^, size);
      end
      else
      begin
        // Unlock
        wglFreeMemoryNV(nil);

        FValues := FLockedOldValues;
        FLockedOldValues := nil;
      end;
    end;
  end;
end;

procedure TVXVertexList.EnterLockSection;
begin
  if Locked then
  begin
    glVertexArrayRangeNV(FCount * SizeOf(TVXVertexData), FValues);
    glEnableClientState(GL_VERTEX_ARRAY_RANGE_NV);
  end;
end;

procedure TVXVertexList.LeaveLockSection;
begin
  if Locked then
  begin
    glDisableClientState(GL_VERTEX_ARRAY_RANGE_NV);
    glFlushVertexArrayRangeNV;
  end;
end;

procedure TVXVertexList.SetVertices(index: Integer; const val: TVXVertexData);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  FValues^[index] := val;
  NotifyChange(Self);
end;

function TVXVertexList.GetVertices(index: Integer): TVXVertexData;
begin
  Assert(Cardinal(index) < Cardinal(Count));
  Result := FValues^[index];
end;

procedure TVXVertexList.SetVertexCoord(index: Integer; const val: TAffineVector);
begin
  FValues^[index].coord := val;
  NotifyChange(Self);
end;

function TVXVertexList.GetVertexCoord(index: Integer): TAffineVector;
begin
  Result := FValues^[index].coord;
end;

procedure TVXVertexList.SetVertexNormal(index: Integer; const val: TAffineVector);
begin
  FValues^[index].normal := val;
  NotifyChange(Self);
end;

function TVXVertexList.GetVertexNormal(index: Integer): TAffineVector;
begin
  Result := FValues^[index].normal;
end;

procedure TVXVertexList.SetVertexTexCoord(index: Integer; const val: TTexPoint);
begin
  FValues^[index].textCoord := val;
  NotifyChange(Self);
end;

function TVXVertexList.GetVertexTexCoord(index: Integer): TTexPoint;
begin
  Result := FValues^[index].textCoord;
end;

procedure TVXVertexList.SetVertexColor(index: Integer; const val: TVector4f);
begin
  FValues^[index].color := val;
  NotifyChange(Self);
end;

function TVXVertexList.GetVertexColor(index: Integer): TVector4f;
begin
  Result := FValues^[index].color;
end;

procedure TVXVertexList.AddVertex(const vertexData: TVXVertexData);
begin
  if FCount = FCapacity then
    Grow;
  FValues^[FCount] := vertexData;
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TVXVertexList.AddVertex3(const vd1, vd2, vd3: TVXVertexData);
begin
  // extend memory space
  if FCount + 2 >= FCapacity then
    Grow;
  // calculate destination address for new vertex data
  FValues^[FCount] := vd1;
  FValues^[FCount + 1] := vd2;
  FValues^[FCount + 2] := vd3;
  Inc(FCount, 3);
  NotifyChange(Self);
end;

procedure TVXVertexList.AddVertex(const aVertex: TVertex;
  const aNormal: TAffineVector; const aColor: TColorVector;
  const aTexPoint: TTexPoint);
begin
  if FCount = FCapacity then
    Grow;
  // calculate destination address for new vertex data
  with FValues^[FCount] do
  begin
    textCoord := aTexPoint;
    color := aColor;
    normal := aNormal;
    coord := aVertex;
  end;
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TVXVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector; const color: TColorVector);
begin
  AddVertex(vertex, normal, color, NullTexPoint);
end;

procedure TVXVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector);
begin
  AddVertex(vertex, normal, clrBlack, NullTexPoint);
end;

procedure TVXVertexList.DuplicateVertex(index: Integer);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  if FCount = FCapacity then
    Grow;
  FValues[FCount] := FValues[index];
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TVXVertexList.Clear;
begin
  Assert(not Locked, 'Cannot clear a locked list !');
  FreeMem(FValues);
  FCount := 0;
  FCapacity := 0;
  FValues := nil;
  NotifyChange(Self);
end;

function TVXVertexList.SumVertexCoords: TAffineVector;
var
  i: Integer;
begin
  Result := NullVector;
  for i := 0 to Count - 1 do
    AddVector(Result, FValues^[i].coord);
end;

procedure TVXVertexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    with FValues^[i] do
      for k := 0 to 2 do
      begin
        f := coord.V[k];
        if f < min.V[k] then
          min.V[k] := f;
        if f > max.V[k] then
          max.V[k] := f;
      end;
  end;
end;

procedure TVXVertexList.NormalizeNormals;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    NormalizeVector(FValues^[i].coord);
end;

procedure TVXVertexList.Translate(const v: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AddVector(FValues^[i].coord, v);
end;

procedure TVXVertexList.DefineOpenGLArrays;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVXVertexData) - SizeOf(TAffineVector),
    FirstVertex);
  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TVXVertexData) - SizeOf(TAffineVector),
    FirstNormal);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVXVertexData) - SizeOf(TTexPoint),
    FirstTexPoint);
end;

procedure TVXVertexList.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXVertexList) then
  begin
    FCount := TVXVertexList(Source).FCount;
    FCapacity := FCount;
    ReallocMem(FValues, FCount * SizeOf(TVXVertexData));
    Move(TVXVertexList(Source).FValues^, FValues^, FCount * SizeOf(TVXVertexData));
  end
  else
    inherited Assign(Source);
end;

// ----------------- TVXMesh ------------------------------------------------------

constructor TVXMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ObjectStyle:=ObjectStyle+[osDirectDraw];
  FVertices := TVXVertexList.Create(Self);
  FVertices.AddVertex(XVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(YVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(ZVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.OnNotifyChange := VerticesChanged;
  FAxisAlignedDimensionsCache.X := -1;
  FVertexMode := vmVNCT;
  // should change this later to default to vmVN. But need to
end; // change GLMeshPropform so that it greys out unused vertex info

destructor TVXMesh.Destroy;
begin
  FVertices.Free;
  inherited Destroy;
end;

procedure TVXMesh.VerticesChanged(Sender: TObject);
begin
  StructureChanged;
end;

procedure TVXMesh.BuildList(var rci: TVXRenderContextInfo);
var
  VertexCount: Longint;
begin
  inherited;
  if osDirectDraw in ObjectStyle then
    FVertices.EnterLockSection;
  case FVertexMode of
    vmV:
      glInterleavedArrays(GL_V3F, SizeOf(TVXVertexData), FVertices.FirstVertex);
    vmVN:
      glInterleavedArrays(GL_N3F_V3F, SizeOf(TVXVertexData),
        FVertices.FirstNormal);
    vmVNC:
      glInterleavedArrays(GL_C4F_N3F_V3F, SizeOf(TVXVertexData),
        FVertices.FirstColor);
    vmVNT, vmVNCT:
      glInterleavedArrays(GL_T2F_C4F_N3F_V3F, 0, FVertices.FirstEntry);
    vmVT:
      glInterleavedArrays(GL_T2F_V3F, 0, FVertices.FirstEntry);
  else
    Assert(False, strInterleaveNotSupported);
  end;
  if FVertexMode in [vmVNC, vmVNCT] then
  begin
    rci.VXStates.Enable(stColorMaterial);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    rci.VXStates.SetMaterialColors(cmFront, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
    rci.VXStates.SetMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
  end;
  VertexCount := FVertices.Count;
  case FMode of
    mmTriangleStrip:
      glDrawArrays(GL_TRIANGLE_STRIP, 0, VertexCount);
    mmTriangleFan:
      glDrawArrays(GL_TRIANGLE_FAN, 0, VertexCount);
    mmTriangles:
      glDrawArrays(GL_TRIANGLES, 0, VertexCount);
    mmQuadStrip:
      glDrawArrays(GL_QUAD_STRIP, 0, VertexCount);
    mmQuads:
      glDrawArrays(GL_QUADS, 0, VertexCount);
    mmPolygon:
      glDrawArrays(GL_POLYGON, 0, VertexCount);
  else
    Assert(False);
  end;
  if osDirectDraw in ObjectStyle then
    FVertices.LeaveLockSection;
end;

procedure TVXMesh.SetMode(AValue: TMeshMode);
begin
  if AValue <> FMode then
  begin
    FMode := AValue;
    StructureChanged;
  end;
end;

procedure TVXMesh.SetVertices(AValue: TVXVertexList);
begin
  if AValue <> FVertices then
  begin
    FVertices.Assign(AValue);
    StructureChanged;
  end;
end;

procedure TVXMesh.SetVertexMode(AValue: TVertexMode);
begin
  if AValue <> FVertexMode then
  begin
    FVertexMode := AValue;
    StructureChanged;
  end;
end;

procedure TVXMesh.CalcNormals(Frontface: TVXFaceWinding);
var
  vn: TAffineFltVector;
  i, j: Integer;
begin
  case FMode of
    mmTriangleStrip:
      with Vertices do
        for i := 0 to Count - 3 do
        begin
          if (Frontface = fwCounterClockWise) xor ((i and 1) = 0) then
            vn := CalcPlaneNormal(FValues^[i + 0].coord, FValues^[i + 1].coord,
              FValues^[i + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[i + 2].coord, FValues^[i + 1].coord,
              FValues^[i + 0].coord);
          FValues^[i].normal := vn;
        end;
    mmTriangles:
      with Vertices do
        for i := 0 to ((Count - 3) div 3) do
        begin
          j := i * 3;
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[j + 0].coord, FValues^[j + 1].coord,
              FValues^[j + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[j + 2].coord, FValues^[j + 1].coord,
              FValues^[j + 0].coord);
          FValues^[j + 0].normal := vn;
          FValues^[j + 1].normal := vn;
          FValues^[j + 2].normal := vn;
        end;
    mmQuads:
      with Vertices do
        for i := 0 to ((Count - 4) div 4) do
        begin
          j := i * 4;
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[j + 0].coord, FValues^[j + 1].coord,
              FValues^[j + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[j + 2].coord, FValues^[j + 1].coord,
              FValues^[j + 0].coord);
          FValues^[j + 0].normal := vn;
          FValues^[j + 1].normal := vn;
          FValues^[j + 2].normal := vn;
          FValues^[j + 3].normal := vn;
        end;
    mmPolygon:
      with Vertices do
        if Count > 2 then
        begin
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[0].coord, FValues^[1].coord,
              FValues^[2].coord)
          else
            vn := CalcPlaneNormal(FValues^[2].coord, FValues^[1].coord,
              FValues^[0].coord);
          for i := 0 to Count - 1 do
            FValues^[i].normal := vn;
        end;
  else
    Assert(False);
  end;
  StructureChanged;
end;

procedure TVXMesh.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVXMesh) then
  begin
    FVertices.Assign(TVXMesh(Source).Vertices);
    FMode := TVXMesh(Source).FMode;
    FVertexMode := TVXMesh(Source).FVertexMode;
  end
  else
    inherited Assign(Source);
end;

function TVXMesh.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    Vertices.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := MaxFloat(Abs(dMin.X), Abs(dMax.X));
    FAxisAlignedDimensionsCache.Y := MaxFloat(Abs(dMin.Y), Abs(dMax.Y));
    FAxisAlignedDimensionsCache.Z := MaxFloat(Abs(dMin.Z), Abs(dMax.Z));
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

procedure TVXMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  // class registrations
  RegisterClasses([TVXMesh]);

end.

