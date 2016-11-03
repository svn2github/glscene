//
// VKScene project, http://glscene.sourceforge.net
//
{ 
  Raw Mesh support in GLScene.

  This unit is for simple meshes and legacy support, VKS.VectorFileObjects
  implements more efficient (though more complex) mesh tools.

}
unit VKS.Mesh;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL, 
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  //VKS
  VKS.OpenGLAdapter,
  VKS.Strings,  
  VKS.XOpenGL,  
  VKS.Context,  
  VKS.Scene,
  VKS.VectorGeometry,   
  VKS.State,
  VKS.Color, 
  VKS.BaseClasses,  
  VKS.RenderContextInfo, 
  VKS.VectorTypes;

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

  TVertexData = packed record
    textCoord: TTexPoint;
    color: TVector;
    normal: TAffineVector;
    coord: TVertex;
  end;

  PVertexData = ^TVertexData;
  TVertexDataArray = array[0..(MAXINT shr 6)] of TVertexData;
  PVertexDataArray = ^TVertexDataArray;

  // TVKVertexList
  //
  { Stores an interlaced vertex list for direct use in OpenGL. 
    Locking (hardware passthrough) is supported, see "Locked" property for details. }
  TVKVertexList = class(TVKUpdateAbleObject)
  private
    { Private Declarations }
    FValues: PVertexDataArray;
    FCount: Integer;
    FCapacity, FGrowth: Integer;
    FLockedOldValues: PVertexDataArray;

  protected
    { Protected Declarations }
    FGL: TVKExtensionsAndEntryPoints;
    procedure SetCapacity(const val: Integer);
    procedure SetGrowth(const val: Integer);
    procedure Grow;
    procedure SetVertices(index: Integer; const val: TVertexData);
    function GetVertices(index: Integer): TVertexData;
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
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    function CreateInterpolatedCoords(list2: TVKVertexList; lerpFactor: Single)
      : TVKVertexList;

    { Adds a vertex to the list, fastest method. }
    procedure AddVertex(const vertexData: TVertexData); overload;
    { Adds a vertex to the list, fastest method for adding a triangle. }
    procedure AddVertex3(const vd1, vd2, vd3: TVertexData); overload;
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

    property Vertices[index: Integer]: TVertexData read GetVertices
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
      Vulkan implementations (NVidia's). A Locked list size shouldn't be
      changed and calculations should be avoided. 
      Performance can only be gained from a lock for osDirectDraw object,
      ie. meshes that are updated for each frame (the default build list
      mode is faster on static meshes). 
      Be aware that the "Locked" state enforcement is not very strict
      to avoid performance hits, and VKScene may not always notify you
      that you're doing things you shouldn't on a locked list! }
    property Locked: Boolean read GetLocked write SetLocked;
    procedure EnterLockSection;
    procedure LeaveLockSection;
  end;

  // TVKMesh
  //
  { Basic mesh object. 
    Each mesh holds a set of vertices and a Mode value defines how they make
    up the mesh (triangles, strips...) }
  TVKMesh = class(TVKSceneObject)
  private
    { Private Declarations }
    FVertices: TVKVertexList;
    FMode: TMeshMode;
    FVertexMode: TVertexMode;
    FAxisAlignedDimensionsCache: TVector;

  protected
    { Protected Declarations }
    procedure SetMode(AValue: TMeshMode);
    procedure SetVertices(AValue: TVKVertexList);
    procedure SetVertexMode(AValue: TVertexMode);

    procedure VerticesChanged(Sender: TObject);

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;
    procedure CalcNormals(Frontface: TFaceWinding);
    property Vertices: TVKVertexList read FVertices write SetVertices;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure StructureChanged; override;

  published
    { Published Declarations }
    property Mode: TMeshMode read FMode write SetMode;
    property VertexMode: TVertexMode read FVertexMode write SetVertexMode
      default vmVNCT;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ----------------- TVKVertexList ------------------------------------------------

constructor TVKVertexList.Create(AOwner: TPersistent);
begin
  inherited;
  FValues := nil;
  FCount := 0;
  FCapacity := 0;
  FGrowth := 256;
end;

// Destroy
//

destructor TVKVertexList.Destroy;
begin
  Locked := False;
  FreeMem(FValues);
  inherited;
end;

// CreateInterpolatedCoords
//

function TVKVertexList.CreateInterpolatedCoords(list2: TVKVertexList;
  lerpFactor: Single): TVKVertexList;
var
  i: Integer;
begin
  Assert(Count = list2.Count);
  Result := TVKVertexList.Create(nil);
  Result.Capacity := Count;
  Move(FValues[0], Result.FValues[0], Count * SizeOf(TVertexData));
  // interpolate vertices
  for i := 0 to Count - 1 do
    VectorLerp(FValues^[i].coord, list2.FValues^[i].coord, lerpFactor,
      Result.FValues^[i].coord);
end;

// SetCapacity
//

procedure TVKVertexList.SetCapacity(const val: Integer);
begin
  Assert(not Locked, 'Cannot change locked list capacity !');
  FCapacity := val;
  if FCapacity < FCount then
    FCapacity := FCount;
  ReallocMem(FValues, FCapacity * SizeOf(TVertexData));
end;

// SetGrowth
//

procedure TVKVertexList.SetGrowth(const val: Integer);
begin
  if val > 16 then
    FGrowth := val
  else
    FGrowth := 16;
end;

// Grow
//

procedure TVKVertexList.Grow;
begin
  Assert(not Locked, 'Cannot add to a locked list !');
  FCapacity := FCapacity + FGrowth;
  ReallocMem(FValues, FCapacity * SizeOf(TVertexData));
end;

// GetFirstColor
//

function TVKVertexList.GetFirstColor: PGLFloat;
begin
  Result := @(FValues^[0].color);
end;

// GetFirstEntry
//

function TVKVertexList.GetFirstEntry: PGLFloat;
begin
  Result := Pointer(FValues);
end;

// GetFirstNormal
//

function TVKVertexList.GetFirstNormal: PGLFloat;
begin
  Result := @(FValues^[0].normal);
end;

// GetFirstVertex
//

function TVKVertexList.GetFirstVertex: PGLFloat;
begin
  Result := @(FValues^[0].coord);
end;

// GetFirstTexPoint
//

function TVKVertexList.GetFirstTexPoint: PGLFloat;
begin
  Result := @(FValues^[0].textCoord);
end;

// GetLocked
//

function TVKVertexList.GetLocked: Boolean;
begin
  Result := Assigned(FLockedOldValues);
end;

// SetLocked
//

procedure TVKVertexList.SetLocked(val: Boolean);
var
  size: Integer;
begin
  if val <> Locked then
  begin
    //! Only supported with NVidia's right now
    if GL_NV_vertex_array_range and (CurrentVKContext <> nil) then
    begin
      size := FCount * SizeOf(TVertexData);
      if val then
      begin
        // Lock
        FLockedOldValues := FValues;
        {$IFDEF MSWINDOWS}
        FValues := FGL.wglAllocateMemoryNV(size, 0, 0, 0.5);
        {$ENDIF}
        {$IFDEF LINUX}
        FValues := FGL.glxAllocateMemoryNV(size, 0, 0, 0.5);
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
        FGL.wglFreeMemoryNV(0, 0, 0, 0); //<-FGL.wglFreeMemoryNV(FValues);

        FValues := FLockedOldValues;
        FLockedOldValues := nil;
      end;
    end;
  end;
end;

// EnterLockSection
//

procedure TVKVertexList.EnterLockSection;
begin
  if Locked then
  begin
    glVertexArrayRangeNV(FCount * SizeOf(TVertexData), FValues);
    glEnableClientState(GL_VERTEX_ARRAY_RANGE_NV);
  end;
end;

// LeaveLockSection
//

procedure TVKVertexList.LeaveLockSection;
begin
  if Locked then
  begin
    glDisableClientState(GL_VERTEX_ARRAY_RANGE_NV);
    glFlushVertexArrayRangeNV;
  end;
end;

// SetVertices
//

procedure TVKVertexList.SetVertices(index: Integer; const val: TVertexData);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  FValues^[index] := val;
  NotifyChange(Self);
end;

// GetVertices
//

function TVKVertexList.GetVertices(index: Integer): TVertexData;
begin
  Assert(Cardinal(index) < Cardinal(Count));
  Result := FValues^[index];
end;

// SetVertexCoord
//

procedure TVKVertexList.SetVertexCoord(index: Integer; const val: TAffineVector);
begin
  FValues^[index].coord := val;
  NotifyChange(Self);
end;

// GetVertexCoord
//

function TVKVertexList.GetVertexCoord(index: Integer): TAffineVector;
begin
  Result := FValues^[index].coord;
end;

// SetVertexNormal
//

procedure TVKVertexList.SetVertexNormal(index: Integer; const val: TAffineVector);
begin
  FValues^[index].normal := val;
  NotifyChange(Self);
end;

// GetVertexNormal
//

function TVKVertexList.GetVertexNormal(index: Integer): TAffineVector;
begin
  Result := FValues^[index].normal;
end;

// SetVertexTexCoord
//

procedure TVKVertexList.SetVertexTexCoord(index: Integer; const val: TTexPoint);
begin
  FValues^[index].textCoord := val;
  NotifyChange(Self);
end;

// GetVertexTexCoord
//

function TVKVertexList.GetVertexTexCoord(index: Integer): TTexPoint;
begin
  Result := FValues^[index].textCoord;
end;

// SetVertexColor
//

procedure TVKVertexList.SetVertexColor(index: Integer; const val: TVector4f);
begin
  FValues^[index].color := val;
  NotifyChange(Self);
end;

// GetVertexColor
//

function TVKVertexList.GetVertexColor(index: Integer): TVector4f;
begin
  Result := FValues^[index].color;
end;

// AddVertex (direct)
//

procedure TVKVertexList.AddVertex(const vertexData: TVertexData);
begin
  if FCount = FCapacity then
    Grow;
  FValues^[FCount] := vertexData;
  Inc(FCount);
  NotifyChange(Self);
end;

// AddVertex3
//

procedure TVKVertexList.AddVertex3(const vd1, vd2, vd3: TVertexData);
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

// AddVertex (texturing)
//

procedure TVKVertexList.AddVertex(const aVertex: TVertex;
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

// AddVertex (no texturing)
//

procedure TVKVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector; const color: TColorVector);
begin
  AddVertex(vertex, normal, color, NullTexPoint);
end;

// AddVertex (no texturing, no color)
//

procedure TVKVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector);
begin
  AddVertex(vertex, normal, clrBlack, NullTexPoint);
end;

// DuplicateVertex
//

procedure TVKVertexList.DuplicateVertex(index: Integer);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  if FCount = FCapacity then
    Grow;
  FValues[FCount] := FValues[index];
  Inc(FCount);
  NotifyChange(Self);
end;

// Clear
//

procedure TVKVertexList.Clear;
begin
  Assert(not Locked, 'Cannot clear a locked list !');
  FreeMem(FValues);
  FCount := 0;
  FCapacity := 0;
  FValues := nil;
  NotifyChange(Self);
end;

// SumVertexCoords
//

function TVKVertexList.SumVertexCoords: TAffineVector;
var
  i: Integer;
begin
  Result := NullVector;
  for i := 0 to Count - 1 do
    AddVector(Result, FValues^[i].coord);
end;

// GetExtents
//

procedure TVKVertexList.GetExtents(var min, max: TAffineVector);
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

// NormalizeNormals
//

procedure TVKVertexList.NormalizeNormals;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    NormalizeVector(FValues^[i].coord);
end;

// Translate
//

procedure TVKVertexList.Translate(const v: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AddVector(FValues^[i].coord, v);
end;

// DefineOpenGLArrays
//

procedure TVKVertexList.DefineOpenGLArrays;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertexData) - SizeOf(TAffineVector),
    FirstVertex);
  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TVertexData) - SizeOf(TAffineVector),
    FirstNormal);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertexData) - SizeOf(TTexPoint),
    FirstTexPoint);
end;

// Assign
//

procedure TVKVertexList.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKVertexList) then
  begin
    FCount := TVKVertexList(Source).FCount;
    FCapacity := FCount;
    ReallocMem(FValues, FCount * SizeOf(TVertexData));
    Move(TVKVertexList(Source).FValues^, FValues^, FCount * SizeOf(TVertexData));
  end
  else
    inherited Assign(Source);
end;

// ----------------- TVKMesh ------------------------------------------------------

// Create
//

constructor TVKMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ObjectStyle:=ObjectStyle+[osDirectDraw];
  FVertices := TVKVertexList.Create(Self);
  FVertices.AddVertex(XVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(YVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(ZVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.OnNotifyChange := VerticesChanged;
  FAxisAlignedDimensionsCache.X := -1;
  FVertexMode := vmVNCT;
  // should change this later to default to vmVN. But need to
end; // change GLMeshPropform so that it greys out unused vertex info

// Destroy
//

destructor TVKMesh.Destroy;
begin
  FVertices.Free;
  inherited Destroy;
end;

// VerticesChanged
//

procedure TVKMesh.VerticesChanged(Sender: TObject);
begin
  StructureChanged;
end;

// BuildList
//

procedure TVKMesh.BuildList(var rci: TVKRenderContextInfo);
var
  VertexCount: Longint;
begin
  inherited;
  if osDirectDraw in ObjectStyle then
    FVertices.EnterLockSection;
  case FVertexMode of
    vmV:
      glInterleavedArrays(GL_V3F, SizeOf(TVertexData), FVertices.FirstVertex);
    vmVN:
      glInterleavedArrays(GL_N3F_V3F, SizeOf(TVertexData),
        FVertices.FirstNormal);
    vmVNC:
      glInterleavedArrays(GL_C4F_N3F_V3F, SizeOf(TVertexData),
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
    rci.VKStates.Enable(stColorMaterial);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    rci.VKStates.SetMaterialColors(cmFront, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
    rci.VKStates.SetMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
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

// SetMode
//

procedure TVKMesh.SetMode(AValue: TMeshMode);
begin
  if AValue <> FMode then
  begin
    FMode := AValue;
    StructureChanged;
  end;
end;

// SetVertices
//

procedure TVKMesh.SetVertices(AValue: TVKVertexList);
begin
  if AValue <> FVertices then
  begin
    FVertices.Assign(AValue);
    StructureChanged;
  end;
end;

// SetVertexMode
//

procedure TVKMesh.SetVertexMode(AValue: TVertexMode);
begin
  if AValue <> FVertexMode then
  begin
    FVertexMode := AValue;
    StructureChanged;
  end;
end;

// CalcNormals
//

procedure TVKMesh.CalcNormals(Frontface: TFaceWinding);
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
{$IFDEF VKS_ASM}
  // clear fpu exception flag
  asm fclex
  end;
{$ENDIF}
  StructureChanged;
end;

// Assign
//

procedure TVKMesh.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TVKMesh) then
  begin
    FVertices.Assign(TVKMesh(Source).Vertices);
    FMode := TVKMesh(Source).FMode;
    FVertexMode := TVKMesh(Source).FVertexMode;
  end
  else
    inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//

function TVKMesh.AxisAlignedDimensionsUnscaled: TVector;
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

// StructureChanged
//

procedure TVKMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  // class registrations
  RegisterClasses([TVKMesh]);

end.

