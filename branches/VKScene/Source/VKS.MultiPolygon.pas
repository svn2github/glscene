//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Object with support for complex polygons. 
 
}
{ TODO

  And I reactivated the TVectorPool object. The VKS.VectorLists are not suitable for this job.
  When the tesselator finds an intersection of edges it wants us to give him some storage
  for this new vertex, and he wants a pointer (see tessCombine). The pointers taken from
  TAffineVectorList become invalid after enlarging the capacity (makes a ReAllocMem), which
  can happen implicitly while adding. The TVectorPool keeps all pointers valid until the
  destruction itself.

  If anyone feels responsible: it would be fine to have a method ImportFromFile (dxf?) in
  the TVKContour and TVKMultiPolygonBase objects...
}
unit VKS.MultiPolygon;

interface

{$I VKScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  uOpenGLAdapter,
  VKS.XOpenGL,
  VKS.Spline,
  VKS.Context,
  VKS.VectorTypes,
  VKS.VectorGeometry,
  VKS.VectorLists,
  VKS.PersistentClasses,
  VKS.Scene,
  VKS.Objects,
  VKS.GeomObjects,
  VKS.Nodes,
  VKS.BaseClasses,
  VKS.Coordinates,
  VKS.RenderContextInfo;

type
  TVKContourNodes = class(TVKNodes)
  public
    procedure NotifyChange; override;
  end;

  TVKContour = class(TCollectionItem)
  private
    FNodes: TVKContourNodes;
    FDivision: Integer;
    FSplineMode: TLineSplineMode;
    FDescription: string;
    procedure SetNodes(const Value: TVKContourNodes);
    procedure SetDivision(Value: Integer);
    procedure SetSplineMode(const Value: TLineSplineMode);
    procedure SetDescription(const Value: string);
  protected
    procedure CreateNodes; dynamic;
    procedure NodesChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Description: string read FDescription write SetDescription;
    { The nodes list.  }
    property Nodes: TVKContourNodes read FNodes write SetNodes;
    { Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. }
    property Division: Integer read FDivision write SetDivision default 10;
    { Default spline drawing mode. 
      This mode is used only for the curve, not for the rotation path. }
    property SplineMode: TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;
  end;

  TVKContourClass = class of TVKContour;

  TVKContours = class(TVKNotifyCollection)
  private
    function GetItems(index: Integer): TVKContour;
    procedure SetItems(index: Integer; const Value: TVKContour);
  protected
  public
    constructor Create(AOwner: TComponent); overload;
    function Add: TVKContour;
    function FindItemID(ID: Integer): TVKContour;
    property Items[index: Integer]: TVKContour read GetItems write SetItems; default;
    procedure GetExtents(var min, max: TAffineVector);
  end;

  TPolygonList = class(TPersistentObjectList)
  private
    FAktList: TAffineVectorList;
    function GetList(I: Integer): TAffineVectorList;
  public
    procedure Add;
    property AktList: TAffineVectorList read FAktList;
    property List[I: Integer]: TAffineVectorList read GetList;
  end;

  { Multipolygon is defined with multiple contours.
     The contours have to be in the X-Y plane, otherwise they are projected
     to it (this is done automatically by the tesselator).
     The plane normal is pointing in +Z. All contours are automatically closed,
     so there is no need to specify the last node equal to the first one.
     Contours should be defined counterclockwise, the first contour (index = 0)
     is taken as is, all following are reversed. This means you can define the
     outer contour first and the holes and cutouts after that. If you give the
     following contours in clockwise order, the first contour is extended.
     TMultiPolygonBase will take the input contours and let the tesselator
     make an outline from it (this is done in RetreiveOutline). This outline is
     used for Rendering. Only when there are changes in the contours, the
     outline will be recalculated. The ouline in fact is a list of VKS.VectorLists. }
  TMultiPolygonBase = class(TVKSceneObject)
  private
    FContours: TVKContours;
    FOutline: TPolygonList;
    FContoursNormal: TAffineVector;
    FAxisAlignedDimensionsCache: TVector;
    procedure SetContours(const Value: TVKContours);
    function GetPath(i: Integer): TVKContourNodes;
    procedure SetPath(i: Integer; const value: TVKContourNodes);
    function GetOutline: TPolygonList;
    procedure SetContoursNormal(const Value: TAffineVector);
  protected
    procedure RenderTesselatedPolygon(textured: Boolean;
      normal: PAffineVector; invertNormals: Boolean);
    procedure RetrieveOutline(List: TPolygonList);
    procedure ContourChanged(Sender: TObject); virtual;
    //property PNormal:PAffineVector read FPNormal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddNode(const i: Integer; const coords: TVKCoordinates); overload;
    procedure AddNode(const i: Integer; const X, Y, Z: GLfloat); overload;
    procedure AddNode(const i: Integer; const value: TVector); overload;
    procedure AddNode(const i: Integer; const value: TAffineVector); overload;
    property Path[i: Integer]: TVKContourNodes read GetPath write SetPath;
    property Outline: TPolygonList read GetOutline;
    property ContoursNormal: TAffineVector read FContoursNormal write SetContoursNormal;
    function AxisAlignedDimensionsUnscaled: TVector; override;
    procedure StructureChanged; override;
  published
    property Contours: TVKContours read FContours write SetContours;
  end;

  { A polygon that can have holes and multiple contours.
     Use the Path property to access a contour or one of the AddNode methods
     to add a node to a contour (contours are allocated automatically). }
  TVKMultiPolygon = class(TMultiPolygonBase)
  private
    FParts: TPolygonParts;
  protected
    procedure SetParts(const value: TPolygonParts);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVKRenderContextInfo); override;
  published
    property Parts: TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
  end;

//============================================================
implementation
//============================================================

type
  { page oriented pointer array, with persistent pointer target memory.
    In TVectorList a pointer to a vector will not be valid any more after
    a call to SetCapacity, which might be done implicitely during Add.
    The TVectorPool keeps memory in its original position during its
    whole lifetime. }

  // removed Notify (only D5)
  // added Destroy (also working with D4)
  TVectorPool = class(TList)
  private
    FEntrySize: Integer; // size of each entry
    FPageSize: Integer; // number of entries per page
    FArrSize: Integer; // size of one page
    FUsedEntries: Integer; // used entries in actual page
    FAktArray: VKS.VectorGeometry.PByteArray; // pointer to actual page
    procedure CreatePage; // create new page
  public
    constructor Create(APageSize, AEntrySize: Integer);
    destructor Destroy; override;

    { retrieve pointer to new entry. will create new page if needed }
    procedure GetNewVector(var P: Pointer);
  end;

//-----------------------------------------------
  { TVectorPool }
//-----------------------------------------------

constructor TVectorPool.Create(APageSize, AEntrySize: Integer);
begin
  inherited Create;
  Assert(APageSize > 0);
  Assert(AEntrySize > 0);
  FPageSize := APageSize;
  FEntrySize := AEntrySize;
  FArrSize := FPageSize * FEntrySize;
  CreatePage;
end;

procedure TVectorPool.CreatePage;
begin
  GetMem(FAktArray, FArrSize);
  Add(FAktArray);
  FUsedEntries := 0;
end;

destructor TVectorPool.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    FreeMem(Items[i], FArrSize);
  inherited;
end;

procedure TVectorPool.GetNewVector(var P: Pointer);
begin
  if FUsedEntries >= FPageSize then
    CreatePage;
  Inc(FUsedEntries);
  P := @(FAktArray[(FUsedEntries - 1) * FEntrySize]);
end;

// ------------------
// ------------------ TPolygonList ------------------
// ------------------

procedure TPolygonList.Add;
begin
  FAktList := TAffineVectorList.Create;
  inherited Add(FAktList);
end;

function TPolygonList.GetList(i: Integer): TAffineVectorList;
begin
  Result := TAffineVectorList(Items[i]);
end;

// ------------------
// ------------------ TVKContour ------------------
// ------------------

constructor TVKContour.Create(Collection: TCollection);
begin
  inherited;
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

procedure TVKContour.CreateNodes;
begin
  FNodes := TVKContourNodes.Create(Self);
end;

destructor TVKContour.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TVKContour.Assign(Source: TPersistent);
begin
  if Source is TVKContour then
  begin
    FNodes.Assign(TVKContour(Source).FNodes);
    FDivision := TVKContour(Source).FDivision;
    FSplineMode := TVKContour(Source).FSplineMode;
    FDescription := TVKContour(Source).FDescription;
  end
  else
    inherited;
end;

function TVKContour.GetDisplayName: string;
begin
  result := Description;
  if result = '' then
    result := Format('GLContour: %d nodes', [Nodes.Count]);
end;

procedure TVKContour.NodesChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TVKContour.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TVKContour.SetDivision(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FDivision then
  begin
    FDivision := value;
    Changed(false);
  end;
end;

procedure TVKContour.SetNodes(const Value: TVKContourNodes);
begin
  FNodes.Assign(Value);
  Changed(false);
end;

procedure TVKContour.SetSplineMode(const Value: TLineSplineMode);
begin
  if FSplineMode <> value then
  begin
    FSplineMode := value;
    Changed(false);
  end;
end;

//--------------------------------------------
{ TVKContours }
//--------------------------------------------

function TVKContours.Add: TVKContour;
begin
  Result := TVKContour(inherited Add);
end;

constructor TVKContours.Create(AOwner: TComponent);
begin
  Create(AOwner, TVKContour);
end;

function TVKContours.FindItemID(ID: Integer): TVKContour;
begin
  result := TVKContour(inherited FindItemId(Id));
end;

function TVKContours.GetItems(index: Integer): TVKContour;
begin
  result := TVKContour(inherited Items[index]);
end;

procedure TVKContours.SetItems(index: Integer; const Value: TVKContour);
begin
  inherited Items[index] := value;
end;

procedure TVKContours.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1e30;
  cSmallValue: Single = -1e30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetItems(i).Nodes.GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

//--------------------------------------------
{ TMultiPolygonBase }
//--------------------------------------------

constructor TMultiPolygonBase.Create(AOwner: TComponent);
begin
  inherited;
  FContours := TVKContours.Create(Self);
  FContours.OnNotifyChange := ContourChanged;
  FContoursNormal := AffineVectorMake(0, 0, 1);
  FAxisAlignedDimensionsCache.X := -1;
end;

destructor TMultiPolygonBase.Destroy;
begin
  if FOutline <> nil then
  begin
    FOutline.Clean;
    FreeAndNil(FOutline);
  end;  
  FContours.Free;
  inherited;
end;

procedure TMultiPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TMultiPolygonBase then
  begin
    FContours.Assign(TMultiPolygonBase(Source).FContours);
  end;
  inherited;
end;

procedure TMultiPolygonBase.ContourChanged(Sender: TObject);
begin
  if Assigned(FOutline) then
  begin
    // force a RetrieveOutline with next Render
    FOutline.Clean;
    FreeAndNil(FOutline);
    StructureChanged;
  end;
end;

procedure TMultiPolygonBase.AddNode(const i: Integer; const value: TVector);
begin
  Path[i].AddNode(value);
end;

procedure TMultiPolygonBase.AddNode(const i: Integer; const x, y, z: GLfloat);
begin
  Path[i].AddNode(x, y, z);
end;

procedure TMultiPolygonBase.AddNode(const i: Integer; const coords: TVKCoordinates);
begin
  Path[i].AddNode(coords);
end;

procedure TMultiPolygonBase.AddNode(const I: Integer; const value: TAffineVector);
begin
  Path[i].AddNode(value);
end;

procedure TMultiPolygonBase.SetContours(const Value: TVKContours);
begin
  FContours.Assign(Value);
end;

function TMultiPolygonBase.GetOutline: TPolygonList;
begin
  if not Assigned(FOutline) then
  begin
    FOutline := TPolygonList.Create;
    RetrieveOutline(FOutline);
  end;
  Result := FOutline;
end;

function TMultiPolygonBase.GetPath(i: Integer): TVKContourNodes;
begin
  Assert(i >= 0);
  while i >= Contours.Count do
    Contours.Add;
  Result := Contours[i].Nodes;
end;

procedure TMultiPolygonBase.SetPath(i: Integer; const value: TVKContourNodes);
begin
  Assert(i >= 0);
  while i >= Contours.Count do
    Contours.Add;
  Contours[i].Nodes.Assign(value);
end;

//
// Tessellation routines (Vulkan callbacks)
//

var
  vVertexPool: TVectorPool;

procedure tessError(errno: GLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  Assert(False, IntToStr(errno) + ' : ' + string(gluErrorString(errno)));
end;

procedure tessIssueVertex(vertexData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  glTexCoord2fv(vertexData);
  glVertex3fv(vertexData);
end;

procedure tessCombine(coords: PDoubleVector; vertex_data: Pointer;
  weight: PGLFloat; var outData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  vVertexPool.GetNewVector(outData);
  SetVector(PAffineVector(outData)^, coords^[0], coords^[1], coords^[2]);
end;

procedure tessBeginList(typ: GLEnum; polygonData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  TPolygonList(polygonData).Add;
end;

procedure tessIssueVertexList(vertexData: Pointer; polygonData: Pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  TPolygonList(polygonData).AktList.Add(PAffineVector(vertexData)^);
end;

procedure TMultiPolygonBase.RetrieveOutline(List: TPolygonList);
var
  i, n: Integer;
  tess: GLUTesselator;

  procedure TesselatePath(contour: TVKContour; inverted: Boolean);

    procedure IssueVertex(v: TAffineVector);
    var
      dblVector: TAffineDblVector;
      p: PAffineVector;
    begin
      vVertexPool.GetNewVector(Pointer(p));
      p^ := v;
      SetVector(dblVector, v);
      gluTessVertex(tess, @dblVector, p);
    end;

  var
    i, n: Integer;
    spline: TCubicSpline;
    f: Single;
    splineDivisions: Integer;
    nodes: TVKContourNodes;
  begin
    gluTessBeginContour(tess);
    nodes := contour.Nodes;
    if contour.SplineMode = lsmLines then
      splineDivisions := 0
    else
      splineDivisions := contour.Division;
    if splineDivisions > 1 then
    begin
      spline := nodes.CreateNewCubicSpline;
      try
        f := 1 / splineDivisions;
        n := splineDivisions * (nodes.Count - 1);
        if inverted then
        begin
          for i := n downto 0 do
            IssueVertex(spline.SplineAffineVector(i * f))
        end
        else
        begin
          for i := 0 to n do
            IssueVertex(spline.SplineAffineVector(i * f));
        end;
      finally
        spline.Free;
      end;
    end
    else
    begin
      n := nodes.Count - 1;
      if inverted then
      begin
        for i := n downto 0 do
          IssueVertex(nodes[i].AsAffineVector)
      end
      else
      begin
        for i := 0 to n do
          IssueVertex(nodes[i].AsAffineVector);
      end;
    end;
    gluTessEndContour(tess);
  end;

begin
  List.Clear;
  if (Contours.Count > 0) and (Path[0].Count > 2) then
  begin
    // Vertex count
    n := 0;
    for i := 0 to Contours.Count - 1 do
      n := n + Path[i].Count;
    // Create and initialize the GLU tesselator
    vVertexPool := TVectorPool.Create(n, SizeOf(TAffineVector));
    tess := gluNewTess;
    try
      // register callbacks
      gluTessCallback(tess, GLU_TESS_BEGIN_DATA, @tessBeginList);
      gluTessCallback(tess, GLU_TESS_END_DATA, nil);
      gluTessCallback(tess, GLU_TESS_VERTEX_DATA, @tessIssueVertexList);
      gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
      gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);

      // issue normal
      gluTessNormal(tess, FContoursNormal.X, FContoursNormal.Y, FContoursNormal.Z);

      // set properties
      gluTessProperty(Tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
      gluTessProperty(Tess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);

      gluTessBeginPolygon(tess, List);
      // outside contour
      TesselatePath(Contours[0], False);
      // inside contours
      for n := 1 to Contours.Count - 1 do
        TesselatePath(Contours[n], True);
      gluTessEndPolygon(tess);
    finally
      gluDeleteTess(tess);
      vVertexPool.Free;
      vVertexPool := nil;
    end;
  end;
end;

procedure TMultiPolygonBase.RenderTesselatedPolygon(textured: Boolean;
  normal: PAffineVector;
  invertNormals: Boolean);
var
  tess: GLUTesselator;

  procedure IssueVertex(v: TAffineVector);
  var
    dblVector: TAffineDblVector;
    p: PAffineVector;
  begin
    vVertexPool.GetNewVector(Pointer(p));
    p^ := v;
    SetVector(dblVector, v);
    gluTessVertex(tess, @dblVector, p);
  end;

var
  i, n: Integer;
begin
  // call to Outline will call RetrieveOutline if necessary
  if (Outline.Count = 0) or (Outline.List[0].Count < 2) then
    Exit;
  // Vertex count
  n := 0;
  for i := 0 to Outline.Count - 1 do
    n := n + Outline.List[i].Count;
  // Create and initialize a vertex pool and the GLU tesselator
  vVertexPool := TVectorPool.Create(n, Sizeof(TAffineVector));
  tess := gluNewTess;
  try
    gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
    if textured then
      gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
    else
      gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
    gluTessCallback(tess, GLU_TESS_END, @glEnd);
    gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
    gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
    // Issue normal
    if Assigned(normal) then
    begin
      glNormal3fv(PGLFloat(normal));
      gluTessNormal(tess, normal^.X, normal^.Y, normal^.Z);
    end;
    gluTessProperty(Tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
    // Issue polygon
    gluTessBeginPolygon(tess, nil);
    for n := 0 to Outline.Count - 1 do
    begin
      with Outline.List[n] do
      begin
        gluTessBeginContour(tess);
        if invertNormals then
          for i := Count - 1 downto 0 do
            IssueVertex(Items[i])
        else
          for i := 0 to Count - 1 do
            IssueVertex(Items[i]);
        gluTessEndContour(tess);
      end;
    end;
    gluTessEndPolygon(tess);
  finally
    gluDeleteTess(tess);
    vVertexPool.Free;
    vVertexPool := nil;
  end;
end;

// ------------------
// ------------------ TVKMultiPolygon ------------------
// ------------------

constructor TVKMultiPolygon.Create(AOwner: TComponent);
begin
  inherited;
  FParts := [ppTop, ppBottom];
end;

procedure TVKMultiPolygon.Assign(Source: TPersistent);
begin
  if Source is TVKMultiPolygon then
  begin
    FParts := TVKMultiPolygon(Source).FParts;
  end;
  inherited;
end;

procedure TVKMultiPolygon.BuildList(var rci: TVKRenderContextInfo);
var
  normal: TAffineVector;
begin
  if (Outline.Count < 1) then
    Exit;
  normal := ContoursNormal;
  // Render
  // tessellate top polygon
  if ppTop in FParts then
    RenderTesselatedPolygon(True, @normal, False);
  // tessellate bottom polygon
  if ppBottom in FParts then
  begin
    NegateVector(normal);
    RenderTesselatedPolygon(True, @normal, True)
  end;
end;

procedure TVKMultiPolygon.SetParts(const value: TPolygonParts);
begin
  if FParts <> value then
  begin
    FParts := value;
    StructureChanged;
  end;
end;

procedure TMultiPolygonBase.SetContoursNormal(const Value: TAffineVector);
begin
  FContoursNormal := Value;
end;

function TMultiPolygonBase.AxisAlignedDimensionsUnscaled: TVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    Contours.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := MaxFloat(Abs(dMin.X), Abs(dMax.X));
    FAxisAlignedDimensionsCache.Y := MaxFloat(Abs(dMin.Y), Abs(dMax.Y));
    FAxisAlignedDimensionsCache.Z := MaxFloat(Abs(dMin.Z), Abs(dMax.Z));
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

procedure TMultiPolygonBase.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------
// ------------------ TVKContourNodes ------------------
// ------------------

procedure TVKContourNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TVKContour).Changed(False);
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClass(TVKMultiPolygon);

end.

