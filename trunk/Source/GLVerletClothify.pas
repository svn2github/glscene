//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLVerletClothify<p>

   Methods for turning a TGLBaseMesh into a Verlet cloth / jelly<p>

	<b>History : </b><font size=-1><ul>
      <li>17/06/03 - MF - Creation
   </ul>
}

unit GLVerletClothify;

interface

uses
  Classes,  GLVectorFileObjects, VerletClasses, VectorTypes, VectorLists,
  Geometry, GLTexture, OpenGL12, SysUtils;

type
  TFace = class
  public
    Vertices : array[0..2] of integer;
    Normal : TAffineVector;
    MeshObject : TMeshObject;

    procedure UpdateNormal;

    constructor Create(aMeshObject : TMeshObject);
  end;

  TFaceList = class(TList)
  private
    function GetItems(i: integer): TFace;
    procedure SetItems(i: integer; const Value: TFace);
  public
    property Items[i : integer] : TFace read GetItems write SetItems; default;
  end;

  TFaceExtractor = class
  private
    FFaceList : TFaceList;
    FGLBaseMesh : TGLBaseMesh;
    FNodeList : TVerletNodeList;
    FWeldDistance: single;
    FEdgeDoublesSkipped : integer;

    procedure SetWeldDistance(const Value: single);
  protected
    procedure ProcessMeshObject(const MeshObject : TMeshObject); virtual;
  public
    procedure ExtractFacesFromVertexIndexList(
      const FaceGroup : TFGVertexIndexList; const MeshObject : TMeshObject);

    property FaceList : TFaceList read FFaceList;

    procedure Clear; virtual;
    procedure ProcessMesh; virtual;

    property WeldDistance : single read FWeldDistance write SetWeldDistance;
    property EdgeDoublesSkipped : integer read FEdgeDoublesSkipped;

    property GLBaseMesh : TGLBaseMesh read FGLBaseMesh;

    function AddFace(const Vi0, Vi1, Vi2 : integer; const MeshObject : TMeshObject) : TFace; virtual;

    constructor Create(const aGLBaseMesh : TGLBaseMesh); virtual;
    destructor Destroy; override;
  end;

  // ************ FACE SMOOTHER

  TVertexStat = class
  private
    FFaceList: TFaceList;
    FMeshObject : TMeshObject;
    FVertexIndex : integer;
  public
    property VertexIndex : integer read FVertexIndex;
    property MeshObject : TMeshObject read FMeshObject;

    property FaceList : TFaceList read FFaceList write FFaceList;

    procedure UpdateNormal;

    constructor Create(const aMeshObject : TMeshObject; const aVertexIndex : integer);
    destructor Destroy; override;
  end;

  TVertextStatsList = class(TList)
  private
    function GetItems(i: integer): TVertexStat;
    procedure SetItems(i: integer; const Value: TVertexStat);

  public
    property Items[i : integer] : TVertexStat read GetItems write SetItems; default;
  end;

  { Doesn't work at all right now! }
  TFaceSmoother = class(TFaceExtractor)
  private
    FVertextStatsList: TVertextStatsList;
    FTempList : TVertextStatsList;
  public
    property VertextStatsList : TVertextStatsList read FVertextStatsList;
    procedure UpdateNormals;

    function AddFace(const Vi0, Vi1, Vi2 : integer; const MeshObject : TMeshObject) : TFace; override;

    procedure Clear; override;

    procedure ProcessMeshObject(const MeshObject : TMeshObject); override;

    constructor Create(const aGLBaseMesh : TGLBaseMesh); override;
    destructor Destroy; override;
  end;

  // ************ EDGE DETECTOR

  TEdge = class
  public
    Vertices : array[0..1] of integer;
    Faces : array[0..1] of TFace;

    MeshObject : TMeshObject;

    Solid : boolean;

    function SameSame(NodeList : TVerletNodeList) : boolean;
  end;

  TEdgeList = class(TList)
  private
    function GetItems(i: integer): TEdge;
    procedure SetItems(i: integer; const Value: TEdge);
  public
    property Items[i : integer] : TEdge read GetItems write SetItems; default;
  end;

  TEdgeDetector = class(TFaceExtractor)
  private
    FEdgeList : TEdgeList;

    procedure BuildOpposingEdges;
  public
    property EdgeList : TEdgeList read FEdgeList;

    procedure Clear; override;
    procedure ProcessMesh; override;

    function AddEdge(const Vi0, Vi1 : integer; const Face : TFace; const MeshObject : TMeshObject) : TEdge;
    function AddFace(const Vi0, Vi1, Vi2 : integer; const MeshObject : TMeshObject) : TFace; override;
    function AddNode(const VerletAssembly : TVerletAssembly; const MeshObject : TMeshObject; const VertexIndex : integer) : TVerletNode; virtual;

    function CreateVAEmpty : TVerletAssembly;
    function CreateVAWithSticks(Slack : single) : TVerletAssembly;
    function CreateVAWithForces(Strength, Damping, Slack : single) : TVerletAssembly;

    procedure RenderEdges(var rci : TRenderContextInfo);

    constructor Create(const aGLBaseMesh : TGLBaseMesh); override;
    destructor Destroy; override;
  end;

  TMeshObjectVerletNode = class(TVerletNode)
    MeshObject : TMeshObject;
    VertexIndices : TIntegerList;

    procedure Updated; override;

    constructor Create(aOwner : TVerletAssembly); override;
    destructor Destroy; override;
  end;

implementation

{ TFaceExtractor }

procedure TFaceExtractor.Clear;
var
  i : integer;
begin
  for i := 0 to FaceList.Count-1 do
    FaceList[i].Free;

  FaceList.Clear;
end;

constructor TFaceExtractor.Create(const aGLBaseMesh : TGLBaseMesh);
begin
  FFaceList := TFaceList.Create;
  FGLBaseMesh := aGLBaseMesh;
  FNodeList := TVerletNodeList.Create;
  FWeldDistance := 0.01;
end;

destructor TFaceExtractor.Destroy;
begin
  Clear;

  FreeAndNil(FNodeList);
  FreeAndNil(FFaceList);

  inherited;
end;

procedure TFaceExtractor.ExtractFacesFromVertexIndexList(
  const FaceGroup : TFGVertexIndexList; const MeshObject : TMeshObject);
var
  List : PIntegerArray;
  iFace, iVertex  : integer;
begin
  case FaceGroup.Mode of

    fgmmTriangles, fgmmFlatTriangles :
    begin
      for iFace := 0 to FaceGroup.TriangleCount - 1 do
      begin
        List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
        AddFace(List[0], List[1], List[2], MeshObject);
      end;
    end;

    fgmmTriangleStrip :
    begin
      for iFace:=0 to FaceGroup.VertexIndices.Count-3 do
      begin
        List := @FaceGroup.VertexIndices.List[iFace];
        if (iFace and 1)=0 then
           AddFace(List[0], List[1], List[2], MeshObject)
        else
           AddFace(List[2], List[1], List[0], MeshObject);
      end;
    end;

    fgmmTriangleFan :
    begin
      List := @FaceGroup.VertexIndices.List;

      for iVertex:=2 to FaceGroup.VertexIndices.Count-1 do
        AddFace(List[0], List[iVertex-1], List[iVertex], MeshObject)
    end;
    else
      Assert(false,'Not supported');
  end;
end;

procedure TFaceExtractor.ProcessMesh;
var
  iMeshObject : integer;
  MeshObject : TMeshObject;
begin
  for iMeshObject := 0 to FGLBaseMesh.MeshObjects.Count - 1 do
  begin
    MeshObject := FGLBaseMesh.MeshObjects[iMeshObject];

    ProcessMeshObject(MeshObject);
  end;
end;

procedure TFaceExtractor.ProcessMeshObject(const MeshObject : TMeshObject);
var
 iFaceGroup : integer;
begin
  if MeshObject.Mode = momFaceGroups then
  begin
    for iFaceGroup := 0 to MeshObject.FaceGroups.Count - 1 do
    begin
      if MeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList then
      begin
        ExtractFacesFromVertexIndexList(MeshObject.FaceGroups[iFaceGroup] as TFGVertexIndexList, MeshObject);
      end else
        Assert(false);
    end;
  end else
    Assert(false);
end;

function TFaceExtractor.AddFace(const Vi0, Vi1, Vi2: integer; const MeshObject : TMeshObject) : TFace;
var
  Face : TFace;
begin
  Face := TFace.Create(MeshObject);

  FaceList.Add(Face);

  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;

  result := Face;
end;

procedure TFaceExtractor.SetWeldDistance(const Value: single);
begin
  FWeldDistance := Value;
end;


{ TFaceList }

function TFaceList.GetItems(i: integer): TFace;
begin
  result := Get(i);
end;

procedure TFaceList.SetItems(i: integer; const Value: TFace);
begin
  Put(i, Value);
end;

{ TEdgeList }

function TEdgeList.GetItems(i: integer): TEdge;
begin
  result := Get(i);
end;

procedure TEdgeList.SetItems(i: integer; const Value: TEdge);
begin
  Put(i, Value);
end;

{ TMeshObjectVerletNode }

constructor TMeshObjectVerletNode.Create(aOwner: TVerletAssembly);
begin
  inherited;
  VertexIndices := TIntegerList.Create;
end;

destructor TMeshObjectVerletNode.Destroy;
begin
  VertexIndices.Free;
  inherited;
end;

procedure TMeshObjectVerletNode.Updated;
var
  i : integer;
begin
  // Update the actual vertex
  for i := 0 to VertexIndices.Count-1 do
    MeshObject.Vertices[VertexIndices[i]] := MeshObject.Owner.Owner.AbsoluteToLocal(Location);
end;

{ TEdge }

function TEdge.SameSame(NodeList: TVerletNodeList): boolean;
begin
  result := NodeList[Vertices[0]] = NodeList[Vertices[1]];
end;

{ TEdgeDetector }

procedure TEdgeDetector.Clear;
var
  i : integer;
begin
  inherited;

  for i := 0 to EdgeList.Count-1 do
    EdgeList[i].Free;

  EdgeList.Clear;
end;

constructor TEdgeDetector.Create(const aGLBaseMesh: TGLBaseMesh);
begin
  FEdgeList := TEdgeList.Create;

  inherited;
end;

destructor TEdgeDetector.Destroy;
begin
  inherited;

  FreeAndNil(FEdgeList);
end;

function TEdgeDetector.AddEdge(const Vi0, Vi1: integer; const Face: TFace; const MeshObject : TMeshObject): TEdge;
var
  i : integer;
  Edge : TEdge;
begin
  // Find an indentical edge, if there is one
  for i := 0 to EdgeList.Count - 1 do
  begin
    Edge := EdgeList[i];

    if (Edge.Vertices[0]=Vi0) and (Edge.Vertices[1]=Vi1) or
       (Edge.Vertices[1]=Vi0) and (Edge.Vertices[0]=Vi1) then
    begin
      Edge.Faces[1] := Face;

      result := Edge;
      exit;
    end;
  end;

  // No edge was found, create a new one
  Edge := TEdge.Create;
  Edge.Vertices[0] := Vi0;
  Edge.Vertices[1] := Vi1;
  Edge.Faces[0] := Face;
  Edge.Faces[1] := nil;
  Edge.MeshObject := MeshObject;
  Edge.Solid := true;

  EdgeList.Add(Edge);

  result := Edge;
end;

function TEdgeDetector.AddFace(const Vi0, Vi1, Vi2: integer;
  const MeshObject: TMeshObject): TFace;
var
  Face : TFace;
begin
  Face := TFace.Create(MeshObject);

  FaceList.Add(Face);

  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;

  AddEdge(Vi0, Vi1, Face, MeshObject);
  AddEdge(Vi1, Vi2, Face, MeshObject);
  AddEdge(Vi2, Vi0, Face, MeshObject);//}

  result := Face;
end;

function TEdgeDetector.CreateVAEmpty : TVerletAssembly;
var
  i : integer;
  VerletAssembly : TVerletAssembly;
  MO : TMeshObject;
begin
  VerletAssembly := TVerletAssembly.Create;

  MO := FGLBaseMesh.MeshObjects[0];

  for i := 0 to MO.Vertices.Count-1 do
    AddNode(VerletAssembly, MO, i);

  Assert(FNodeList.Count = MO.Vertices.Count, Format('%d <> %d',[FNodeList.Count, MO.Vertices.Count]));

  result := VerletAssembly;
end;

function TEdgeDetector.CreateVAWithForces(Strength,
  Damping, Slack: single): TVerletAssembly;
var
  i : integer;
  VerletAssembly : TVerletAssembly;
begin
  VerletAssembly := CreateVAEmpty;

  for i := 0 to EdgeList.Count-1 do
    if not EdgeList[i].SameSame(FNodeList) then
    begin
      VerletAssembly.CreateSpring(
        FNodeList[EdgeList[i].Vertices[0]],
        FNodeList[EdgeList[i].Vertices[1]],
        Strength, Damping, Slack);

      if EdgeList[i].Solid then
        VerletAssembly.AddSolidEdge(
          FNodeList[EdgeList[i].Vertices[0]],
          FNodeList[EdgeList[i].Vertices[1]]);
    end;

  result := VerletAssembly;
end;

function TEdgeDetector.CreateVAWithSticks(Slack : single): TVerletAssembly;
var
  i : integer;
  VerletAssembly : TVerletAssembly;
begin
  VerletAssembly := CreateVAEmpty;

  for i := 0 to EdgeList.Count-1 do
    if not EdgeList[i].SameSame(FNodeList) then
    begin
      VerletAssembly.CreateStick(
        FNodeList[EdgeList[i].Vertices[0]],
        FNodeList[EdgeList[i].Vertices[1]],
        Slack);

      if EdgeList[i].Solid then
        VerletAssembly.AddSolidEdge(
          FNodeList[EdgeList[i].Vertices[0]],
          FNodeList[EdgeList[i].Vertices[1]]);
    end;

  result := VerletAssembly;
end;

procedure TEdgeDetector.RenderEdges(var rci: TRenderContextInfo);
var
  i : integer;
  Edge : TEdge;
  Vertex0, Vertex1 : TAffineVector;
begin
  if EdgeList.Count>0 then
  begin
    glDisable(GL_LIGHTING);

    glLineWidth(3);
    glColor3f(1,1,1);

    glBegin(GL_LINES);
      for i := 0 to EdgeList.Count - 1 do
      begin
        Edge := EdgeList[i];

        Vertex0 := Edge.MeshObject.Vertices[Edge.Vertices[0]];
        Vertex1 := Edge.MeshObject.Vertices[Edge.Vertices[1]];

        glVertex3fv(PGLfloat(@Vertex0));
        glVertex3fv(PGLfloat(@Vertex1));
      end;
    glEnd;
    glEnable(GL_LIGHTING);
  end;//}
end;

procedure TEdgeDetector.BuildOpposingEdges;
var
  iEdge, EdgeCount, vi0, vi1, iEdgeTest : integer;
  Face0, Face1 : TFace;
  Edge, NewEdge, TestEdge : TEdge;
begin
  // For each edge that's connected by two triangles, create a new edge that
  // connects the two "extra" vertices.... makes sense?
  EdgeCount := EdgeList.Count;

  for iEdge := 0 to EdgeCount-1 do
  begin
    Edge := EdgeList[iEdge];

    if Assigned(Edge.Faces[1]) then
    begin
      Face0 := Edge.Faces[0];
      Face1 := Edge.Faces[1];

      if (Face0.Vertices[0] <> Edge.Vertices[0]) and (Face0.Vertices[0] <> Edge.Vertices[1]) then
        vi0 := Face0.Vertices[0]
      else if (Face0.Vertices[1] <> Edge.Vertices[0]) and (Face0.Vertices[1] <> Edge.Vertices[1]) then
        vi0 := Face0.Vertices[1]
      else
        vi0 := Face0.Vertices[2];

      if (Face1.Vertices[0] <> Edge.Vertices[0]) and (Face1.Vertices[0] <> Edge.Vertices[1]) then
        vi1 := Face1.Vertices[0]
      else if (Face1.Vertices[1] <> Edge.Vertices[0]) and (Face1.Vertices[1] <> Edge.Vertices[1]) then
        vi1 := Face1.Vertices[1]
      else
        vi1 := Face1.Vertices[2];

      if (vi0=vi1) or
         (vi0=Edge.Vertices[0]) or
         (vi0=Edge.Vertices[1]) or
         (vi1=Edge.Vertices[0]) or
         (vi1=Edge.Vertices[1]) then
        continue;

      // Find an indentical edge, if there is one
      for iEdgeTest := 0 to EdgeList.Count - 1 do
      begin
        TestEdge := EdgeList[iEdgeTest];

        if (TestEdge.Vertices[0]=Vi0) and (TestEdge.Vertices[1]=Vi1) or
           (TestEdge.Vertices[1]=Vi0) and (TestEdge.Vertices[0]=Vi1) then
        begin
          // Edge allready exists!
          inc(FEdgeDoublesSkipped);
          continue;
        end;
      end;

      NewEdge := TEdge.Create;
      NewEdge.Vertices[0] := Vi0;
      NewEdge.Vertices[1] := Vi1;
      NewEdge.Faces[0] := nil;
      NewEdge.Faces[1] := nil;
      NewEdge.MeshObject := Edge.MeshObject;

      EdgeList.Add(NewEdge);//}
    end;
  end;
end;

function TEdgeDetector.AddNode(const VerletAssembly : TVerletAssembly; const MeshObject: TMeshObject;
  const VertexIndex: integer): TVerletNode;
var
  Location : TAffineVector;
  aNode : TMeshObjectVerletNode;
  i : integer;
begin
  // Is there an identical node?
  Location := MeshObject.Owner.Owner.LocalToAbsolute(MeshObject.Vertices[VertexIndex]);

  for i := 0 to FNodeList.Count-1 do
  begin
    aNode := TMeshObjectVerletNode(FNodeList[i]);

    // TODO : Threshold shouldn't be hardcoded!
    if VectorDistance2(Location, aNode.Location)<=FWeldDistance then
    begin
      FNodeList.Add(aNode);
      aNode.VertexIndices.Add(VertexIndex);
      Result:=aNode;
      Exit;
    end;
  end;//}

  aNode := TMeshObjectVerletNode.Create(VerletAssembly);
  aNode.MeshObject := MeshObject;
  aNode.VertexIndices.Add(VertexIndex);
  aNode.Location := Location;
  aNode.OldLocation := Location;

  FNodeList.Add(aNode);
  Result:=aNode;
end;

procedure TEdgeDetector.ProcessMesh;
begin
  inherited;

  BuildOpposingEdges;
end;

{ TVertexStat }

constructor TVertexStat.Create(const aMeshObject : TMeshObject; const aVertexIndex : integer);
begin
  FFaceList := TFaceList.Create;
  FMeshObject := aMeshObject;
  FVertexIndex := aVertexIndex;
end;

destructor TVertexStat.Destroy;
begin
  FreeAndNil(FFaceList);
end;

procedure TVertexStat.UpdateNormal;
var
  Normal : TAffineVector;
  i : integer;
begin
  Normal := NullVector;

  for i := 0 to FaceList.Count-1 do
    AddVector(Normal, FaceList[i].Normal);//}

  NormalizeVector(Normal);

  MeshObject.Normals[VertexIndex] := Normal;
end;

{ TFaceSmoother }

function TFaceSmoother.AddFace(const Vi0, Vi1, Vi2: integer;
  const MeshObject: TMeshObject): TFace;
var
  Face : TFace;
begin
  Face := inherited AddFace(Vi0, Vi1, Vi2, MeshObject);

  FTempList[Vi0].FaceList.Add(Face);
  FTempList[Vi1].FaceList.Add(Face);
  FTempList[Vi2].FaceList.Add(Face);

  result := Face;
end;

procedure TFaceSmoother.Clear;
var
  i : integer;
begin
  inherited;

  for i := 0 to VertextStatsList.Count-1 do
    VertextStatsList[i].Free;

  VertextStatsList.Clear;
  FTempList.Clear;
end;

constructor TFaceSmoother.Create(const aGLBaseMesh: TGLBaseMesh);
begin
  inherited;
  FVertextStatsList := TVertextStatsList.Create;
  FTempList := TVertextStatsList.Create;
end;

destructor TFaceSmoother.Destroy;
begin
  inherited;

  FreeAndNil(FVertextStatsList);
  FreeAndNil(FTempList);
end;

procedure TFaceSmoother.ProcessMeshObject(const MeshObject : TMeshObject); 
var
  iVertice : integer;
  VertexStat : TVertexStat;
begin
  FTempList.Clear;
  for iVertice := 0 to MeshObject.Vertices.Count-1 do
  begin
    VertexStat := TVertexStat.Create(MeshObject, iVertice);
    VertextStatsList.Add(VertexStat);

    // A sorted list where the indexes are the same as the original vertex
    // indices
    FTempList.Add(VertexStat);
  end;

  inherited;
end;

procedure TFaceSmoother.UpdateNormals;
var
  i : integer;
begin
  for i := 0 to FaceList.Count-1 do
    FaceList[i].UpdateNormal;

  for i := 0 to VertextStatsList.Count-1 do
    VertextStatsList[i].UpdateNormal;
end;

{ TFace }

constructor TFace.Create(aMeshObject: TMeshObject);
begin
  MeshObject := aMeshObject;
end;

procedure TFace.UpdateNormal;
begin
  CalcPlaneNormal(
    MeshObject.Vertices[Vertices[0]],
    MeshObject.Vertices[Vertices[1]],
    MeshObject.Vertices[Vertices[2]], Normal);
end;

{ TVertextStatsList }

function TVertextStatsList.GetItems(i: integer): TVertexStat;
begin
  result := Get(i);
end;

procedure TVertextStatsList.SetItems(i: integer;
  const Value: TVertexStat);
begin
  put(i, Value);
end;
end.
