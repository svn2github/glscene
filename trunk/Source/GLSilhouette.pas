// GLSilhouette
{: Enhanced silhouette classes.<p>

   Introduces more evolved/specific silhouette generation and management
   classes.<p>

   CAUTION : both connectivity classes leak memory.<p>

	<b>History : </b><font size=-1><ul>
      <li>10/06/03 - EG - Creation (based on code from Mattias Fagerlund)
   </ul></font>
}
unit GLSilhouette;

interface

uses Classes, GLMisc, Geometry, VectorLists, GLVectorFileObjects;

type

   // TGLSilhouette
   //
   {: Base mesh-oriented silhouette class.<p>
      This class introduces helper methods for constructing the indexed
      vertices sets for the silhouette and the cap. }
   TGLSilhouette = class (TGLBaseSilhouette)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         {: Adds an edge (two vertices) to the silhouette.<p>
            If TightButSlow is true, no vertices will be doubled in the
            silhouette list. This should only be used when creating re-usable
            silhouettes, because it's much slower. }
         procedure AddEdgeToSilhouette(const v0, v1 : TAffineVector;
                                       tightButSlow : Boolean);
         {: Adds a capping triangle to the silhouette.<p>
            If TightButSlow is true, no vertices will be doubled in the
            silhouette list. This should only be used when creating re-usable
            silhouettes, because it's much slower. }
         procedure AddCapToSilhouette(const v0, v1, v2 : TAffineVector;
                                      tightButSlow : Boolean);
   end;

   // TBaseConnectivity
   TBaseConnectivity = class
  private
    FPrecomputeFaceNormal: boolean;
       private
          function GetEdgeCount: integer; virtual;
          function GetFaceCount: integer; virtual;
       public
          property EdgeCount : integer read GetEdgeCount;
          property FaceCount : integer read GetFaceCount;

          property PrecomputeFaceNormal : boolean read FPrecomputeFaceNormal;
          procedure CreateSilhouetteOmni(SeenFrom : TAffineVector; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean; AddCap : boolean); virtual;

          constructor Create(PrecomputeFaceNormal : boolean); virtual;
   end;

   // TConnectivity
   //
   TConnectivity = class(TBaseConnectivity)
       private
          { All storage of faces and adges are cut up into tiny pieces for a reason,
          it'd be nicer with Structs or classes, but it's actually faster this way.
          The reason it's faster is because of less cache overwrites when we only
          access a tiny bit of a triangle (for instance), not all data.}
          FEdgeVertices : TIntegerList;
          FEdgeFaces : TIntegerList;

          FFaceVisible : TIntegerList;

          FFaceVertexIndex : TIntegerList;

          FFaceNormal : TAffineVectorList;

          FVertexMemory : TIntegerList;

          FVertices : TAffineVectorList;

          function GetEdgeCount: integer; override;
          function GetFaceCount: integer; override;

          function ReuseOrFindVertexID(SeenFrom : TAffineVector; aSilhouette: TGLSilhouette;
            Index: integer): integer;
       public
          {: Clears out all connectivity information. }
          procedure Clear; virtual;

          procedure CreateSilhouetteOmni(SeenFrom : TAffineVector; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean; AddCap : boolean); override;

          function AddIndexedEdge(VertexIndex0, VertexIndex1 : integer; FaceID: integer) : integer;
          function AddIndexedFace(Vi0, Vi1, Vi2 : integer) : integer;

          function AddFace(Vertex0, Vertex1, Vertex2 : TAffineVector) : integer;
          function AddQuad(Vertex0, Vertex1, Vertex2, Vertex3 : TAffineVector) : integer;

          // TODO : Similar method for parallel lights

          property EdgeCount : integer read GetEdgeCount;
          property FaceCount : integer read GetFaceCount;

          constructor Create(PrecomputeFaceNormal : boolean); override;
          destructor Destroy; override;
   end;



   // TFaceGroupConnectivity
   //
   TFaceGroupConnectivity = class(TConnectivity)
       private
          FMeshObject : TMeshObject;

       public
          procedure Clear; override;

          {: Builds the connectivity information. }
          procedure RebuildEdgeList;

          constructor Create(aMeshObject : TMeshObject; PrecomputeFaceNormal : boolean);
          destructor Destroy; override;
   end;

   // TGLBaseMeshConnectivity
   //
   TGLBaseMeshConnectivity = class(TBaseConnectivity)
       private
          FGLBaseMesh : TGLBaseMesh;
          FFaceGroupConnectivityList : TList;
          function GetEdgeCount: integer; override;
          function GetFaceCount: integer; override;
          function GetFaceGroupConnectivity(i: integer): TFaceGroupConnectivity;
          function GetConnectivityCount: integer;

       public
          property ConnectivityCount : integer read GetConnectivityCount;
          property FaceGroupConnectivity[i : integer] : TFaceGroupConnectivity read GetFaceGroupConnectivity;

          procedure Clear(SaveFaceGroupConnectivity : boolean);

          {: Builds the connectivity information. }
          procedure RebuildEdgeList;

          procedure CreateSilhouetteOmni(SeenFrom : TAffineVector; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean; AddCap : boolean); override;

          constructor Create(aGLBaseMesh : TGLBaseMesh);
          destructor Destroy; override;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TGLSilhouette ------------------
// ------------------

// AddEdgeToSilhouette
//
procedure TGLSilhouette.AddEdgeToSilhouette(const v0, v1 : TAffineVector;
                                            tightButSlow : Boolean);
begin
   if tightButSlow then
      Indices.Add(Vertices.FindOrAddPoint(v0),
                  Vertices.FindOrAddPoint(v1))
   else Indices.Add(Vertices.Add(v0, 1),
                    Vertices.Add(v1, 1));
end;

// AddCapToSilhouette
//
procedure TGLSilhouette.AddCapToSilhouette(const v0, v1, v2 : TAffineVector;
                                           tightButSlow : Boolean);
begin
   if tightButSlow then
      CapIndices.Add(Vertices.FindOrAddPoint(v0),
                     Vertices.FindOrAddPoint(v1),
                     Vertices.FindOrAddPoint(v2))
   else CapIndices.Add(Vertices.Add(v0, 1),
                       Vertices.Add(v1, 1),
                       Vertices.Add(v2, 1));
end;

// ------------------
// ------------------ TBaseConnectivity ------------------
// ------------------

{ TBaseConnectivity }

constructor TBaseConnectivity.Create(PrecomputeFaceNormal: boolean);
begin
  FPrecomputeFaceNormal := PrecomputeFaceNormal;
end;

procedure TBaseConnectivity.CreateSilhouetteOmni(SeenFrom: TAffineVector;
  var aSilhouette: TGLSilhouette; AddToSilhouette, AddCap: boolean);
begin
  // Purely virtual!
end;

// ------------------
// ------------------ TConnectivity ------------------
// ------------------

function TBaseConnectivity.GetEdgeCount: integer;
begin
  result := 0;
end;

function TBaseConnectivity.GetFaceCount: integer;
begin
  result := 0;
end;

{ TConnectivity }

constructor TConnectivity.Create(PrecomputeFaceNormal : boolean);
begin
  FFaceVisible := TIntegerList.Create;

  FFaceVertexIndex := TIntegerList.Create;
  FFaceNormal := TAffineVectorList.Create;

  FEdgeVertices := TIntegerList.Create;
  FEdgeFaces := TIntegerList.Create;

  FPrecomputeFaceNormal := PrecomputeFaceNormal;

  FVertexMemory := TIntegerList.Create;

  FVertices := TAffineVectorList.Create;
end;

destructor TConnectivity.Destroy;
begin
  Clear;

  FFaceVisible.Free;
  FFaceVertexIndex.Free;
  FFaceNormal.Free;

  FEdgeVertices.Free;
  FEdgeFaces.Free;

  FVertexMemory.Free;

  if Assigned(FVertices) then
    FVertices.Free;

  inherited;
end;

procedure TConnectivity.Clear;
begin
  FEdgeVertices.Clear;
  FEdgeFaces.Clear;
  FFaceVisible.Clear;
  FFaceVertexIndex.Clear;
  FFaceNormal.Clear;
  FVertexMemory.Clear;

  if FVertices<>nil then
    FVertices.Clear;
end;

procedure TConnectivity.CreateSilhouetteOmni(
  SeenFrom: TAffineVector; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean; AddCap : boolean);
var
  i : integer;
  V0, V1, V2 : TAffineVector;
  Vi0, Vi1, Vi2 : integer;
  tVi0, tVi1, tVi2 : integer;
  FaceNormal : TAffineVector;

  dot : single;

  Face0ID, Face1ID : integer;
begin
  if aSilhouette=nil then
    aSilhouette:=TGLSilhouette.Create;

  if not AddToSilhouette then
    aSilhouette.Flush;

  // Clear the vertex memory
  FVertexMemory.Flush;

  // Update visibility information for all Faces
  for i := 0 to FaceCount-1 do
  begin
    // Retrieve the vertex indices
    Vi0 := FFaceVertexIndex[i * 3 + 0];
    Vi1 := FFaceVertexIndex[i * 3 + 1];
    Vi2 := FFaceVertexIndex[i * 3 + 2];

    V0 := FVertices[Vi0];

    if FPrecomputeFaceNormal then
      FaceNormal := FFaceNormal[i]
    else
    begin
      // Retrieve the last vertices
      V1 := FVertices[Vi1];
      V2 := FVertices[Vi2];

      FaceNormal :=
        CalcPlaneNormal(V0, V1, V2);
    end;

    dot := PointProject(SeenFrom, V0, FaceNormal);

    if (dot>=0) then
      FFaceVisible[i] := 1
    else
      FFaceVisible[i] := 0;

    if AddCap and (dot<0) then
    begin
      tVi0 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi0);
      tVi1 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi1);
      tVi2 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi2);

      aSilhouette.CapIndices.Add(tVi0, tVi1, tVi2);
    end;
  end;

  for i := 0 to EdgeCount-1 do
  begin
    Face0ID := FEdgeFaces[i * 2 + 0];
    Face1ID := FEdgeFaces[i * 2 + 1];//}

    if (Face1ID = -1) or (FFaceVisible[Face0ID] <> FFaceVisible[Face1ID]) then
    begin
      // Retrieve the two vertice values add add them to the Silhouette list
      Vi0 := FEdgeVertices[i*2 + 0];
      Vi1 := FEdgeVertices[i*2 + 1];

      // In this moment, we _know_ what vertex id the vertex had in the old
      // mesh. We can remember this information and re-use it for a speedup
      if (FFaceVisible[Face0ID]=0) then
      begin
        tVi0 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi0);
        tVi1 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi1);

        aSilhouette.Indices.Add(tVi0, tVi1);
      end
      else
        if Face1ID>-1 then
        begin
          tVi0 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi0);
          tVi1 := ReuseOrFindVertexID(SeenFrom, aSilhouette, Vi1);

          aSilhouette.Indices.Add(tVi1, tVi0);
        end;
    end;
  end;
end;

function TConnectivity.GetEdgeCount: integer;
begin
  result := FEdgeVertices.Count div 2;
end;

function TConnectivity.GetFaceCount: integer;
begin
  result := FFaceVisible.Count;
end;

function TConnectivity.ReuseOrFindVertexID(
  SeenFrom: TAffineVector; aSilhouette: TGLSilhouette;
  Index: integer): integer;
var
  MemIndex, i : integer;
  Vertex : TAffineVector;
  OldCount  : integer;
  List : PIntegerArray;
begin
  // DUMBO VERSION
  // LScene generates;
  //
  // Non capped 146 fps
  // 500 runs = 560,12 ms => 1,12 ms / run
  // aSilhouette.Count=807, vertices=1614
  //
  // Capped 75 fps
  // 500 runs = 1385,33 ms => 2,77 ms / run
  // aSilhouette.Count=807, vertices=10191
  {Vertex := FMeshObject.Owner.Owner.LocalToAbsolute(FMeshObject.Vertices[Index]);
  result := aSilhouette.FVertices.Add(Vertex);
  VertexFar := VectorAdd(Vertex, VectorScale(VectorSubtract(Vertex, SeenFrom), cEXTRUDE_LENGTH));
  aSilhouette.FVertices.Add(VertexFar);
  exit;//}

  // SMARTO VERSION

  //  LScene generates;
  //
  //  Non capped 146 fps
  //  500 runs = 630,06 ms => 1,26 ms / run
  //  aSilhouette.Count=807, vertices=807
  //
  //  Capped 88 fps
  //  500 runs = 1013,29 ms => 2,03 ms / run
  //  aSilhouette.Count=807, vertices=1873
  if Index>=FVertexMemory.Count then
  begin
    OldCount := FVertexMemory.Count;
    FVertexMemory.Count := Index+1;

    List := FVertexMemory.List;
    for i := OldCount to FVertexMemory.Count-1 do
      List[i] := -1;
  end;//}

  MemIndex := FVertexMemory[Index];

  if MemIndex=-1 then
  begin
    // Add the "near" vertex
    Vertex := FVertices[Index];
    MemIndex := aSilhouette.Vertices.Add(Vertex, 1);

    FVertexMemory[Index] := MemIndex;
    result := MemIndex;
  end else
    result := MemIndex;//}
end;

function TConnectivity.AddIndexedEdge(VertexIndex0,
  VertexIndex1: integer; FaceID: integer) : integer;
var
  i : integer;
  EdgeVi0, EdgeVi1 : integer;
begin
  // Make sure that the edge doesn't already exists
  for i := 0 to EdgeCount-1 do begin
    // Retrieve the two vertices in the edge
    EdgeVi0 := FEdgeVertices[i*2 + 0];
    EdgeVi1 := FEdgeVertices[i*2 + 1];

    if ((EdgeVi0 = VertexIndex0) and (EdgeVi1 = VertexIndex1)) or
       ((EdgeVi0 = VertexIndex1) and (EdgeVi1 = VertexIndex0)) then begin
      // Update the second Face of the edge and we're done (this _MAY_
      // overwrite a previous Face in a broken mesh)
      FEdgeFaces[i*2 + 1] := FaceID;

      exit;
    end;
  end;

  // No edge was found, create a new one
  FEdgeVertices.Add(VertexIndex0);
  FEdgeVertices.Add(VertexIndex1);

  FEdgeFaces.Add(FaceID);
  FEdgeFaces.Add(-1);

  result := EdgeCount-1;
end;

function TConnectivity.AddIndexedFace(Vi0, Vi1, Vi2: integer) : integer;
var
  FaceID : integer;
  V0, V1, V2 : TAffineVector;
begin
  FFaceVertexIndex.Add(Vi0);
  FFaceVertexIndex.Add(Vi1);
  FFaceVertexIndex.Add(Vi2);

  V0 := FVertices[Vi0];
  V1 := FVertices[Vi1];
  V2 := FVertices[Vi2];

  if FPrecomputeFaceNormal then
    FFaceNormal.Add(CalcPlaneNormal(V0, V1, V2));

  FaceID := FFaceVisible.Add(0);

  AddIndexedEdge(Vi0, Vi1, FaceID);
  AddIndexedEdge(Vi1, Vi2, FaceID);
  AddIndexedEdge(Vi2, Vi0, FaceID);//}

  result := FaceID;
end;

function TConnectivity.AddFace(Vertex0, Vertex1, Vertex2: TAffineVector) : integer;
var
  Vi0, Vi1, Vi2 : integer;
begin
  Vi0 := FVertices.FindOrAdd(Vertex0);
  Vi1 := FVertices.FindOrAdd(Vertex1);
  Vi2 := FVertices.FindOrAdd(Vertex2);

  result := AddIndexedFace(Vi0, Vi1, Vi2);
end;

function TConnectivity.AddQuad(Vertex0, Vertex1, Vertex2,
  Vertex3: TAffineVector): integer;
var
  Vi0, Vi1, Vi2, Vi3 : integer;
begin
  Vi0 := FVertices.FindOrAdd(Vertex0);
  Vi1 := FVertices.FindOrAdd(Vertex1);
  Vi2 := FVertices.FindOrAdd(Vertex2);
  Vi3 := FVertices.FindOrAdd(Vertex3);

  // First face
  result := AddIndexedFace(Vi0, Vi1, Vi2);

  // Second face
  AddIndexedFace(Vi2, Vi3, Vi0);
end;

{ TFaceGroupConnectivity }

// ------------------
// ------------------ TFaceGroupConnectivity ------------------
// ------------------

procedure TFaceGroupConnectivity.Clear;
begin
  if Assigned(FVertices) then
  begin
    FVertices := nil;
    inherited;
    FVertices := FMeshObject.Vertices;
  end else
    inherited;
end;

constructor TFaceGroupConnectivity.Create(aMeshObject: TMeshObject;
  PrecomputeFaceNormal: boolean);
begin
  inherited Create(PrecomputeFaceNormal);

  FVertices.Free;
  FMeshObject := aMeshObject;
  FVertices := FMeshObject.Vertices;

  RebuildEdgeList;
end;

destructor TFaceGroupConnectivity.Destroy;
begin
  // Protect FVertices, because it belongs to FMeshObject
  FVertices := nil;
  inherited;
end;

procedure TFaceGroupConnectivity.RebuildEdgeList;
var
  iFaceGroup, iFace, iVertex  : integer;
  FaceGroup : TFGVertexIndexList;
  List : PIntegerArray;
begin
  // Make sure that the connectivity information is empty
  Clear;

  // Create a list of edges for the meshobject
  for iFaceGroup := 0 to FMeshObject.FaceGroups.Count-1 do
  begin
    Assert(FMeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList,'Method only works for descendants of TFGVertexIndexList.');
    FaceGroup := TFGVertexIndexList(FMeshObject.FaceGroups[iFaceGroup]);

    case FaceGroup.Mode of
      fgmmTriangles, fgmmFlatTriangles :
      begin
        for iFace := 0 to FaceGroup.TriangleCount - 1 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
          AddIndexedFace(List[0], List[1], List[2]);
        end;
      end;
      fgmmTriangleStrip :
      begin
        for iFace:=0 to FaceGroup.VertexIndices.Count-3 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace];
          if (iFace and 1)=0 then
             AddIndexedFace(List[0], List[1], List[2])
          else
             AddIndexedFace(List[2], List[1], List[0]);
        end;
      end;
      fgmmTriangleFan :
      begin
        List := FaceGroup.VertexIndices.List;

        for iVertex:=2 to FaceGroup.VertexIndices.Count-1 do
          AddIndexedFace(List[0], List[iVertex-1], List[iVertex])
      end;
      else
        Assert(false,'Not supported');
    end;
  end;
end;

// ------------------
// ------------------ TGLBaseMeshConnectivity ------------------
// ------------------

procedure TGLBaseMeshConnectivity.RebuildEdgeList;
var
  i : integer;
begin
  for i := 0 to ConnectivityCount - 1 do
    FaceGroupConnectivity[i].RebuildEdgeList;
end;

procedure TGLBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity : boolean);
var
  i : integer;
begin
  if SaveFaceGroupConnectivity then
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].Clear;
  end else
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].Free;

    FFaceGroupConnectivityList.Clear;
  end;
end;

constructor TGLBaseMeshConnectivity.Create(aGLBaseMesh: TGLBaseMesh);
var
  i : integer;
  MO : TMeshObject;
  Connectivity : TFaceGroupConnectivity;
begin
  FFaceGroupConnectivityList := TList.Create;

  // Only precompute normals if the basemesh isn't an actor (because they change)
  FPrecomputeFaceNormal := not (aGLBaseMesh is TGLActor);
  FGLBaseMesh := aGLBaseMesh;

  for i := 0 to aGLBaseMesh.MeshObjects.Count-1 do
  begin
    MO := aGLBaseMesh.MeshObjects[i];

    Connectivity := TFaceGroupConnectivity.Create(MO, FPrecomputeFaceNormal);

    FFaceGroupConnectivityList.Add(Connectivity);
  end;
end;

procedure TGLBaseMeshConnectivity.CreateSilhouetteOmni(SeenFrom : TAffineVector; var aSilhouette : TGLSilhouette; AddToSilhouette : boolean; AddCap : boolean);

var
  i : integer;
begin
  if aSilhouette=nil then
    aSilhouette:=TGLSilhouette.Create
  else
    aSilhouette.Flush;

  for i := 0 to ConnectivityCount-1 do
    FaceGroupConnectivity[i].CreateSilhouetteOmni(SeenFrom, aSilhouette, true, AddCap);
end;

destructor TGLBaseMeshConnectivity.Destroy;
begin
  Clear(false);
  FFaceGroupConnectivityList.Free;

  inherited;
end;

function TGLBaseMeshConnectivity.GetConnectivityCount: integer;
begin
  result := FFaceGroupConnectivityList.Count;
end;

function TGLBaseMeshConnectivity.GetEdgeCount: integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to ConnectivityCount - 1 do
    result := result + FaceGroupConnectivity[i].EdgeCount;
end;

function TGLBaseMeshConnectivity.GetFaceCount: integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to ConnectivityCount - 1 do
    result := result + FaceGroupConnectivity[i].FaceCount;
end;

function TGLBaseMeshConnectivity.GetFaceGroupConnectivity(
  i: integer): TFaceGroupConnectivity;
begin
  result := TFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
end;
end.
