//
// This unit is part of the GLScene Project   
//
{ : VKS.BaseMeshSilhouette<p>

  Silhouette classes for GLBaseMesh and FaceGroups.<p>

  <b>History : </b><font size=-1><ul>
  <li>16/11/10 - Yar - Added mesh visibility checking in TVKBaseMeshConnectivity.SetGLBaseMesh (thanks to dalex65)
  <li>30/03/07 - DaStr - Added $I GLScene.inc
  <li>25/03/07 - DaStr - Renamed parameters in some methods
  (thanks Burkhard Carstens) (Bugtracker ID = 1678658)
  <li>23/03/07 - DaStr - Added explicit pointer dereferencing
  (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
  <li>09/02/04 - MF - Fixed bug where vertices weren't freed when owned
  <li>24/06/03 - MF - Created file from parts of GLShilouette
  </ul></font>
}

unit VKS.BaseMeshSilhouette;

interface

{$I VKScene.inc}

uses
  System.Classes,
  VKS.VectorGeometry, VKS.VectorLists, VKS.VectorFileObjects, VKS.Silhouette;

type
  // TFaceGroupConnectivity
  //
  TFaceGroupConnectivity = class(TConnectivity)
  private
    FMeshObject: TMeshObject;
    FOwnsVertices: boolean;
    procedure SetMeshObject(const Value: TMeshObject);

  public
    procedure Clear; override;

    { : Builds the connectivity information. }
    procedure RebuildEdgeList;

    property MeshObject: TMeshObject read FMeshObject write SetMeshObject;

    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aMeshObject: TMeshObject; APrecomputeFaceNormal: boolean);
    destructor Destroy; override;
  end;

  // TVKBaseMeshConnectivity
  //
  TVKBaseMeshConnectivity = class(TBaseConnectivity)
  private
    FGLBaseMesh: TVKBaseMesh;
    FFaceGroupConnectivityList: TList;
    function GetFaceGroupConnectivity(i: integer): TFaceGroupConnectivity;
    function GetConnectivityCount: integer;
    procedure SetGLBaseMesh(const Value: TVKBaseMesh);

  protected
    function GetEdgeCount: integer; override;
    function GetFaceCount: integer; override;

  public
    property ConnectivityCount: integer read GetConnectivityCount;
    property FaceGroupConnectivity[i: integer]: TFaceGroupConnectivity read GetFaceGroupConnectivity;
    property GLBaseMesh: TVKBaseMesh read FGLBaseMesh write SetGLBaseMesh;

    procedure Clear(SaveFaceGroupConnectivity: boolean);

    { : Builds the connectivity information. }
    procedure RebuildEdgeList;

    procedure CreateSilhouette(const silhouetteParameters: TVKSilhouetteParameters; var aSilhouette: TVKSilhouette; AddToSilhouette: boolean); override;

    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aGLBaseMesh: TVKBaseMesh);
    destructor Destroy; override;
  end;

implementation

{ TFaceGroupConnectivity }

// ------------------
// ------------------ TFaceGroupConnectivity ------------------
// ------------------

procedure TFaceGroupConnectivity.Clear;
  begin
    if Assigned(FVertices) then
    begin
      if FOwnsVertices then
        FVertices.Clear
      else
        FVertices := nil;

      inherited;

      if not FOwnsVertices and Assigned(FMeshObject) then
        FVertices := FMeshObject.Vertices;
    end
    else
      inherited;
  end;

constructor TFaceGroupConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    inherited;

    FOwnsVertices := true;
  end;

procedure TFaceGroupConnectivity.SetMeshObject(const Value: TMeshObject);
  begin
    Clear;

    FMeshObject := Value;

    if FOwnsVertices then
      FVertices.Free;

    FVertices := FMeshObject.Vertices;

    FOwnsVertices := false;

    RebuildEdgeList;
  end;

constructor TFaceGroupConnectivity.CreateFromMesh(aMeshObject: TMeshObject; APrecomputeFaceNormal: boolean);
  begin
    Create(APrecomputeFaceNormal);

    MeshObject := aMeshObject;
  end;

destructor TFaceGroupConnectivity.Destroy;
  begin
    if FOwnsVertices then
      FVertices.Free;

    FVertices := nil;
    inherited;
  end;

procedure TFaceGroupConnectivity.RebuildEdgeList;
  var
    iFaceGroup, iFace, iVertex: integer;
    FaceGroup: TFGVertexIndexList;
    List: PIntegerArray;
  begin
    // Make sure that the connectivity information is empty
    Clear;

    // Create a list of edges for the meshobject
    for iFaceGroup := 0 to FMeshObject.FaceGroups.Count - 1 do
    begin
      Assert(FMeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList, 'Method only works for descendants of TFGVertexIndexList.');
      FaceGroup := TFGVertexIndexList(FMeshObject.FaceGroups[iFaceGroup]);

      case FaceGroup.Mode of
        fgmmTriangles, fgmmFlatTriangles:
          begin
            for iFace := 0 to FaceGroup.TriangleCount - 1 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
              AddIndexedFace(List^[0], List^[1], List^[2]);
            end;
          end;
        fgmmTriangleStrip:
          begin
            for iFace := 0 to FaceGroup.VertexIndices.Count - 3 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace];
              if (iFace and 1) = 0 then
                AddIndexedFace(List^[0], List^[1], List^[2])
              else
                AddIndexedFace(List^[2], List^[1], List^[0]);
            end;
          end;
        fgmmTriangleFan:
          begin
            List := FaceGroup.VertexIndices.List;

            for iVertex := 2 to FaceGroup.VertexIndices.Count - 1 do
              AddIndexedFace(List^[0], List^[iVertex - 1], List^[iVertex])
          end;
      else
        Assert(false, 'Not supported');
      end;
    end;
  end;

// ------------------
// ------------------ TVKBaseMeshConnectivity ------------------
// ------------------

procedure TVKBaseMeshConnectivity.RebuildEdgeList;
  var
    i: integer;
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].RebuildEdgeList;
  end;

procedure TVKBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity: boolean);
  var
    i: integer;
  begin
    if SaveFaceGroupConnectivity then
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Clear;
    end
    else
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Free;

      FFaceGroupConnectivityList.Clear;
    end;
  end;

constructor TVKBaseMeshConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    FFaceGroupConnectivityList := TList.Create;

    inherited;
  end;

constructor TVKBaseMeshConnectivity.CreateFromMesh(aGLBaseMesh: TVKBaseMesh);
  begin
    Create(not(aGLBaseMesh is TVKActor));
    GLBaseMesh := aGLBaseMesh;
  end;

procedure TVKBaseMeshConnectivity.SetGLBaseMesh(const Value: TVKBaseMesh);
  var
    i: integer;
    MO: TMeshObject;
    Connectivity: TFaceGroupConnectivity;
  begin
    Clear(false);

    FGLBaseMesh := Value;

    // Only precompute normals if the basemesh isn't an actor (because they change)
    FPrecomputeFaceNormal := not(Value is TVKActor);
    FGLBaseMesh := Value;

    for i := 0 to Value.MeshObjects.Count - 1 do
    begin
      MO := Value.MeshObjects[i];

      if MO.Visible then
      begin
        Connectivity := TFaceGroupConnectivity.CreateFromMesh(MO, FPrecomputeFaceNormal);

        FFaceGroupConnectivityList.Add(Connectivity);
      end;
    end;
  end;

procedure TVKBaseMeshConnectivity.CreateSilhouette(const silhouetteParameters: TVKSilhouetteParameters; var aSilhouette: TVKSilhouette; AddToSilhouette: boolean);

  var
    i: integer;
  begin
    if aSilhouette = nil then
      aSilhouette := TVKSilhouette.Create
    else
      aSilhouette.Flush;

    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].CreateSilhouette(silhouetteParameters, aSilhouette, true);
  end;

destructor TVKBaseMeshConnectivity.Destroy;
  begin
    Clear(false);
    FFaceGroupConnectivityList.Free;

    inherited;
  end;

function TVKBaseMeshConnectivity.GetConnectivityCount: integer;
  begin
    result := FFaceGroupConnectivityList.Count;
  end;

function TVKBaseMeshConnectivity.GetEdgeCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].EdgeCount;
  end;

function TVKBaseMeshConnectivity.GetFaceCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].FaceCount;
  end;

function TVKBaseMeshConnectivity.GetFaceGroupConnectivity(i: integer): TFaceGroupConnectivity;
  begin
    result := TFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
  end;

end.
