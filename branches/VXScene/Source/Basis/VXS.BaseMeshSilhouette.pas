//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Silhouette classes for VKBaseMesh and FaceGroups.
}

unit VXS.BaseMeshSilhouette;

interface

{$I VXScene.inc}

uses
  System.Classes,
  VXS.VectorGeometry,
  VXS.VectorLists,
  VXS.VectorFileObjects,
  VXS.Silhouette;

type
  TVXFaceGroupConnectivity = class(TConnectivity)
  private
    FMeshObject: TVXMeshObject;
    FOwnsVertices: boolean;
    procedure SetMeshObject(const Value: TVXMeshObject);
  public
    procedure Clear; override;
    { Builds the connectivity information. }
    procedure RebuildEdgeList;
    property MeshObject: TVXMeshObject read FMeshObject write SetMeshObject;
    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aMeshObject: TVXMeshObject; APrecomputeFaceNormal: boolean);
    destructor Destroy; override;
  end;

  TVXBaseMeshConnectivity = class(TBaseConnectivity)
  private
    FBaseMesh: TVXBaseMesh;
    FFaceGroupConnectivityList: TList;
    function GetFaceGroupConnectivity(i: integer): TVXFaceGroupConnectivity;
    function GetConnectivityCount: integer;
    procedure SetBaseMesh(const Value: TVXBaseMesh);
  protected
    function GetEdgeCount: integer; override;
    function GetFaceCount: integer; override;
  public
    property ConnectivityCount: integer read GetConnectivityCount;
    property FaceGroupConnectivity[i: integer]: TVXFaceGroupConnectivity read GetFaceGroupConnectivity;
    property BaseMesh: TVXBaseMesh read FBaseMesh write SetBaseMesh;
    procedure Clear(SaveFaceGroupConnectivity: boolean);
    { Builds the connectivity information. }
    procedure RebuildEdgeList;
    procedure CreateSilhouette(const silhouetteParameters: TVXSilhouetteParameters; var aSilhouette: TVXSilhouette; 
	  AddToSilhouette: boolean); 
    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aBaseMesh: TVXBaseMesh);
    destructor Destroy; override;
  end;

//==================================================================
implementation
//==================================================================

// ------------------
// ------------------ TVXFaceGroupConnectivity ------------------
// ------------------

procedure TVXFaceGroupConnectivity.Clear;
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

constructor TVXFaceGroupConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    inherited;

    FOwnsVertices := true;
  end;

procedure TVXFaceGroupConnectivity.SetMeshObject(const Value: TVXMeshObject);
  begin
    Clear;

    FMeshObject := Value;

    if FOwnsVertices then
      FVertices.Free;

    FVertices := FMeshObject.Vertices;

    FOwnsVertices := false;

    RebuildEdgeList;
  end;

constructor TVXFaceGroupConnectivity.CreateFromMesh(aMeshObject: TVXMeshObject; APrecomputeFaceNormal: boolean);
  begin
    Create(APrecomputeFaceNormal);

    MeshObject := aMeshObject;
  end;

destructor TVXFaceGroupConnectivity.Destroy;
  begin
    if FOwnsVertices then
      FVertices.Free;

    FVertices := nil;
    inherited;
  end;

procedure TVXFaceGroupConnectivity.RebuildEdgeList;
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
// ------------------ TVXBaseMeshConnectivity ------------------
// ------------------

procedure TVXBaseMeshConnectivity.RebuildEdgeList;
  var
    i: integer;
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].RebuildEdgeList;
  end;

procedure TVXBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity: boolean);
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

constructor TVXBaseMeshConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    FFaceGroupConnectivityList := TList.Create;

    inherited;
  end;

constructor TVXBaseMeshConnectivity.CreateFromMesh(aBaseMesh: TVXBaseMesh);
  begin
    Create(not(aBaseMesh is TVXActor));
    BaseMesh := aBaseMesh;
  end;

procedure TVXBaseMeshConnectivity.SetBaseMesh(const Value: TVXBaseMesh);
  var
    i: integer;
    MO: TVXMeshObject;
    Connectivity: TVXFaceGroupConnectivity;
  begin
    Clear(false);

    FBaseMesh := Value;

    // Only precompute normals if the basemesh isn't an actor (because they change)
    FPrecomputeFaceNormal := not(Value is TVXActor);
    FBaseMesh := Value;

    for i := 0 to Value.MeshObjects.Count - 1 do
    begin
      MO := Value.MeshObjects[i];

      if MO.Visible then
      begin
        Connectivity := TVXFaceGroupConnectivity.CreateFromMesh(MO, FPrecomputeFaceNormal);

        FFaceGroupConnectivityList.Add(Connectivity);
      end;
    end;
  end;

procedure TVXBaseMeshConnectivity.CreateSilhouette(const silhouetteParameters: TVXSilhouetteParameters; var aSilhouette: TVXSilhouette; AddToSilhouette: boolean);
var
  i: integer;
begin
  if aSilhouette = nil then
    aSilhouette := TVXSilhouette.Create
  else
    aSilhouette.Flush;

  for i := 0 to ConnectivityCount - 1 do
    FaceGroupConnectivity[i].CreateSilhouette(silhouetteParameters, aSilhouette, true);
end;

destructor TVXBaseMeshConnectivity.Destroy;
  begin
    Clear(false);
    FFaceGroupConnectivityList.Free;

    inherited;
  end;

function TVXBaseMeshConnectivity.GetConnectivityCount: integer;
  begin
    result := FFaceGroupConnectivityList.Count;
  end;

function TVXBaseMeshConnectivity.GetEdgeCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].EdgeCount;
  end;

function TVXBaseMeshConnectivity.GetFaceCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].FaceCount;
  end;

function TVXBaseMeshConnectivity.GetFaceGroupConnectivity(i: integer): TVXFaceGroupConnectivity;
  begin
    result := TVXFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
  end;

end.
