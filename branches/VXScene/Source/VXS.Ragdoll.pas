//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Base abstract ragdoll class. Should be extended to use any physics system.  
  
}

unit VXS.Ragdoll;

interface

uses
  VXS.Scene, VXS.PersistentClasses, VXS.VectorGeometry, VXS.VectorFileObjects,
  VXS.VectorLists, VXS.Objects, VXS.VectorTypes;

type
  TVXRagdoll = class;
  TVXRagdolBone = class;

  TVXRagdolJoint = class
  end;

  TVXRagdolBoneList = class (TPersistentObjectList)
  private
    
     FRagdoll : TVXRagdoll;
  protected
    
    function GetRagdollBone(Index: Integer) : TVXRagdolBone;
  public
    
    constructor Create(Ragdoll: TVXRagdoll); reintroduce;
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Ragdoll : TVXRagdoll read FRagdoll;
    property Items[Index: Integer] : TVXRagdolBone read GetRagdollBone; default;
	end;

	TVXRagdolBone = class (TVXRagdolBoneList)
  private
    
    FOwner : TVXRagdolBoneList;
    FName : String;
    FBoneID : Integer; //Refering to TVXActor Bone
    FBoundMax: TAffineVector;
    FBoundMin: TAffineVector;
    FBoundBoneDelta: TAffineVector; //Stores the diference from the bone.GlobalMatrix to the center of the bone's bounding box
    FOrigin: TAffineVector;
    FSize: TAffineVector;
    FBoneMatrix: TMatrix;
    FJoint: TVXRagdolJoint;
    FOriginalMatrix: TMatrix; //Stores the Bone.GlobalMatrix before the ragdoll start
    FReferenceMatrix: TMatrix; //Stores the first bone matrix to be used as reference
    FAnchor: TAffineVector; //The position of the joint
    procedure CreateBoundingBox;
    procedure SetAnchor(Anchor: TAffineVector);
    procedure AlignToSkeleton;
    procedure CreateBoundsChild;
    procedure StartChild;
    procedure AlignChild;
    procedure UpdateChild;
    procedure StopChild;
  protected
    
    function GetRagdollBone(Index: Integer) : TVXRagdolBone;
    procedure Start; virtual; abstract;
    procedure Align; virtual; abstract;
    procedure Update; virtual; abstract;
    procedure Stop; virtual; abstract;
  public
    
    constructor CreateOwned(aOwner : TVXRagdolBoneList);
    constructor Create(Ragdoll: TVXRagdoll);
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    property Owner : TVXRagdolBoneList read FOwner;
    property Name : String read FName write FName;
    property BoneID : Integer read FBoneID write FBoneID;
    property Origin : TAffineVector read FOrigin;
    property Size : TAffineVector read FSize;
    property BoneMatrix : TMatrix read FBoneMatrix;
    property ReferenceMatrix : TMatrix read FReferenceMatrix;
    property Anchor : TAffineVector read FAnchor;
    property Joint : TVXRagdolJoint read FJoint write FJoint;
    property Items[Index: Integer] : TVXRagdolBone read GetRagdollBone; default;
	end;

  TVXRagdoll = class(TPersistentObject)
	private
    
    FOwner : TVXBaseMesh;
    FRootBone : TVXRagdolBone;
    FEnabled: Boolean;
    FBuilt: Boolean;
  protected
    
  public
    
    constructor Create(AOwner : TVXBaseMesh); reintroduce;
    destructor Destroy; override;

    procedure WriteToFiler(writer : TVirtualWriter); override;
    procedure ReadFromFiler(reader : TVirtualReader); override;

    { Must be set before build the ragdoll }
    procedure SetRootBone(RootBone: TVXRagdolBone);
    { Create the bounding box and setup the ragdoll do be started later }
    procedure BuildRagdoll;

    procedure Start;
    procedure Update;
    procedure Stop;

    property Owner : TVXBaseMesh read FOwner;
    property RootBone : TVXRagdolBone read FRootBone;
    property Enabled : Boolean read FEnabled;
	end;
//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

{ TVXRagdolBoneList }

constructor TVXRagdolBoneList.Create(Ragdoll: TVXRagdoll);
begin
  inherited Create;
  FRagdoll := Ragdoll;
end;

destructor TVXRagdolBoneList.Destroy;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Destroy;
  inherited;
end;

function TVXRagdolBoneList.GetRagdollBone(Index: Integer): TVXRagdolBone;
begin
  Result:=TVXRagdolBone(List^[Index]);
end;

procedure TVXRagdolBoneList.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
  //Not implemented
end;

procedure TVXRagdolBoneList.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;
  //Not implemented
end;

{ TVXRagdolBone }

constructor TVXRagdolBone.Create(Ragdoll: TVXRagdoll);
begin
  inherited Create(Ragdoll);
end;

procedure TVXRagdolBone.CreateBoundingBox;
var
  bone: TVXSkeletonBone;
  i, j: integer;
  BoneVertices : TAffineVectorList;
  BoneVertex, max,min: TAffineVector;
  invMat, mat: TMatrix;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);

  //Get all vertices weighted to this bone
  BoneVertices:=TAffineVectorList.Create;
  for i:=0 to Ragdoll.Owner.MeshObjects.Count-1 do
  with TVXSkeletonMeshObject(Ragdoll.Owner.MeshObjects[i]) do
    for j:=0 to Vertices.Count-1 do
      if bone.BoneID = VerticesBonesWeights[j][0].BoneID then
        BoneVertices.FindOrAdd(Vertices[j]);

  invMat := bone.GlobalMatrix;
  InvertMatrix(invMat);

  //For each vertex, get the max and min XYZ (Bounding box)
  if BoneVertices.Count > 0 then
  begin
    BoneVertex := VectorTransform(BoneVertices[0], invMat);
    max := BoneVertex;
    min := BoneVertex;
    for i:=1 to BoneVertices.Count-1 do begin
      BoneVertex := VectorTransform(BoneVertices[i], invMat);
      if (BoneVertex.X > max.X) then max.X := BoneVertex.X;
      if (BoneVertex.Y > max.Y) then max.Y := BoneVertex.Y;
      if (BoneVertex.Z > max.Z) then max.Z := BoneVertex.Z;

      if (BoneVertex.X < min.X) then min.X := BoneVertex.X;
      if (BoneVertex.Y < min.Y) then min.Y := BoneVertex.Y;
      if (BoneVertex.Z < min.Z) then min.Z := BoneVertex.Z;
    end;

    FBoundMax := max;
    FBoundMin := min;
    //Get the origin and subtract from the bone matrix
    FBoundBoneDelta := VectorScale(VectorAdd(FBoundMax, FBoundMin), 0.5);
  end else begin
    FBoundMax := NullVector;
    FBoundMin := NullVector;
  end;

  AlignToSkeleton;
  FReferenceMatrix := FBoneMatrix;
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Joint position
  SetAnchor(AffineVectorMake(mat.W));

  BoneVertices.Free; // NEW1
end;

constructor TVXRagdolBone.CreateOwned(aOwner: TVXRagdolBoneList);
begin
	Create(aOwner.Ragdoll);
  FOwner:=aOwner;
  aOwner.Add(Self);
end;

destructor TVXRagdolBone.Destroy;
begin
  inherited;
end;

procedure TVXRagdolBone.AlignToSkeleton;
var
  o: TAffineVector;
  bone: TVXSkeletonBone;
  mat, posMat: TMatrix;
  noBounds: Boolean;
begin
  bone := Ragdoll.Owner.Skeleton.BoneByID(FBoneID);
  noBounds := VectorIsNull(FBoundMax) and VectorIsNull(FBoundMin);
  //Get the bone matrix relative to the Actor matrix
  mat := MatrixMultiply(bone.GlobalMatrix,FRagdoll.Owner.AbsoluteMatrix);
  //Set Rotation
  FBoneMatrix := mat;
  NormalizeMatrix(FBoneMatrix);

  if (noBounds) then
  begin
    FOrigin := AffineVectorMake(mat.W);
    FSize := AffineVectorMake(0.1,0.1,0.1);
  end else begin
    //Set Origin
    posMat := mat;
    posMat.W := NullHmgVector;
    o := VectorTransform(FBoundBoneDelta, posMat);
    FOrigin := VectorAdd(AffineVectorMake(mat.W), o);
    //Set Size
    FSize := VectorScale(VectorSubtract(FBoundMax, FBoundMin),0.9);
    FSize.X := FSize.X*VectorLength(mat.X);
    FSize.Y := FSize.Y*VectorLength(mat.Y);
    FSize.Z := FSize.Z*VectorLength(mat.Z);
  end;
  //Put the origin in the BoneMatrix
  FBoneMatrix.W := VectorMake(FOrigin,1);
end;

function TVXRagdolBone.GetRagdollBone(Index: Integer): TVXRagdolBone;
begin
  Result:=TVXRagdolBone(List^[Index]);
end;

procedure TVXRagdolBone.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;

end;

procedure TVXRagdolBone.StartChild;
var i: integer;
begin
  FOriginalMatrix := Ragdoll.Owner.Skeleton.BoneByID(FBoneID).GlobalMatrix;
  AlignToSkeleton;
  Start;
  for i := 0 to Count-1 do items[i].StartChild;
end;

procedure TVXRagdolBone.UpdateChild;
var i: integer;
begin
  Update;
  for i := 0 to Count-1 do items[i].UpdateChild;
end;

procedure TVXRagdolBone.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TVXRagdolBone.StopChild;
var i: integer;
begin
  Stop;
  Ragdoll.Owner.Skeleton.BoneByID(FBoneID).SetGlobalMatrix(FOriginalMatrix);
  for i := 0 to Count-1 do items[i].StopChild;
end;

procedure TVXRagdolBone.CreateBoundsChild;
var i: integer;
begin
  CreateBoundingBox;
  for i := 0 to Count-1 do items[i].CreateBoundsChild;
end;

procedure TVXRagdolBone.SetAnchor(Anchor: TAffineVector);
begin
  FAnchor := Anchor;
end;

procedure TVXRagdolBone.AlignChild;
var i: integer;
begin
  Align;
  Update;
  for i := 0 to Count-1 do items[i].AlignChild;
end;

{ TVXRagdoll }

constructor TVXRagdoll.Create(AOwner : TVXBaseMesh);
begin
  FOwner := AOwner;
  FEnabled := False;
  FBuilt := False;
end;

destructor TVXRagdoll.Destroy;
begin
  if FEnabled then Stop;
  inherited Destroy;
end;

procedure TVXRagdoll.ReadFromFiler(reader: TVirtualReader);
begin
  inherited;
end;

procedure TVXRagdoll.SetRootBone(RootBone: TVXRagdolBone);
begin
  FRootBone := RootBone;
end;

procedure TVXRagdoll.Start;
begin
  Assert(FBuilt, 'First you need to build the ragdoll. BuildRagdoll;');
  if (FEnabled) then Exit;
  FEnabled:= True;
  //First start the ragdoll in the reference position
  RootBone.StartChild;
  //Now align it to the animation
  RootBone.AlignChild;
  //Now it recalculate the vertices to use as reference
  FOwner.Skeleton.StartRagDoll;
end;

procedure TVXRagdoll.Update;
begin
  if FEnabled then
  begin
    RootBone.UpdateChild;
    FOwner.Skeleton.MorphMesh(true);
  end;
end;

procedure TVXRagdoll.Stop;
begin
  if not FEnabled then Exit;
  FEnabled := False;
  RootBone.StopChild;
  //Restore the old information
  FOwner.Skeleton.StopRagDoll;
  FOwner.Skeleton.MorphMesh(true);
end;

procedure TVXRagdoll.WriteToFiler(writer: TVirtualWriter);
begin
  inherited;

end;

procedure TVXRagdoll.BuildRagdoll;
begin
  Assert(RootBone <> nil, 'First you need to set the root bone. SetRootBone();');
  RootBone.CreateBoundsChild;
  FBuilt := True;
end;

end.
