{: SpatialPartitioning<p>

	Spatial Partitioning speeds up geometrical queries, like what objects does A
  overlap.<p>

  Nota that the class TOctreeSpacePartition is optimized for dynamic scenes with
  objects that are small in relation to the size of the Octree space. This from
  Eric;

  <i>The non-duplicating octree shouldn't really be used if  you have big objects,
  and this especially if you have lots of big objects (the more objects you have
  the less efficient the partitionning, due to the "magnifying glass" effect of
  the non-discriminating volume).</i><p>


	<b>History : </b><font size=-1><ul>
      <li>23/06/03 - MF - Separated functionality for Octrees and general
                          sectored space partitions so Quadtrees will be easy
                          to add.
      <li>20/06/03 - MF - Created
  </ul></font>
}

unit SpatialPartitioning;

interface

uses
  Classes, Geometry, SysUtils, GeometryBB, PersistentClasses;

const
  cOctree_LEAF_TRHESHOLD = 30;
  cOctree_MAX_TREE_DEPTH = 8;
  cOctree_GROW_GRAVY = 0.1;

type
  TBaseSpacePartition = class;

  {: Used to store the actual objects in the SpacePartition }
  TSpacePartitionLeaf = class(TPersistentObject)
  private
    FSpacePartition : TBaseSpacePartition;
    procedure SetSpacePartition(const Value: TBaseSpacePartition);
  public
    {: This can be used by the space partitioner as it sees fit}
    FPartitionTag : pointer;
    {: Leaves cache their AABBs so they can easily be accessed when needed by
    the space partitioner }
    FCachedAABB : TAABB;
    {: Leaves cache their BoundingSpheres so they can easily be accessed when
    needed by the space partitioner }
    FCachedBSphere : TBSphere;

    {: Whenever the size or location of the leaf changes, the space partitioner
    should be notified through a call to Changed. In the basic version, all it
    does is update the cached AABB and BSphere. You do not need to override this
    method.}
    procedure Changed; virtual;

    // *******************
    // *** Override this!
    {: AABBs and BSpheres are cached for leafs, and this function should be
    overriden to update the cache from the structure that the leaf stores. This
    is the only function you MUST override to use space partitions.}
    procedure UpdateCachedAABBAndBSphere; virtual;

    {: The TBaseSpacePartition that owns this leaf}
    property SpacePartition : TBaseSpacePartition read FSpacePartition write SetSpacePartition;

    {: This tag can be used by the space partition to store vital information
    in the leaf}
    property PartitionTag : pointer read FPartitionTag;

    constructor Create(SpacePartition : TBaseSpacePartition);
    destructor Destroy; override;
  published
  end;

  {: List for storing space partition leaves}
  TSpacePartitionLeafList = class(TPersistentObjectList)
  private
    function GetItems(i: integer): TSpacePartitionLeaf;
    procedure SetItems(i: integer; const Value: TSpacePartitionLeaf);
  public
    property Items[i : integer] : TSpacePartitionLeaf read GetItems write SetItems; default;
  end;

  {: Basic space partition, does not implement any actual space partitioning }
  TBaseSpacePartition = class(TPersistentObject)
  protected
    FQueryResult: TSpacePartitionLeafList;
    FQueryInterObjectTests : integer;

    {: Empties the search result and resetting all search statistics }
    procedure FlushQueryResult; virtual;
  public
    {: The results from the last query }
    property QueryResult : TSpacePartitionLeafList read FQueryResult;

    {: Clear all internal storage Leaves }
    procedure Clear; virtual;

    // ** Update space partition
    {: Add a leaf}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); virtual;
    {: Remove a leaf}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); virtual;
    {: Called by leaf when it has changed}
    procedure LeafChanged(aLeaf : TSpacePartitionLeaf); virtual;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult}
    function QueryAABB(const aAABB : TAABB) : integer; virtual;
    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult}
    function QueryBSphere(const aBSphere : TBSphere) : integer; virtual;
    {: Query space for Leaves that intersect the bounding sphere or box
    of a leaf. Result is returned through QueryResult}
    function QueryLeaf(const aLeaf : TSpacePartitionLeaf) : integer; virtual;

    {: Once a query has been run, this number tells of how many inter object
    tests that were run. This value must be set by all that override the
    queries. }
    property QueryInterObjectTests : integer read FQueryInterObjectTests;

    {: Some space partitioners delay processing changes until all changes have
    been made. ProcessUpdated should be called when all changes have been
    performed. }
    procedure ProcessUpdated; virtual;

    constructor Create; override;
    destructor Destroy; override;
  end;

  {: Implements a list of all leaves added to the space partition, _not_ a
  good solution, but it can be used as a benchmark against more complex methods}
  TLeavedSpacePartition = class(TBaseSpacePartition)
  private
    FLeaves : TSpacePartitionLeafList;

  public
    {: Clear all internal storage Leaves }
    procedure Clear; override;

    // ** Update space partition
    {: Add a leaf}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); override;
    {: Remove a leaf}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); override;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult. This override scans _all_ leaves
    in the list, so it's far from optimal.}
    function QueryAABB(const aAABB : TAABB) : integer; override;
    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult. This override scans _all_ leaves
    in the list, so it's far from optimal.}
    function QueryBSphere(const aBSphere : TBSphere) : integer; override;

    constructor Create; override;
    destructor Destroy; override;
  published
    property Leaves : TSpacePartitionLeafList read FLeaves;
  end;

  TSectoredSpacePartition = class;
  TSectorNode = class;
  TSectorNodeArray = array[0..7] of TSectorNode;

  {: Implements a SectorNode node. Each node can have 0 or 8 children, each child
  being a portion of the size of the parent. For quadtrees, that's 1/4, for
  octrees, it's 1/8 }
  TSectorNode = class
  private
    FLeaves : TSpacePartitionLeafList;
    FAABB : TAABB;
    FCenter : TAffineVector;
    FSectoredSpacePartition : TSectoredSpacePartition;
    FRecursiveLeafCount: integer;
    FParent: TSectorNode;
    FNodeDepth : integer;
    FChildCount : integer;
    FChildren: TSectorNodeArray;
    function GetNoChildren: boolean;
    procedure SetAABB(const Value: TAABB);
  protected
    {: Recursively counts the RecursiveLeafCount, this should only be used in
    debugging purposes, because the proprtyu RecursiveLeafCount is always up to
    date.}
    function CalcRecursiveLeafCount : integer;

    {: Places a leaf in one of the children of this node, or in the node itself
    if it doesn't fit in any of the children }
    function PlaceLeafInChild(aLeaf : TSpacePartitionLeaf ) : TSectorNode;

    {: Debug method that checks that FRecursiveLeafCount and
    CalcRecursiveLeafCount actually agree }
    function VerifyRecursiveLeafCount : string;
  public
    {: Clear deletes all children and empties the leaves. It doesn't destroy
    the leaves, as they belong to the SpacePartition}
    procedure Clear;

    {: The Axis Aligned Bounding Box for this node. All leaves MUST fit inside
    this box. }
    property AABB : TAABB read FAABB write SetAABB;
    {: Center of the AABB for this node.}
    property Center : TAffineVector read FCenter;
    {: NoChildren is true if the node has no children.}
    property NoChildren : boolean read GetNoChildren;
    {: A list of the children for this node, only ChildCount children are none
    nil }
    property Children : TSectorNodeArray read FChildren;
    {: The number of child sectors that have been created }
    property ChildCount : integer read FChildCount;

    {: Computes which child the AABB should go in. Returns nil if no such child
    exists }
    function GetChildForAABB(AABB : TAABB) : TSectorNode; virtual;

    {: The leaves that are stored in this node }
    property Leaves : TSpacePartitionLeafList read FLeaves;

    {: The Structure that owns this node }
    property SectoredSpacePartition : TSectoredSpacePartition read FSectoredSpacePartition;

    {: The parent node of this node. If parent is nil, that means that this
    node is the root node }
    property Parent : TSectorNode read FParent;

    {: The number of leaves stored in this node and all it's children.}
    property RecursiveLeafCount : integer read FRecursiveLeafCount;

    {: The tree depth at which this node is located. For the root, this value
    is 0, for the roots children, it is 1 and so on }
    property NodeDepth : integer read FNodeDepth;

    {: Checks if an AABB fits completely inside this node }
    function AABBFitsInNode(const aAABB : TAABB) : boolean; virtual;

    {: Checks if an AABB intersects this node }
    function AABBIntersectsNode(const aAABB : TAABB) : boolean; virtual;

    {: Checks if a BSphere fits completely inside this node }
    function BSphereFitsInNode(const BSphere : TBSphere) : boolean; virtual;

    {: Checks if a BSphere intersects this node }
    function BSphereIntersectsNode(const BSphere : TBSphere) : boolean; virtual;

    {: Checks if a AABB partially or completely contains this sector}
    function AABBContainsSector(const AABB : TAABB) : TSpaceContains; virtual;

    {: Checks if a BSphere partially or completely contains this sector}
    function BSphereContainsSector(const BSphere : TBSphere) : TSpaceContains; virtual;

    {: Checks if this node partially or completely contains a BSphere}
    function ContainsBSphere(const aBSphere : TBSphere) : TSpaceContains; virtual;

    {: Checks if this node partially or completely contains an AABB}
    function ContainsAABB(const aAABB : TAABB) : TSpaceContains; virtual;

    {: Adds leaf to this node - or one of it's children. If the node has enough
    leaves and has no children, children will be created and all leaves will be
    spread among the children. }
    function AddLeaf(aLeaf : TSpacePartitionLeaf) : TSectorNode;

    {: Remove leaf will remove a leaf from this node. If it is determined that
    this node has too few leaves after the delete, it may be collapsed }
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf; OwnerByThis : boolean);

    {: Query the node and it's children for leaves that match the AABB }
    procedure QueryAABB(const aAABB : TAABB; const QueryResult : TSpacePartitionLeafList);

    {: Query the node and it's children for leaves that match the BSphere }
    procedure QueryBSphere(const aBSphere : TBSphere; const QueryResult : TSpacePartitionLeafList);

    {: Adds all leaves to query result without testing if they intersect, and
    then do the same for all children. This is used when QueryAABB or
    QueryBSphere determines that a node fits completely in the searched space}
    procedure AddAllLeavesRecursive(const QueryResult : TSpacePartitionLeafList);

    {: Add children to this node and spread the leaves among it's children }
    procedure ExpandNode;

    {: Create the number of children this node type needs }
    procedure CreateChildren; virtual;

    {: Delete all children for this node, adding their leaves to this node }
    procedure CollapseNode;

    {: Returns the number of nodes in the Octree }
    function GetNodeCount : integer;

    constructor Create(aSectoredSpacePartition : TSectoredSpacePartition; aParent : TSectorNode);
    destructor Destroy; override;
  end;

  {: Implements sectored space partitioning, sectored space partitions include
  Octrees, Quadtrees and  BSP-trees }
  TSectoredSpacePartition = class(TLeavedSpacePartition)
  private
    FRootNode : TSectorNode;
    FLeafThreshold: integer;
    FMaxTreeDepth: integer;
    FAutoGrow: boolean;
    FGrowGravy: single;
    procedure SetLeafThreshold(const Value: integer);
    procedure SetMaxTreeDepth(const Value: integer);
  protected
    FQueryNodeTests : integer;

    {: Empties the search result and resetting all search statistics }
    procedure FlushQueryResult; override;
  public
    // ** Update space partition
    {: Add a leaf to the structure. If the leaf doesn't fit in the structure, the
    structure is either grown or an exception is raised. If AutoGrow is set to
    true, the octree will be grown.}
    procedure AddLeaf(aLeaf : TSpacePartitionLeaf); override;

    {: Remove a leaf from the structure.}
    procedure RemoveLeaf(aLeaf : TSpacePartitionLeaf); override;

    {: Called by leaf when it has changed, the leaf will be moved to an
    apropriate node}
    procedure LeafChanged(aLeaf : TSpacePartitionLeaf); override;

    // ** Query space partition
    {: Query space for Leaves that intersect the axis aligned bounding box,
    result is returned through QueryResult. This method simply defers to the
    QueryAABB method of the root node.}
    function QueryAABB(const aAABB : TAABB) : integer; override;

    {: Query space for Leaves that intersect the bounding sphere, result is
    returned through QueryResult. This method simply defers to the
    QueryBSphere method of the root node.}
    function QueryBSphere(const aBSphere : TBSphere) : integer; override;

    {: Query space for Leaves that intersect the bounding sphere or box
    of a leaf. Result is returned through QueryResult}
    function QueryLeaf(const aLeaf : TSpacePartitionLeaf) : integer; override;

    {: After a query has been run, this value will contain the number of nodes
    that were checked during the query }
    property QueryNodeTests : integer read FQueryNodeTests;

    {: Returns the number of nodes in the structure }
    function GetNodeCount : integer;

    {: UpdateOctreeSize will grow and / or shrink the structure to fit the
    current leaves +-gravy}
    procedure UpdateStructureSize(Gravy : single);

    {: Rebuild tree will change the tree to the newAABB size, and completely
    rebuild it }
    procedure RebuildTree(const NewAABB : TAABB);

    {: Returns the _total_ AABB in structure }
    function GetAABB : TAABB;

    {: CreateNewNode creates a new node of the TSectorNode subclass that this
    structure requires }
    function CreateNewNode(aParent : TSectorNode) : TSectorNode; virtual;

    constructor Create; override;
    destructor Destroy; override;
  published
    {: Root TSectorNode that all others stem from }
    property RootNode : TSectorNode read FRootNode;

    {: Determines how deep a tree should be allowed to grow. }
    property MaxTreeDepth : integer read FMaxTreeDepth write SetMaxTreeDepth;

    {: Determines when a node should be split up to form children. }
    property LeafThreshold : integer read FLeafThreshold write SetLeafThreshold;

    {: Determines if the structure should grow with new leaves, or if an exception
    should be raised }
    property AutoGrow : boolean read FAutoGrow write FAutoGrow;

    {: When the structure is recreated because it's no longer large enough to fit
    all leafs, it will become large enough to safely fit all leafs, plus
    GrowGravy. This is to prevent too many grows }
    property GrowGravy : single read FGrowGravy write FGrowGravy;
  end;

  {: Implements sector node that handles octrees}
  TSPOctreeNode = class(TSectorNode)
  public
    {: Create 8 TSPOctreeNode children }
    procedure CreateChildren; override;

    {: Checks if an AABB fits completely inside this node }
    function AABBFitsInNode(const aAABB : TAABB) : boolean; override;

    {: Checks if an AABB intersects this node }
    function AABBIntersectsNode(const aAABB : TAABB) : boolean; override;

    {: Checks if a BSphere fits completely inside this node }
    function BSphereFitsInNode(const BSphere : TBSphere) : boolean; override;

    {: Checks if a BSphere intersects this node }
    function BSphereIntersectsNode(const BSphere : TBSphere) : boolean; override;
  end;

  {: Implements octrees}
  TOctreeSpacePartition = class(TSectoredSpacePartition)
  public
    {: Set size updates the size of the Octree }
    procedure SetSize(const XMin, YMin, ZMin, XMax, YMax, ZMax : single);

    {: CreateNewNode creates a new TSPOctreeNode }
    function CreateNewNode(aParent : TSectorNode) : TSectorNode; override;
  end;

implementation

// This was copied from Octree.pas!
//
// Theory on FlagMax and FlagMin:
// When a node is subdivided, each of the 8 children assumes 1/8th ownership of its
// parent's bounding box (defined by parent extents).  Calculating a child's min/max
// extent only requires 3 values: the parent's min extent, the parent's max extent
// and the midpoint of the parent's extents (since the cube is divided in half twice).
// The following arrays assume that the children are numbered from 0 to 7, named Upper
// and Lower (Upper = top 4 cubes on Y axis, Bottom = lower 4 cubes), Left and Right, and
// Fore and Back (Fore facing furthest away from you the viewer).
// Each node can use its corresponding element in the array to flag the operation needed
// to find its new min/max extent.  Note that min, mid and max refer to an array of
// 3 coordinates (x,y,z); each of which are flagged separately. Also note that these
// flags are based on the Y vector being the up vector.
const
  cMIN = 0;
  cMID = 1;
  cMAX = 2;
   cFlagMAX: array[0..7] of array [0..2] of byte = (
      (cMID,cMAX,cMAX), //Upper Fore Left
      (cMAX,cMAX,cMAX), //Upper Fore Right
      (cMID,cMAX,cMID), //Upper Back Left
      (cMAX,cMAX,cMID), //Upper Back Right

      (cMID,cMID,cMAX), //Lower Fore Left   (similar to above except height/2)
      (cMAX,cMID,cMAX), //Lower Fore Right
      (cMID,cMID,cMID), //Lower Back Left
      (cMAX,cMID,cMID)  //Lower Back Right
    );

   cFlagMIN: array[0..7] of array [0..2] of byte = (
      (cMIN,cMID,cMID), //Upper Fore Left
      (cMID,cMID,cMID), //Upper Fore Right
      (cMIN,cMID,cMIN), //Upper Back Left
      (cMID,cMID,cMIN), //Upper Back Right

      (cMIN,cMIN,cMID), //Lower Fore Left  (similar to above except height/2)
      (cMID,cMIN,cMID), //Lower Fore Right
      (cMIN,cMIN,cMIN), //Lower Back Left
      (cMID,cMIN,cMIN)  //Lower Back Right
    );

{ TSpacePartitionLeaf }

procedure TSpacePartitionLeaf.UpdateCachedAABBAndBSphere;
begin
  // You MUST override TSpacePartitionLeaf.UpdateCachedAABBAndBSphere, if you
  // only have easy access to a bounding sphere, or only an axis aligned
  // bounding box, you can easily convert from one to the other by using
  // AABBToBSphere and BSphereToAABB.
  //
  // You MUST set both FCachedAABB AND FCachedBSphere
  Assert(false, 'You MUST override TSpacePartitionLeaf.UpdateCachedAABBAndBSphere!');
end;

procedure TSpacePartitionLeaf.Changed;
begin
  UpdateCachedAABBAndBSphere;
  SpacePartition.LeafChanged(self);
end;

constructor TSpacePartitionLeaf.Create(
  SpacePartition: TBaseSpacePartition);
begin
  inherited Create;
  
  FSpacePartition := SpacePartition;
  SpacePartition.AddLeaf(self);
end;

destructor TSpacePartitionLeaf.Destroy;
begin
  if Assigned(FSpacePartition) then
    FSpacePartition.RemoveLeaf(self);

  inherited;
end;

procedure TSpacePartitionLeaf.SetSpacePartition(
  const Value: TBaseSpacePartition);
begin
  if Assigned(FSpacePartition) then
    FSpacePartition.RemoveLeaf(self);

  FSpacePartition := Value;

  if Assigned(FSpacePartition) then
    FSpacePartition.AddLeaf(self);
end;

{ TSpacePartitionLeafList }

function TSpacePartitionLeafList.GetItems(i: integer): TSpacePartitionLeaf;
begin
  result := TSpacePartitionLeaf(Get(i));
end;

procedure TSpacePartitionLeafList.SetItems(i: integer;
  const Value: TSpacePartitionLeaf);
begin
  Put(i, Value);
end;

{ TBaseSpacePartition }

procedure TBaseSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
  aLeaf.UpdateCachedAABBAndBSphere;
end;

procedure TBaseSpacePartition.Clear;
begin
  // Virtual
end;

constructor TBaseSpacePartition.Create;
begin
  inherited;

  FQueryResult := TSpacePartitionLeafList.Create
end;

destructor TBaseSpacePartition.Destroy;
begin
  FreeAndNil(FQueryResult);
  inherited;
end;

procedure TBaseSpacePartition.FlushQueryResult;
begin
  FQueryResult.Count := 0;
  FQueryInterObjectTests := 0;
end;

procedure TBaseSpacePartition.LeafChanged(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
end;

procedure TBaseSpacePartition.ProcessUpdated;
begin
  // Virtual
end;

function TBaseSpacePartition.QueryAABB(const aAABB : TAABB): integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryBSphere(const aBSphere : TBSphere) : integer;
begin
  // Virtual
  result := 0;
end;

function TBaseSpacePartition.QueryLeaf(
  const aLeaf: TSpacePartitionLeaf): integer;
begin
  QueryBSphere(aLeaf.FCachedBSphere);
  // Remove self if it was included (it should have been)
  FQueryResult.Remove(aLeaf);
  result := FQueryResult.Count;
end;

procedure TBaseSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  // Virtual
end;

{ TLeavedSpacePartition }

procedure TLeavedSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  FLeaves.Add(aLeaf);
  aLeaf.UpdateCachedAABBAndBSphere;
end;

procedure TLeavedSpacePartition.Clear;
var
  i : integer;
begin
  inherited;

  for i := 0 to FLeaves.Count-1 do
  begin
    FLeaves[i].FSpacePartition := nil;
    FLeaves[i].Free;
  end;

  FLeaves.Clear;
end;

constructor TLeavedSpacePartition.Create;
begin
  inherited;

  FLeaves := TSpacePartitionLeafList.Create;
end;

destructor TLeavedSpacePartition.Destroy;
begin
  Clear;
  FreeAndNil(FLeaves);

  inherited;
end;

procedure TLeavedSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  FLeaves.Remove(aLeaf);
end;

function TLeavedSpacePartition.QueryAABB(const aAABB: TAABB): integer;
var
  i : integer;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    inc(FQueryInterObjectTests);

    if IntersectAABBsAbsolute(aAABB, Leaves[i].FCachedAABB) then
      FQueryResult.Add(Leaves[i]);
  end;

  result := FQueryResult.Count;
end;

function TLeavedSpacePartition.QueryBSphere(const aBSphere : TBSphere) : integer;
var
  i : integer;
  Distance2 : single;

  Leaf : TSpacePartitionLeaf;
begin
  // Very brute force!
  FlushQueryResult;

  for i := 0 to Leaves.Count-1 do
  begin
    Leaf := Leaves[i];
    Distance2 := VectorDistance2(Leaf.FCachedBSphere.Center, aBSphere.Center);

    inc(FQueryInterObjectTests);

    if Distance2<sqr(Leaf.FCachedBSphere.Radius + aBSphere.Radius) then
      FQueryResult.Add(Leaf);
  end;

  result := FQueryResult.Count;
end;

{ TSectorNode }

function TSectorNode.AABBFitsInNode(const aAABB: TAABB): boolean;
begin
  result := ContainsAABB(aAABB) in [scContainsFully];
end;

function TSectorNode.AABBIntersectsNode(const aAABB: TAABB): boolean;
begin
  result := ContainsAABB(aAABB) in [scContainsPartially, scContainsFully];
end;

procedure TSectorNode.AddAllLeavesRecursive(const QueryResult : TSpacePartitionLeafList);
var
  i : integer;
begin
  for i := 0 to FLeaves.Count-1 do
    QueryResult.Add(FLEaves[i]);

  for i := 0 to FChildCount-1 do
    FChildren[i].AddAllLeavesRecursive(QueryResult);
end;

function TSectorNode.AddLeaf(aLeaf: TSpacePartitionLeaf): TSectorNode;
begin
  // Time to grow the node?
  if NoChildren and
    (FLeaves.Count>=FSectoredSpacePartition.FLeafThreshold) and
    (FNodeDepth<FSectoredSpacePartition.FMaxTreeDepth) then
  begin
    ExpandNode;
  end;

  inc(FRecursiveLeafCount);

  if NoChildren then
  begin
    FLeaves.Add(aLeaf);
    aLeaf.FPartitionTag := self;
    result := self;
  end else
  begin
    // Does it fit completely in any of the children?
    result := PlaceLeafInChild(aLeaf);
  end;
end;

function TSectorNode.BSphereFitsInNode(const BSphere: TBSphere): boolean;
begin
  result := ContainsBSphere(BSphere) in [scContainsFully];
end;

function TSectorNode.BSphereIntersectsNode(const BSphere: TBSphere): boolean;
begin
  result := ContainsBSphere(BSphere) in [scContainsPartially, scContainsFully];
end;

function TSectorNode.CalcRecursiveLeafCount: integer;
var
  i : integer;
begin
  result := FLeaves.Count;

  for i := 0 to FChildCount-1 do
    result := result + FChildren[i].CalcRecursiveLeafCount;
end;

procedure TSectorNode.Clear;
var
  i : integer;
begin
  for i := 0 to FChildCount-1 do
    FreeAndNil(FChildren[i]);

  FLeaves.Clear;
end;

constructor TSectorNode.Create(aSectoredSpacePartition : TSectoredSpacePartition; aParent : TSectorNode);
begin
  FLeaves := TSpacePartitionLeafList.Create;
  FChildCount := 0;
  FParent := aParent;
  FSectoredSpacePartition := aSectoredSpacePartition;

  if aParent=nil then
    FNodeDepth := 0
  else
    FNodeDepth := aParent.FNodeDepth + 1;
end;

procedure TSectorNode.ExpandNode;
var
  i : integer;
  OldLeaves : TSpacePartitionLeafList;

begin
  CreateChildren;

  // The children have been added, now move all leaves to the children - if
  // we can
  OldLeaves := FLeaves;
  FLeaves := TSpacePartitionLeafList.Create;

  for i := 0 to OldLeaves.Count-1 do
    PlaceLeafInChild(OldLeaves[i]);

  OldLeaves.Free;
end;

procedure TSectorNode.CollapseNode;
var
  i,j : integer;
begin
  for i := 0 to FChildCount-1 do
  begin
    FChildren[i].CollapseNode;

    for j := 0 to FChildren[i].FLeaves.Count-1 do
    begin
      FChildren[i].FLeaves[j].FPartitionTag := self;
      FLeaves.Add(FChildren[i].FLeaves[j]);
    end;

    FChildren[i].FLeaves.Clear;

    FreeAndNil(FChildren[i]);
  end;

  FChildCount := 0;
end;

destructor TSectorNode.Destroy;
begin
  Clear;
  FreeAndNil(FLeaves);
  inherited;
end;

function TSectorNode.GetNoChildren: boolean;
begin
  result := FChildCount=0;
end;

function TSectorNode.GetNodeCount: integer;
var
  i : integer;
begin
  result := 1;

  for i := 0 to FChildCount-1 do
    result := result + FChildren[i].GetNodeCount;
end;

function TSectorNode.PlaceLeafInChild(aLeaf: TSpacePartitionLeaf)  : TSectorNode;
var
  TestChildNode, ChildNode : TSectorNode;
  i : integer;
begin
  // Which child does it fit in?
  ChildNode := GetChildForAABB(aLeaf.FCachedAABB);

  if ChildNode <> nil then
  begin
    result := ChildNode.AddLeaf(aLeaf);
    exit;
  end;//}
  
  // Doesn't fit the any child
  aLeaf.FPartitionTag := self;
  FLeaves.Add(aLeaf);
  result := self;
end;

procedure TSectorNode.QueryAABB(const aAABB: TAABB;
  const QueryResult: TSpacePartitionLeafList);
var
  i : integer;
  SpaceContains : TSpaceContains;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  SpaceContains := AABBContainsSector(aAABB);

  if SpaceContains = scContainsFully then
  begin
    AddAllLeavesRecursive(QueryResult);
  end else
  if SpaceContains = scContainsPartially then
  begin
    // Add all leaves that overlap
    for i := 0 to FLeaves.Count-1 do
    begin
      inc(FSectoredSpacePartition.FQueryInterObjectTests);

      if IntersectAABBsAbsolute(FLeaves[i].FCachedAABB, aAABB) then
        QueryResult.Add(FLeaves[i]);
    end;

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
      FChildren[i].QueryAABB(aAABB, QueryResult);
  end;
end;

procedure TSectorNode.QueryBSphere(const aBSphere: TBSphere;
  const QueryResult: TSpacePartitionLeafList);
var
  i : integer;
  SpaceContains : TSpaceContains;
begin
  inc(FSectoredSpacePartition.FQueryNodeTests);

  SpaceContains := BSphereContainsSector(aBSphere);

  if SpaceContains = scContainsFully then
  begin
    AddAllLeavesRecursive(QueryResult);
  end else
  if SpaceContains = scContainsPartially then
  begin
    // Add all leaves that overlap
    for i := 0 to FLeaves.Count-1 do
      if BSphereContainsAABB(aBSphere, FLeaves[i].FCachedAABB) <> scNoOverlap then
        QueryResult.Add(FLeaves[i]);

    // Recursively let the children add their leaves
    for i := 0 to FChildCount-1 do
    begin
      inc(FSectoredSpacePartition.FQueryInterObjectTests);

      FChildren[i].QueryBSphere(aBSphere, QueryResult);
    end;
  end;
end;

procedure TSectorNode.RemoveLeaf(aLeaf: TSpacePartitionLeaf; OwnerByThis : boolean);
begin
  dec(FRecursiveLeafCount);

  if OwnerByThis then
    FLeaves.Remove(aLeaf);

  // If there aren't enough leaves anymore, it's time to remove the node!
  if not NoChildren and (FRecursiveLeafCount+1<FSectoredSpacePartition.FLeafThreshold) then
    CollapseNode;

  if Parent<>nil then
    Parent.RemoveLeaf(aLeaf, false);
end;

function TSectorNode.VerifyRecursiveLeafCount : string;
var
  i : integer;
begin
  if FRecursiveLeafCount<>CalcRecursiveLeafCount then
  begin
    result := Format('Node at depth %d mismatches, %d<>%d!',[FNodeDepth, FRecursiveLeafCount, CalcRecursiveLeafCount]);
    exit;
  end;

  for i := 0 to FChildCount-1 do
  begin
    result := FChildren[i].VerifyRecursiveLeafCount;
    if result<>'' then
      exit;
  end;
end;

procedure TSectorNode.CreateChildren;
begin
  Assert(false, 'You must override CreateChildren!');
end;

function TSectorNode.AABBContainsSector(const AABB: TAABB): TSpaceContains;
begin
  result := AABBContainsAABB(AABB, FAABB);
end;

function TSectorNode.BSphereContainsSector(
  const BSphere: TBSphere): TSpaceContains;
begin
  result := BSphereContainsAABB(BSphere, FAABB);
end;

function TSectorNode.ContainsAABB(const aAABB: TAABB): TSpaceContains;
begin
  result := AABBContainsAABB(FAABB, aAABB);
end;

function TSectorNode.ContainsBSphere(
  const aBSphere: TBSphere): TSpaceContains;
begin
  result := AABBContainsBSphere(FAABB, aBSphere);
end;

procedure TSectorNode.SetAABB(const Value: TAABB);
begin
  FAABB := Value;
  FCenter := VectorScale(VectorAdd(FAABB.min,FAABB.max), 0.5);
end;

function TSectorNode.GetChildForAABB(
  AABB: TAABB): TSectorNode;
var
  Location : TAffineVector;
  ChildNode : TSectorNode;
  ChildNodeIndex : integer;
begin
  // Instead of looping through all children, we simply determine on which
  // side of the center node the child is located
  ChildNodeIndex := 0;

  Location := AABB.min;

  // Upper / Lower
  if Location[1]<FCenter[1] then  ChildNodeIndex := 4;

  // Left / Right
  if Location[2]<FCenter[2] then ChildNodeIndex := ChildNodeIndex or 2;

  // Fore / Back
  if Location[0]>FCenter[0] then ChildNodeIndex := ChildNodeIndex or 1;

  ChildNode := FChildren[ChildNodeIndex];

  if ChildNode.AABBFitsInNode(AABB) then
  begin
    result := ChildNode;
    exit;
  end;

  result := nil;
end;

{ TSectoredSpacePartition }

procedure TSectoredSpacePartition.AddLeaf(aLeaf: TSpacePartitionLeaf);
begin
  inherited;
  FRootNode.AddLeaf(aLeaf);

  if not FRootNode.AABBFitsInNode(aLeaf.FCachedAABB) then
  begin
    if FAutoGrow then
      UpdateStructureSize(GrowGravy)
    else
      Assert(False, 'Node is outside Octree!');
  end;
end;

constructor TSectoredSpacePartition.Create;
begin
  FLeafThreshold := cOctree_LEAF_TRHESHOLD;
  FMaxTreeDepth := cOctree_MAX_TREE_DEPTH;

  FRootNode := CreateNewNode(nil);
  FAutoGrow := true;

  FGrowGravy := cOctree_GROW_GRAVY;

  inherited Create;
end;

function TSectoredSpacePartition.CreateNewNode(aParent : TSectorNode) : TSectorNode;
begin
  result := TSectorNode.Create(self, aParent);
end;

destructor TSectoredSpacePartition.Destroy;
begin
  FRootNode.Free;
  inherited;
end;

function TSectoredSpacePartition.GetAABB: TAABB;
var
  i : integer;
begin
  if FLeaves.Count=0 then
  begin
    MakeVector(result.min, 0,0,0);
    MakeVector(result.max, 0,0,0);
  end else
  begin
    result := FLeaves[0].FCachedAABB;

    for i := 1 to FLeaves.Count-1 do
      AddAABB(result, FLeaves[i].FCachedAABB);
  end;
end;

function TSectoredSpacePartition.GetNodeCount: integer;
begin
  result := FRootNode.GetNodeCount;
end;

procedure TSectoredSpacePartition.LeafChanged(aLeaf: TSpacePartitionLeaf);
var
  Node : TSectorNode;
begin
  // If the leaf still fits in the old node, leave it there - or in one of the
  // children
  Node := TSectorNode(aLeaf.FPartitionTag);

  Assert(Node<>nil);

  if Node.AABBFitsInNode(aLeaf.FCachedAABB) then
  begin
    // If the node has children, try to add the leaf to them - otherwise just
    // leave it!
    if not Node.NoChildren then
    begin
      Node.FLeaves.Remove(aLeaf);
      Node.PlaceLeafInChild(aLeaf);
    end;
  end else
  begin
    Node.RemoveLeaf(aLeaf, true);

    // Does this leaf still fit in the Octree?
    if not FRootNode.AABBFitsInNode(aLeaf.FCachedAABB) then
    begin
      if FAutoGrow then
        UpdateStructureSize(cOctree_GROW_GRAVY)
      else
        Assert(False, 'Node is outside Octree!');
    end else
      FRootNode.AddLeaf(aLeaf);
  end;
end;

function TSectoredSpacePartition.QueryAABB(const aAABB: TAABB): integer;
begin
  FlushQueryResult;
  FRootNode.QueryAABB(aAABB, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryBSphere(
  const aBSphere: TBSphere): integer;
begin
  FlushQueryResult;
  FRootNode.QueryBSphere(aBSphere, FQueryResult);
  result := FQueryResult.Count;
end;

function TSectoredSpacePartition.QueryLeaf(
  const aLeaf: TSpacePartitionLeaf): integer;
var
  i : integer;
  Node : TSectorNode;
  TestLeaf : TSpacePartitionLeaf;
begin
  // Query current node and all nodes upwards until we find the root, no need
  // to check intersections, because we know that the leaf partially intersects
  // all it's parents.

  Node := TSectorNode(aLeaf.FPartitionTag);
  FlushQueryResult;

  // First, query downwards!
  Node.QueryAABB(aLeaf.FCachedAABB, QueryResult);

  // Now, query parents and upwards!
  Node := Node.Parent;

  while Node<>nil do
  begin
    inc(FQueryNodeTests);

    // Add all leaves that overlap
    for i := 0 to Node.FLeaves.Count-1 do
    begin
      TestLeaf := Node.FLeaves[i];
      inc(FQueryInterObjectTests);
      if IntersectAABBsAbsolute(TestLeaf.FCachedAABB, aLeaf.FCachedAABB) then
        QueryResult.Add(TestLeaf);
    end;

    // Try the parent
    Node := Node.Parent;
  end;

  QueryResult.Remove(aLeaf);

  result := QueryResult.Count;
end;

procedure TSectoredSpacePartition.RemoveLeaf(aLeaf: TSpacePartitionLeaf);
begin
  inherited;
  TSectorNode(aLeaf.FPartitionTag).RemoveLeaf(aLeaf, true);
end;

procedure TSectoredSpacePartition.SetLeafThreshold(const Value: integer);
begin
  FLeafThreshold := Value;
end;

procedure TSectoredSpacePartition.SetMaxTreeDepth(const Value: integer);
begin
  FMaxTreeDepth := Value;
end;

procedure TSectoredSpacePartition.RebuildTree(const NewAABB : TAABB);
var
  i : integer;
  OldLeaves : TSpacePartitionLeafList;
  AABBSize : TAffineVector;
begin
  // Delete ALL nodes in the tree
  FRootNode.Free;

  FRootNode := CreateNewNode(nil);
  FRootNode.AABB := NewAABB;

  // Insert all nodes again
  OldLeaves := FLeaves;
  FLeaves := TSpacePartitionLeafList.Create;

  // This will cause an except if the build goes badly, which is better than
  // an infinite loop
  FAutoGrow := false;

  for i := 0 to OldLeaves.Count-1 do
    AddLeaf(OldLeaves[i]);

  FAutoGrow := true;
end;

procedure TSectoredSpacePartition.UpdateStructureSize(Gravy: single);
var
  MaxAABB, NewAABB : TAABB;
  AABBSize : TAffineVector;
begin
  // Create the new extents for the Octree
  MaxAABB := GetAABB;
  AABBSize := VectorSubtract(MaxAABB.max, MaxAABB.min);

  NewAABB.min := VectorSubtract(MaxAABB.min, VectorScale(AABBSize, Gravy));
  NewAABB.max := VectorAdd(MaxAABB.max, VectorScale(AABBSize, Gravy));//}

  RebuildTree(NewAABB);
end;

procedure TSectoredSpacePartition.FlushQueryResult;
begin
  inherited;

  FQueryNodeTests := 0;
end;

{ TSPOctreeNode }

function TSPOctreeNode.AABBFitsInNode(const aAABB: TAABB): boolean;
begin
  // Faster than inherited method
  result := AABBFitsInAABBAbsolute(aAABB, FAABB);
end;

function TSPOctreeNode.AABBIntersectsNode(const aAABB: TAABB): boolean;
begin
  // Faster than inherited method
  result := IntersectAABBsAbsolute(FAABB, aAABB);
end;

function TSPOctreeNode.BSphereFitsInNode(const BSphere: TBSphere): boolean;
var
  AABB : TAABB;
begin
  // Faster than inherited method
  BSphereToAABB(BSphere, AABB);
  result := AABBFitsInAABBAbsolute(AABB, FAABB);//}
end;

function TSPOctreeNode.BSphereIntersectsNode(
  const BSphere: TBSphere): boolean;
var
  AABB : TAABB;
begin
  // Faster than inherited method
  BSphereToAABB(BSphere, AABB);
  result := IntersectAABBsAbsolute(AABB, FAABB);//}
end;

procedure TSPOctreeNode.CreateChildren;
var
  i : integer;
  AABB : TAABB;
  function GetExtent(const flags: array of byte): TAffineVector;
  var
    n: integer;
  begin
     for n:=0 to 2 do begin
       case flags[n] of
         cMIN: result[n]:=FAABB.min[n];
         cMID: result[n]:=(FAABB.max[n]+FAABB.min[n])/2;
         cMAX: result[n]:=FAABB.max[n];
       end;
     end;
  end;
begin
  for i := 0 to 7 do
  begin
    FChildren[i] := FSectoredSpacePartition.CreateNewNode(self);

    //Generate new extents based on parent's extents
    AABB.min :=GetExtent(cFlagMin[i]);
    AABB.max :=GetExtent(cFlagMax[i]);
    FChildren[i].AABB := AABB;
  end;

  FChildCount := 8;
end;

{ TOctreeSpacePartition }

function TOctreeSpacePartition.CreateNewNode(
  aParent: TSectorNode): TSectorNode;
begin
  result := TSPOctreeNode.Create(self, aParent);
end;

procedure TOctreeSpacePartition.SetSize(const XMin, YMin, ZMin, XMax, YMax,
  ZMax: single);
var
  AABB : TAABB;
begin
  MakeVector(AABB.Min, XMin, YMin, ZMin);
  MakeVector(AABB.Max, XMax, YMax, ZMax);

  RebuildTree(AABB);
end;
end.
