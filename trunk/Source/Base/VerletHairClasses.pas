unit VerletHairClasses;

interface

uses
  Classes, VerletClasses, VectorTypes, VectorLists, VectorGeometry;

type
  TVerletHair = class
  private
    FNodeList: TVerletNodeList;
    FLinkCount: integer;
    FRootDepth: single;
    FVerletWorld: TVerletWorld;
    FHairLength: single;
    FData: pointer;
    function GetAnchor: TVerletNode;
    function GetRoot: TVerletNode;
    function GetLinkLength: single;
  protected
    procedure BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
  public
    procedure Clear;

    constructor Create(const AVerletWorld : TVerletWorld;
      const ARootDepth, AHairLength : single; ALinkCount : integer;
      const AAnchorPosition, AHairDirection : TAffineVector);

    destructor Destroy; override;

    property NodeList : TVerletNodeList read FNodeList;
    property VerletWorld : TVerletWorld read FVerletWorld;

    property RootDepth : single read FRootDepth;
    property LinkLength : single read GetLinkLength;
    property LinkCount : integer read FLinkCount;
    property HairLength : single read FHairLength;

    property Data : pointer read FData write FData;

    {: Anchor should be nailed down to give the hair stability }
    property Anchor : TVerletNode read GetAnchor;

    {: Root should be nailed down to give the hair stability }
    property Root : TVerletNode read GetRoot;
  end;

implementation

{ TVerletHair }

procedure TVerletHair.BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
var
  i : integer;
  Position : TAffineVector;
  Node, PrevNode : TVerletNode;
  Direction : TAffineVector;
begin
  Clear;

  Direction := VectorNormalize(AHairDirection);

  // Fix the root of the hair
  Position := VectorAdd(AAnchorPosition, VectorScale(Direction, -FRootDepth));
  Node := VerletWorld.CreateOwnedNode(Position);
  NodeList.Add(Node);
  Node.NailedDown := true;
  PrevNode := Node;

  // Now add the links in the hair
  for i := 0 to FLinkCount-1 do
  begin
    Position := VectorAdd(AAnchorPosition, VectorScale(Direction, HairLength * (i/LinkCount)));

    Node := VerletWorld.CreateOwnedNode(Position);
    NodeList.Add(Node);

    // first one is the anchor
    if i=0 then
      Node.NailedDown := true
    else
      // Create the hair link
      VerletWorld.CreateStick(PrevNode, Node);

    PrevNode := Node;
  end;

  // Now we must stiffen the hair with either sticks or springs
  for i := 0 to NodeList.Count-3 do
    VerletWorld.CreateStick(NodeList[i], NodeList[i+2]);
end;

procedure TVerletHair.Clear;
var
  i : integer;
begin
  for i := FNodeList.Count-1 downto 0 do
    FNodeList[i].Free;

  FNodeList.Clear;
end;

constructor TVerletHair.Create(const AVerletWorld : TVerletWorld;
      const ARootDepth, AHairLength : single; ALinkCount : integer;
      const AAnchorPosition, AHairDirection : TAffineVector);
begin
  FVerletWorld := AVerletWorld;
  FRootDepth := ARootDepth;
  FLinkCount := ALinkCount;
  FHairLength := AHairLength;

  FNodeList := TVerletNodeList.Create;

  BuildHair(AAnchorPosition, AHairDirection);
end;

destructor TVerletHair.Destroy;
begin
  Clear;
  inherited;
end;

function TVerletHair.GetAnchor: TVerletNode;
begin
  result := NodeList[1];
end;

function TVerletHair.GetLinkLength: single;
begin
  if LinkCount>0 then
    result := HairLength / LinkCount
  else
    result := 0;
end;

function TVerletHair.GetRoot: TVerletNode;
begin
  result := NodeList[0];
end;

end.
