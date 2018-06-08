//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Movement path behaviour by Roger Cao 

   Note: It is recommended to set TVXMovementPath.RotationMode = rmUpDirection,
   but the default value is rmTurnPitchRoll for backwards compatibility.
    
}
unit VXS.Movement;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,

  VXS.OpenGL,
  VXS.PersistentClasses,
  VXS.Scene,
  VXS.VectorGeometry,
  VXS.XCollection,
  VXS.Spline,
  VXS.Objects,
  VXS.CrossPlatform,
  VXS.Strings,
  VXS.BaseClasses,
  VXS.VectorTypes;

type

  TVXPathNode = class (TCollectionItem)
  private
    FPosition: TVector;
    FScale: TVector;
    FRotation: TVector;
    FDirection: TVector;
    FUp: TVector;
    FSpeed: single;
    procedure SetPositionAsVector(const Value: TVector);
    procedure SetRotationAsVector(const Value: TVector);
    procedure SetScaleAsVector(const Value: TVector);
    function GetPositionCoordinate(const Index: Integer): Single;
    procedure SetPositionCoordinate(const Index: integer; const AValue: Single);
    function GetRotationCoordinate(const Index: Integer): Single;
    procedure SetRotationCoordinate(const Index: integer; const AValue: Single);
    function GetScaleCoordinate(const Index: Integer): Single;
    procedure SetScaleCoordinate(const Index: integer; const AValue: Single);
    procedure SetSpeed(const Value: single);
    function GetDirectionCoordinate(const Index: Integer): Single;
    procedure SetDirectionCoordinate(const Index: integer; const AValue: Single);
    function GetUpCoordinate(const Index: Integer): Single;
    procedure SetUpCoordinate(const Index: integer; const AValue: Single);
  protected
    function GetDisplayName: string; override;
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function PositionAsAddress: PGLFloat;
    function RotationAsAddress: PGLFloat;
    function ScaleAsAddress: PGLFloat;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeByObject(const Obj: TVXBaseSceneObject);
    { Warning: does not take speed into account. }
    function EqualNode(const aNode: TVXPathNode): boolean;
    { Rotation.X means PitchAngle, Rotation.Y means TurnAngle, Rotation.Z means RollAngle.}
    property RotationAsVector: TVector Read FRotation Write SetRotationAsVector;
    property PositionAsVector: TVector Read FPosition Write SetPositionAsVector;
    property ScaleAsVector: TVector Read FScale Write SetScaleAsVector;
    property UpAsVector: TVector read FUp write FUp;
    property DirectionAsVector: TVector read FDirection write FDirection;
  published
    property X: Single index 0 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Y: Single index 1 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Z: Single index 2 Read GetPositionCoordinate Write SetPositionCoordinate;
    //Rotation.X means PitchAngle;
    //Rotation.Y means TurnAngle;
    //Rotation.Z means RollAngle;
    property PitchAngle: Single index 0 Read GetRotationCoordinate Write SetRotationCoordinate;
    property TurnAngle: Single index 1 Read GetRotationCoordinate Write SetRotationCoordinate;
    property RollAngle: Single index 2 Read GetRotationCoordinate Write SetRotationCoordinate;
    property ScaleX: Single index 0 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleY: Single index 1 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleZ: Single index 2 Read GetScaleCoordinate Write SetScaleCoordinate;
    property DirectionX: Single index 0 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionY: Single index 1 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionZ: Single index 2 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property UpX: Single index 0 Read GetUpCoordinate Write SetUpCoordinate;
    property UpY: Single index 1 Read GetUpCoordinate Write SetUpCoordinate;
    property UpZ: Single index 2 Read GetUpCoordinate Write SetUpCoordinate;
    property Speed: single Read FSpeed Write SetSpeed;
  end;


  TVXMovementRotationMode = (rmTurnPitchRoll, rmUpDirection);
  TVXMovementPath = class;

  TVXPathNodes = class (TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TVXPathNode);
    function GetItems(const index: integer): TVXPathNode;
  public
    constructor Create(aOwner: TVXMovementPath);
    function GetOwnerMovementPath: TVXMovementPath;
    function Add: TVXPathNode;
    function FindItemID(const ID: integer): TVXPathNode;
    property Items[const index: integer]: TVXPathNode Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  TVXMovement = class;
  TVXMovementPaths = class;

  TVXMovementPath = class(TCollectionItem)
  private
    FPathLine: TVXLines;
    FShowPath: Boolean;
    FPathSplineMode: TLineSplineMode;
    FNodes: TVXPathNodes;
    //All the time saved in ms
    FStartTimeApplied: Boolean;
    FStartTime: double;
    FInitialTime: Double;
    FEstimateTime: double;
    FCurrentNode: TVXPathNode;
    FInTravel: boolean;
    FLooped: boolean;
    FName: string;
    FRotationMode: TVXMovementRotationMode;
    MotionSplineControl: TCubicSpline;
    RotationSplineControl: TCubicSpline;
    ScaleSplineControl: TCubicSpline;
    DirectionSplineControl: TCubicSpline;
    UpSplineControl: TCubicSpline;
    FOnTravelStart: TNotifyEvent;
    FOnTravelStop: TNotifyEvent;
    FCurrentNodeIndex: integer;
    function GetNodeCount: integer;
    procedure SetStartTime(const Value: double);
    procedure SetCurrentNodeIndex(const Value: integer);
    procedure SetShowPath(const Value: Boolean);
    procedure SetPathSplineMode(const Value: TLineSplineMode);
  protected
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
    function CanTravel: boolean;
    function GetCollection: TVXMovementPaths;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetMovement: TVXMovement;
    function AddNode: TVXPathNode; overload;
    function AddNode(const Node: TVXPathNode): TVXPathNode; overload;
    function AddNodeFromObject(const Obj: TVXBaseSceneObject): TVXPathNode;
    function InsertNodeFromObject(const Obj: TVXBaseSceneObject; const Index: integer): TVXPathNode;
    function InsertNode(const Node: TVXPathNode; const Index: integer): TVXPathNode; overload;
    function InsertNode(const Index: integer): TVXPathNode; overload;
    function DeleteNode(const Index: integer): TVXPathNode; overload;
    function DeleteNode(const Node: TVXPathNode): TVXPathNode; overload;
    procedure ClearNodes;
    procedure UpdatePathLine;
    function NodeDistance(const Node1, Node2: TVXPathNode): double;
    procedure CalculateState(const CurrentTime: double);
    procedure TravelPath(const Start: boolean); overload;
    procedure TravelPath(const Start: boolean; const aStartTime: double); overload;
    property NodeCount: integer Read GetNodeCount;
    property CurrentNode: TVXPathNode Read FCurrentNode;
    property InTravel: boolean Read FInTravel;
    function PrevNode: integer;
    function NextNode: integer;
    property CurrentNodeIndex: integer Read FCurrentNodeIndex Write SetCurrentNodeIndex;
    property OnTravelStart: TNotifyEvent Read FOnTravelStart Write FOnTravelStart;
    property OnTravelStop: TNotifyEvent Read FOnTravelStop Write FOnTravelStop;
  published
    property Name: string Read FName Write FName;
    { This property is currently ignored. }
    property PathSplineMode: TLineSplineMode read FPathSplineMode write SetPathSplineMode default lsmLines;
    property RotationMode: TVXMovementRotationMode read FRotationMode write FRotationMode default rmTurnPitchRoll;
    property StartTime: double Read FStartTime Write SetStartTime;
    property EstimateTime: double Read FEstimateTime;
    property Looped: boolean Read FLooped Write FLooped;
    property Nodes: TVXPathNodes Read FNodes;
    property ShowPath: Boolean read FShowPath write SetShowPath;
  end;

  TVXMovementPaths = class(TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TVXMovementPath);
    function GetItems(const index: integer): TVXMovementPath;
    function GetMovement: TVXMovement;
  public
    constructor Create(aOwner: TVXMovement);
    function Add: TVXMovementPath;
    function FindItemID(const ID: integer): TVXMovementPath;
    property Items[const index: integer]: TVXMovementPath Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;


  //Event for path related event
  TPathTravelStartEvent = procedure (Sender: TObject;
    Path: TVXMovementPath) of object;
  TPathTravelStopEvent = procedure (Sender: TObject;
    Path: TVXMovementPath; var Looped: boolean) of object;

  TVXMovement = class(TVXBehaviour)
  private
    FPaths: TVXMovementPaths;
    FAutoStartNextPath: boolean;
    FActivePathIndex: integer;
    FOnAllPathTravelledOver: TNotifyEvent;
    FOnPathTravelStart: TPathTravelStartEvent;
    FOnPathTravelStop: TPathTravelStopEvent;
    {
    function GetMovementPath(Index: integer): TVXMovementPath;
    procedure SetMovementPath(Index: integer; AValue: TVXMovementPath);
    }
    function GetPathCount: integer;
    procedure SetActivePathIndex(Value: integer);
    function GetActivePath: TVXMovementPath;
    procedure SetActivePath(Value: TVXMovementPath);
  protected
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure PathTravelStart(Sender: TObject);
    procedure PathTravelStop(Sender: TObject);
    function GetSceneObject: TVXBaseSceneObject;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
      //add an empty path;
    function AddPath: TVXMovementPath; overload;
    //add an path with one node, and the node is based on aObject
    function AddPath(aObject: TVXBaseSceneObject): TVXMovementPath; overload;
    //add one path to the new one
    function AddPath(Path: TVXMovementPath): TVXMovementPath; overload;
    procedure ClearPaths;
      //Result is current path
    function DeletePath(Path: TVXMovementPath): TVXMovementPath; overload;
    function DeletePath(Index: integer): TVXMovementPath; overload;
    function DeletePath: TVXMovementPath; overload;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure StartPathTravel;
    procedure StopPathTravel;
    procedure DoProgress(const progressTime : TVXProgressTimes); override;
    function NextPath: integer;
    function PrevPath: integer;
    function FirstPath: integer;
    function LastPath: integer;
      //property Paths[index: Integer]: TVXMovementPath read GetMovementPath write SetMovementPath;
    property PathCount: integer Read GetPathCount;
    //why do these property can't be saved in IDE ?
    property OnAllPathTravelledOver: TNotifyEvent Read FOnAllPathTravelledOver Write FOnAllPathTravelledOver;
    property OnPathTravelStart: TPathTravelStartEvent Read FOnPathTravelStart Write FOnPathTravelStart;
    property OnPathTravelStop: TPathTravelStopEvent Read FOnPathTravelStop Write FOnPathTravelStop;
  published
    property Paths: TVXMovementPaths Read FPaths;
    property AutoStartNextPath: boolean Read FAutoStartNextPath Write FAutoStartNextPath;
    property ActivePathIndex: integer Read FActivePathIndex Write SetActivePathIndex;
    property ActivePath: TVXMovementPath Read GetActivePath Write SetActivePath;
  end;

function GetMovement(const behaviours: TVXBehaviours): TVXMovement; overload;
function GetMovement(const obj: TVXBaseSceneObject): TVXMovement; overload;
function GetOrCreateMovement(const behaviours: TVXBehaviours): TVXMovement; overload;
function GetOrCreateMovement(const obj: TVXBaseSceneObject): TVXMovement; overload;
procedure StartAllMovements(const Scene: TVXScene; const StartCamerasMove, StartObjectsMove: Boolean);
procedure StopAllMovements(const Scene: TVXScene; const StopCamerasMove, StopObjectsMove: Boolean);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

//----------------------------- TVXPathNode ------------------------------------
constructor TVXPathNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition  := VectorMake(0, 0, 0, 1);
  FRotation  := VectorMake(0, 0, 0, 1);
  FScale     := VectorMake(1, 1, 1, 1);
  FDirection := ZHmgVector;
  FUp        := YHmgVector;
  FSpeed     := 0;
end;

destructor TVXPathNode.Destroy;
begin
  inherited Destroy;
end;

procedure TVXPathNode.SetPositionAsVector(const Value: TVector);
begin
  FPosition := Value;
    (Collection as TVXPathNodes).NotifyChange;
end;

procedure TVXPathNode.SetRotationAsVector(const Value: TVector);
begin
  FRotation := Value;
    (Collection as TVXPathNodes).NotifyChange;
end;

procedure TVXPathNode.SetScaleAsVector(const Value: TVector);
begin
  FScale := Value;
    (Collection as TVXPathNodes).NotifyChange;
end;

function TVXPathNode.PositionAsAddress: PGLFloat;
begin
  Result := @FPosition;
end;

function TVXPathNode.RotationAsAddress: PGLFloat;
begin
  Result := @FRotation;
end;

function TVXPathNode.ScaleAsAddress: PGLFloat;
begin
  Result := @FScale;
end;

procedure TVXPathNode.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
begin
  with Writer do
  begin
    WriteInteger(1); // Archive Version 1.
    WriteStuff := not (VectorEquals(FPosition, NullHmgPoint) and
                       VectorEquals(FRotation, NullHmgPoint) and
                       VectorEquals(FScale, XYZHmgVector) and
                       (Speed = 0) and
                       VectorEquals(FDirection, ZHmgVector) and
                       VectorEquals(FUp, YHmgVector));

    WriteBoolean(writeStuff);

    if WriteStuff then
    begin
      // Archive Version 0.
      Write(FPosition, SizeOf(FPosition));
      Write(FRotation, SizeOf(FRotation));
      Write(FScale, SizeOf(FScale));
      WriteFloat(FSpeed);
      
      // Archive Version 1.
      Write(FDirection, SizeOf(FDirection));
      Write(FUp, SizeOf(FUp));
    end;
  end;
end;

procedure TVXPathNode.ReadFromFiler(reader : TReader);
var
  lVersion: Integer;
begin
  with Reader do
  begin
    lVersion := ReadInteger;
    if ReadBoolean then
    begin
      // Archive Version 0.
      Read(FPosition, SizeOf(FPosition));
      Read(FRotation, SizeOf(FRotation));
      Read(FScale, SizeOf(FScale));
      FSpeed := ReadFloat;

      // Archive Version 1.
      if lVersion >= 1 then
      begin
        Read(FDirection, SizeOf(FDirection));
        Read(FUp, SizeOf(FUp));
      end;
    end
    else
    begin
      // Default parameters.
      FPosition := NullHmgPoint;
      FRotation := NullHmgPoint;
      FScale := VectorMake(1, 1, 1, 1);
      FSpeed := 0;
      FDirection := ZHmgVector;
      FUp        := YHmgVector;
    end;
  end;
end;

procedure TVXPathNode.InitializeByObject(const Obj: TVXBaseSceneObject);
begin
  if Assigned(Obj) then
  begin
    FPosition := Obj.Position.AsVector;
    FScale    := Obj.Scale.AsVector;
    FRotation := Obj.Rotation.AsVector;
    FDirection := Obj.Direction.AsVector;
    FUp        := Obj.Up.AsVector;
  end;
end;

procedure TVXPathNode.Assign(Source: TPersistent);
begin
  if Source is TVXPathNode then
  begin
    FPosition := TVXPathNode(Source).FPosition;
    FRotation := TVXPathNode(Source).FRotation;
    FScale    := TVXPathNode(Source).FScale;
    FSpeed    := TVXPathNode(Source).FSpeed;

    FDirection := TVXPathNode(Source).FDirection;
    FUp        := TVXPathNode(Source).FUp;
  end else
    inherited Assign(Source);
end;

function TVXPathNode.EqualNode(const aNode: TVXPathNode): boolean;
begin
  Result := VectorEquals(FPosition, aNode.FPosition) and
            VectorEquals(FRotation, aNode.FRotation) and
            VectorEquals(FScale, aNode.FScale) and
            VectorEquals(FDirection, aNode.FDirection) and
            VectorEquals(FUp, aNode.FUp);
end;

procedure TVXPathNode.SetSpeed(const Value: single);
begin
  FSpeed := Value;
end;

function TVXPathNode.GetDisplayName: string;
begin
  Result := 'PathNode';
end;

function TVXPathNode.GetPositionCoordinate(const Index: Integer): Single;
begin
  result := FPosition.V[Index];
end;

procedure TVXPathNode.SetPositionCoordinate(const Index: integer; const AValue: Single);
begin
  FPosition.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVXPathNodes).NotifyChange;
end;

function TVXPathNode.GetRotationCoordinate(const Index: Integer): Single;
begin
  result := FRotation.V[Index];
end;

procedure TVXPathNode.SetRotationCoordinate(const Index: integer; const AValue: Single);
begin
  FRotation.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVXPathNodes).NotifyChange;
end;

function TVXPathNode.GetScaleCoordinate(const Index: Integer): Single;
begin
  result := FScale.V[Index];
end;

procedure TVXPathNode.SetScaleCoordinate(const Index: integer; const AValue: Single);
begin
  FScale.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVXPathNodes).NotifyChange;
end;


function TVXPathNode.GetDirectionCoordinate(const Index: Integer): Single;
begin
  result := FDirection.V[Index];
end;


procedure TVXPathNode.SetDirectionCoordinate(const Index: integer;
  const AValue: Single);
begin
  FDirection.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVXPathNodes).NotifyChange;
end;

function TVXPathNode.GetUpCoordinate(const Index: Integer): Single;
begin
  result := FUp.V[Index];
end;

procedure TVXPathNode.SetUpCoordinate(const Index: integer; const AValue: Single);
begin
  FUp.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVXPathNodes).NotifyChange;
end;

//--------------------------- TVXPathNodes -------------------------------------
constructor TVXPathNodes.Create(aOwner: TVXMovementPath);
begin
  inherited Create(aOwner, TVXPathNode);
end;

procedure TVXPathNodes.SetItems(const index: integer; const val: TVXPathNode);
begin
  inherited Items[index] := val;
end;

function TVXPathNodes.GetItems(const index: integer): TVXPathNode;
begin
  Result := TVXPathNode(inherited Items[index]);
end;

function TVXPathNodes.Add: TVXPathNode;
begin
  Result := (inherited Add) as TVXPathNode;
end;

function TVXPathNodes.GetOwnerMovementPath: TVXMovementPath;
begin
  Result := TVXMovementPath(GetOwner);
end;

function TVXPathNodes.FindItemID(const ID: integer): TVXPathNode;
begin
  Result := (inherited FindItemID(ID)) as TVXPathNode;
end;

procedure TVXPathNodes.NotifyChange;
begin
  // Update the path-line if avalible in TVXMovementPath.
  GetOwnerMovementPath.UpdatePathLine;
end;

//--------------------------- TVXMovementPath ----------------------------------
constructor TVXMovementPath.Create(Collection: TCollection);
begin
  // This object can only be added to a TVXMovement class.
  inherited Create(Collection);

  FNodes := TVXPathNodes.Create(Self);
  FCurrentNodeIndex := -1;
  FRotationMode := rmTurnPitchRoll;
  FPathSplineMode := lsmCubicSpline;
  FStartTimeApplied := False;  
end;

destructor TVXMovementPath.Destroy;
begin
  // Make sure the splines are freed.
  FLooped:= false;
  
  ClearNodes;
  FNodes.Free;

  inherited Destroy;
end;

procedure TVXMovementPath.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    WriteInteger(1); // Archive Version 1.
    WriteStuff := (FNodes.Count>0) or (FLooped) or (FCurrentNodeIndex<>-1) or (FShowPath) or
                  (FPathSplineMode <> lsmCubicSpline) or (FRotationMode <> rmTurnPitchRoll);
    WriteBoolean(writeStuff);
    if WriteStuff then
    begin
      // Archive Version 0.
      WriteBoolean(FLooped);
      WriteInteger(FCurrentNodeIndex);
      WriteBoolean(FShowPath);
      Write(FPathSplineMode, SizeOf(FPathSplineMode));
      WriteInteger(FNodes.Count);
      for I:=0 to FNodes.Count-1 do
        FNodes.Items[I].WriteToFiler(Writer);

      // Archive Version 1.
      WriteInteger(Ord(FRotationMode));
    end;
  end;
end;

procedure TVXMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Node: TVXPathNode;
  lVersion: Integer;
begin
  ClearNodes;
  with Reader do
  begin
    lVersion := ReadInteger; // Archive Version.
    if ReadBoolean then
    begin
      // Archive Version 0.
      FLooped := ReadBoolean;
      FCurrentNodeIndex := ReadInteger;
      ShowPath := ReadBoolean;
      Read(FPathSplineMode, SizeOf(FPathSplineMode));

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Node := AddNode;
        Node.ReadFromFiler(Reader);
      end;

      // Archive Version 1.
      if lVersion >= 1 then
      begin
        FRotationMode := TVXMovementRotationMode(ReadInteger);
      end;  
    end
    else
    begin
      FLooped := False;
      FCurrentNodeIndex := -1;
      FShowPath := False;
      FPathSplineMode := lsmCubicSpline;
      FRotationMode := rmTurnPitchRoll;
    end;
  end;
  UpdatePathLine;
end;

procedure TVXMovementPath.SetPathSplineMode(const Value: TLineSplineMode);
begin
  if Value<>FPathSplineMode then
  begin
    FPathSplineMode := Value;
    if FShowPath then
      FPathLine.SplineMode := FPathSplineMode;
  end;
end;

procedure TVXMovementPath.UpdatePathLine;
var
  I: Integer;
  Node: TVXPathNode;
begin
  if FShowPath then
  begin
    FPathLine.Nodes.Clear;
    for I:=0 to Nodes.Count-1 do
    begin
      Node := Nodes.Items[I];
      FPathLine.AddNode(Node.PositionAsVector);
    end;
  end;
end;

procedure TVXMovementPath.SetShowPath(const Value: Boolean);
var
  OwnerObj: TVXBaseSceneObject;
begin
  if FShowPath<>Value then
  begin
    FShowPath := Value;
    OwnerObj := GetMovement.GetSceneObject;
    if FShowPath then
    begin
      FPathLine := TVXLines.Create(OwnerObj);
      MakeSubComponent(FPathLine, True);
      OwnerObj.Scene.Objects.AddChild(FPathLine);
      FPathLine.SplineMode := FPathSplineMode;
      UpdatePathLine;
    end
    else
      FreeAndNil(FPathLine);
  end;
end;

procedure TVXMovementPath.ClearNodes;
begin
  TravelPath(False);
  FNodes.Clear;
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode.Free;
    FCurrentNode := nil;
  end;
  FCurrentNodeIndex := -1;
  UpdatePathLine;
end;

procedure TVXMovementPath.SetCurrentNodeIndex(const Value: integer);
begin
  if FNodes.Count = 0 then
  begin
    FCurrentNodeIndex := -1;
    exit;
  end;
  if (FInTravel) or (Value > FNodes.Count - 1) or (Value < 0) then
    exit
  else
  begin
    FCurrentNodeIndex := Value;
    if not Assigned(FCurrentNode) then
      FCurrentNode := TVXPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[FCurrentNodeIndex]);
  end;
end;

function TVXMovementPath.InsertNode(const Node: TVXPathNode; const Index: integer): TVXPathNode;
var
  N: TVXPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Node)) and (Assigned(Nodes[Index])) then
  begin
    N := TVXPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
  end
  else
    //add to the tail of list
    N    := FNodes.Add;
  Result := N;
  UpdatePathLine;
end;

function TVXMovementPath.InsertNode(const Index: integer): TVXPathNode;
var
  N: TVXPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Nodes[Index])) then
  begin
    N := TVXPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
    Result := N;
  end
  else
    //add to the tail of list
    Result := AddNode;
  UpdatePathLine;
end;

function TVXMovementPath.AddNodeFromObject(const Obj: TVXBaseSceneObject): TVXPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;
  Result           := AddNode;
  Result.FPosition := Obj.Position.AsVector;
  Result.FScale    := Obj.Scale.AsVector;
  Result.FRotation := Obj.Rotation.AsVector;
  Result.FDirection:=  Obj.Direction.AsVector;
  Result.FUp:=         Obj.Up.AsVector;

  UpdatePathLine;
end;

function TVXMovementPath.InsertNodeFromObject(const Obj: TVXBaseSceneObject; const Index: integer): TVXPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;

  Result      := InsertNode(Index);
  Result.FPosition := Obj.Position.AsVector;
  Result.FScale    := Obj.Scale.AsVector;
  Result.FRotation := Obj.Rotation.AsVector;
  Result.FDirection:= Obj.Direction.AsVector;
  Result.FUp:= Obj.Up.AsVector;

  UpdatePathLine;
end;

function TVXMovementPath.DeleteNode(const Index: integer): TVXPathNode;
var
  Node: TVXPathNode;
begin
  Result := nil;
  //Ontravel, can't delete
  if FInTravel then
    exit;
  Node := Nodes[Index];
  if Assigned(Node) then
  begin
    FNodes.Delete(Index);
    if FCurrentNodeIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FNodes.Count > 0 then
        FCurrentNodeIndex := 0
      else
        FCurrentNodeIndex := -1;
    end
    else
    begin
      //one has been deleted, so the index should be equal to FNodeList.Count
      if Index =FNodes.Count then
        FCurrentNodeIndex := Index -1
      else
        FCurrentNodeIndex := Index;
    end;
    Result := Nodes[FCurrentNodeIndex];
  end;
  UpdatePathLine;
end;

function TVXMovementPath.DeleteNode(const Node: TVXPathNode): TVXPathNode;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count - 1 do
  begin
    if Node = Nodes[I] then
    begin
      Result := DeleteNode(I);
      break;
    end;
  end;
  UpdatePathLine;
end;

function TVXMovementPath.PrevNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Dec(FCurrentNodeIndex);
  if (FCurrentNodeIndex < 0) then
    FCurrentNodeIndex := 0
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TVXMovementPath.NextNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Inc(FCurrentNodeIndex);
  if (FCurrentNodeIndex = FNodes.Count) then
    Dec(FCurrentNodeIndex)
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TVXMovementPath.NodeDistance(const Node1, Node2: TVXPathNode): double;
begin
  Result := VectorDistance(Node1.FPosition, Node2.FPosition);
end;

//need to do
//1 No acceleration implemented
//2 The travel-time of a segment is based a simple linear movement, at the start and the end
//  of the segment, the speed will be more high than in the middle
//3 Rotation Interpolation has not been tested
procedure TVXMovementPath.CalculateState(const CurrentTime: double);
var
  I:       integer;
  SumTime: double;
  L, L2:       single;
  Interpolated: boolean;
  T:       double;
  a:double;

  procedure Interpolation(ReturnNode: TVXPathNode; Time1, Time2: double; Index: integer);
  var
    Ratio: double;
    x, y, z, p, t, r, sx, sy, sz: single;
    dx, dy, dz,ux, uy, uz: single;
  begin
    Ratio:=(Nodes[I - 1].Speed*Time2+0.5*a*time2*time2)/L + Index;

    MotionSplineControl.SplineXYZ(Ratio, x, y, z);
    RotationSplineControl.SplineXYZ(Ratio, p, t, r);
    ScaleSplineControl.SplineXYZ(Ratio, sx, sy, sz);

    DirectionSplineControl.SplineXYZ(Ratio,dx,dy,dz);
    UpSplineControl.SplineXYZ(Ratio,ux,uy,uz);


    ReturnNode.FPosition := VectorMake(x, y, z, 1);
    ReturnNode.FRotation := VectorMake(p, t, r, 1);
    ReturnNode.FScale    := VectorMake(sx, sy, sz, 1);

    ReturnNode.FDirection := VectorMake(dx,dy,dz, 1);
    ReturnNode.FUp := VectorMake(ux,uy,uz, 1);
  end;

begin
  I := 1;

  if (FInitialTime = 0) or (FInitialTime > CurrentTime) then
    FInitialTime := CurrentTime;


  if (FStartTime <> 0) and not FStartTimeApplied then
  begin
    if FInitialTime + FStartTime < CurrentTime then
    begin
      FInitialTime := CurrentTime;
      FStartTimeApplied := True;
    end
    else
      Exit;
  end;

  SumTime      := FInitialTime;
  Interpolated := False;
  while I < FNodes.Count do
  begin
    L := NodeDistance(Nodes[I], Nodes[I - 1]);

    if L = 0 then
      L := VectorDistance(Nodes[i].FScale, Nodes[i-1].FScale);

    if L = 0 then
    begin
      L := VectorDistance(Nodes[i].FDirection, Nodes[i-1].FDirection);
      L2 := VectorDistance(Nodes[i].FUp, Nodes[i-1].Fup);
      if (L2 > L) then L:= L2;
    end;

    if L = 0 then
      L := Nodes[I - 0].Speed;

    T := L / (Nodes[I - 1].Speed + Nodes[I - 0].Speed) * 2;
    if (SumTime + T) >= CurrentTime then
    begin
      a:=(Nodes[I - 0].Speed-Nodes[I - 1].Speed)/T;
      Interpolation(FCurrentNode, T, CurrentTime - SumTime, I - 1);
      Interpolated := True;
      break;
    end
    else
    begin
      Inc(I);
      SumTime := SumTime + T;
    end;
  end;

  if (not Interpolated) then
  begin
    Interpolation(FCurrentNode, 1.0, 0.0, FNodes.Count - 1);
    TravelPath(False);
  end;
end;


function TVXMovementPath.CanTravel: boolean;
var
  I: integer;
begin
  Result := True;
  if FNodes.Count < 2 then
  begin
    Result := False;
    exit;
  end;
  for I := 0 to FNodes.Count - 1 do
    if Abs(Nodes[I].Speed) < 0.01 then
    begin
      Result := False;
      break;
    end;
end;

function TVXMovementPath.GetCollection: TVXMovementPaths;
begin
  Result := TVXMovementPaths(GetOwner);
end;

function TVXMovementPath.GetMovement: TVXMovement;
begin
  Result := GetCollection.GetMovement;
end;

procedure TVXMovementPath.TravelPath(const Start: boolean);
var
  x, y, z:    PFloatArray;
  p, t, r:    PFloatArray;
  sx, sy, sz: PFloatArray;
  dx, dy, dz: PFloatArray;
  ux, uy, uz: PFloatArray;

  I:          integer;
begin
  if (FInTravel = Start) or (FNodes.Count = 0) then
    exit;
  //One of the node speed < 0.01;
  if (Start) and (not CanTravel) then
    exit;
  FInTravel := Start;
  if FInTravel then
  begin
    GetMem(x, sizeof(single) * FNodes.Count);
    GetMem(y, sizeof(single) * FNodes.Count);
    GetMem(z, sizeof(single) * FNodes.Count);
    GetMem(p, sizeof(single) * FNodes.Count);
    GetMem(t, sizeof(single) * FNodes.Count);
    GetMem(r, sizeof(single) * FNodes.Count);
    GetMem(sx, sizeof(single) * FNodes.Count);
    GetMem(sy, sizeof(single) * FNodes.Count);
    GetMem(sz, sizeof(single) * FNodes.Count);
    GetMem(dx, sizeof(single) * FNodes.Count);
    GetMem(dy, sizeof(single) * FNodes.Count);
    GetMem(dz, sizeof(single) * FNodes.Count);
    GetMem(ux, sizeof(single) * FNodes.Count);
    GetMem(uy, sizeof(single) * FNodes.Count);
    GetMem(uz, sizeof(single) * FNodes.Count);

    for I := 0 to FNodes.Count - 1 do
    begin
      PFloatArray(x)[I]  := Nodes[I].FPosition.X;
      PFloatArray(y)[I]  := Nodes[I].FPosition.Y;
      PFloatArray(z)[I]  := Nodes[I].FPosition.Z;
      PFloatArray(p)[I]  := Nodes[I].FRotation.X;
      PFloatArray(t)[I]  := Nodes[I].FRotation.Y;
      PFloatArray(r)[I]  := Nodes[I].FRotation.Z;
      PFloatArray(sx)[I] := Nodes[I].FScale.X;
      PFloatArray(sy)[I] := Nodes[I].FScale.Y;
      PFloatArray(sz)[I] := Nodes[I].FScale.Z;

      PFloatArray(dx)[I] := Nodes[I].FDirection.X;
      PFloatArray(dy)[I] := Nodes[I].FDirection.Y;
      PFloatArray(dz)[I] := Nodes[I].FDirection.Z;

      PFloatArray(ux)[I] := Nodes[I].FUp.X;
      PFloatArray(uy)[I] := Nodes[I].FUp.Y;
      PFloatArray(uz)[I] := Nodes[I].FUp.Z;

    end;
    MotionSplineControl   := TCubicSpline.Create(x, y, z, nil, FNodes.Count);
    RotationSplineControl := TCubicSpline.Create(p, t, r, nil, FNodes.Count);
    ScaleSplineControl    := TCubicSpline.Create(sx, sy, sz, nil, FNodes.Count);
    DirectionSplineControl:= TCubicSpline.Create(dx, dy, dz, nil, FNodes.Count);
    UpSplineControl:= TCubicSpline.Create(ux, uy, uz, nil, FNodes.Count);

    FreeMem(x);
    FreeMem(y);
    FreeMem(z);
    FreeMem(p);
    FreeMem(t);
    FreeMem(r);
    FreeMem(sx);
    FreeMem(sy);
    FreeMem(sz);
    FreeMem(dx);
    FreeMem(dy);
    FreeMem(dz);
    FreeMem(ux);
    FreeMem(uy);
    FreeMem(uz);


    FreeAndNil(FCurrentNode);
    FCurrentNode := TVXPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[0]);
    FCurrentNodeIndex := -1;

    FEstimateTime := 0;
    for I := 1 to FNodes.Count - 1 do
      FEstimateTime := FEstimateTime + NodeDistance(Nodes[I], Nodes[I - 1]) / Nodes[I - 1].Speed;

    if Assigned(FOnTravelStart) then
      FOnTravelStart(self);
  end
  else
  begin
    FreeAndNil(MotionSplineControl);
    FreeAndNil(RotationSplineControl);
    FreeAndNil(ScaleSplineControl);
    FreeAndNil(DirectionSplineControl);
    FreeAndNil(UpSplineControl);

    if Assigned(FOnTravelStop) then
      FOnTravelStop(self);
  end;
end;

procedure TVXMovementPath.TravelPath(const Start: boolean; const aStartTime: double);
begin
  if FInTravel = Start then
    exit;
  FInitialTime := aStartTime;
  FStartTimeApplied := False;
  TravelPath(Start);
end;

function TVXMovementPath.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

//-------------------------- This function need modified -----------------------
procedure TVXMovementPath.SetStartTime(const Value: double);
begin
  FStartTime := Value;
end;

procedure TVXMovementPath.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TVXMovementPath then
  begin
    ClearNodes;
    for I := 0 to TVXMovementPath(Source).NodeCount - 1 do
    begin
      AddNode;
      Nodes[I].Assign(TVXMovementPath(Source).Nodes[I]);
      FStartTime := TVXMovementPath(Source).FStartTime;
      //FEstimateTime := TVXMovementPath(Source).FEstimateTime;
      FLooped := TVXMovementPath(Source).FLooped;
      FRotationMode := TVXMovementPath(Source).FRotationMode;
    end;
  end;
end;

function TVXMovementPath.AddNode: TVXPathNode;
var
  Node: TVXPathNode;
  I:    integer;
begin
  //Add a empty node, if it's not the first one, try locate the node to the previous one
  Node := FNodes.Add;
  I    := FNodes.Count;
  if I > 1 then
    Node.Assign(Nodes[I - 2]);
  Result := Node;
end;

function TVXMovementPath.AddNode(const Node: TVXPathNode): TVXPathNode;
begin
  Result := AddNode;
  if Assigned(Node) then
    Result.Assign(Node);
end;

//------------------------- TVXMovementPaths ----------------------------------
constructor TVXMovementPaths.Create(aOwner: TVXMovement);
begin
  inherited Create(aOwner, TVXMovementPath);
end;

procedure TVXMovementPaths.SetItems(const index: integer; const val: TVXMovementPath);
begin
  inherited Items[index] := val;
end;

function TVXMovementPaths.GetItems(const index: integer): TVXMovementPath;
begin
  Result := TVXMovementPath(inherited Items[index]);
end;

function TVXMovementPaths.Add: TVXMovementPath;
begin
  Result := (inherited Add) as TVXMovementPath;
end;

function TVXMovementPaths.FindItemID(const ID: integer): TVXMovementPath;
begin
  Result := (inherited FindItemID(ID)) as TVXMovementPath;
end;

procedure TVXMovementPaths.NotifyChange;
begin
  // Do nothing here.
end;

function TVXMovementPaths.GetMovement: TVXMovement;
begin
  Result := TVXMovement(GetOwner);
end;


//--------------------------- TVXMovement --------------------------------------
constructor TVXMovement.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FPaths           := TVXMovementPaths.Create(Self);
  FAutoStartNextPath := True;
  FActivePathIndex := -1;
  FOnAllPathTravelledOver := nil;
  FOnPathTravelStart := nil;
  FOnPathTravelStop := nil;
end;

destructor TVXMovement.Destroy;
begin
  ClearPaths;
  FPaths.Free;
  inherited Destroy;
end;

procedure TVXMovement.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    // Archive Version 1, added inherited call
    WriteInteger(1);
    inherited;
    WriteStuff := (FPaths.Count>0) or (not FAutoStartNextPath) or (FActivePathIndex<>-1);
    WriteBoolean(WriteStuff);
    if WriteStuff then
    begin
      WriteBoolean(FAutoStartNextPath);
      WriteInteger(FActivePathIndex);

      WriteInteger(FPaths.Count);
      for I:=0 to FPaths.Count-1 do
        FPaths.Items[I].WriteToFiler(Writer);
    end;
  end;
end;

procedure TVXMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Path: TVXMovementPath;
  archiveVersion: Integer;
begin
  ClearPaths;
  with Reader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion >= 1 then
      inherited;
    if ReadBoolean then
    begin
      FAutoStartNextPath := ReadBoolean;
      FActivePathIndex := ReadInteger;

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Path := AddPath;
        Path.ReadFromFiler(Reader);
      end;
    end else
    begin
      FAutoStartNextPath := True;
      FActivePathIndex := -1;
    end;
  end;
end;

procedure TVXMovement.ClearPaths;
begin
  StopPathTravel;
  FPaths.Clear;
  FActivePathIndex := -1;
end;

procedure TVXMovement.PathTravelStart(Sender: TObject);
begin
  if Assigned(FOnPathTravelStart) then
    FOnPathTravelStart(Self, TVXMovementPath(Sender));
end;

procedure TVXMovement.PathTravelStop(Sender: TObject);
begin
  if Assigned(FOnPathTravelStop) then
    FOnPathTravelStop(Self, TVXMovementPath(Sender), TVXMovementPath(Sender).FLooped);
  if TVXMovementPath(Sender).FLooped then
  begin
    //if looped, then re-start the path
    StartPathTravel;
  end
  else if (FActivePathIndex = FPaths.Count - 1) then
  begin
    if (Assigned(FOnAllPathTravelledOver)) then
      FOnAllPathTravelledOver(Self);
  end
  else //auto-start next path
  if FAutoStartNextPath then
  begin
    Inc(FActivePathIndex);
    StartPathTravel;
  end;
end;

function TVXMovement.GetSceneObject: TVXBaseSceneObject;
begin
  Result := TVXBaseSceneObject(Owner{TVXBehavours}.Owner);
end;

function TVXMovement.AddPath: TVXMovementPath;
var
  Path: TVXMovementPath;
begin
  Path   := FPaths.Add;
  Path.OnTravelStart := PathTravelStart;
  Path.OnTravelStop := PathTravelStop;
  Result := Path;
end;

function TVXMovement.AddPath(aObject: TVXBaseSceneObject): TVXMovementPath;
begin
  Result := AddPath;
  Result.AddNodeFromObject(aObject);
end;

function TVXMovement.AddPath(Path: TVXMovementPath): TVXMovementPath;
begin
  Result := AddPath;
  if Assigned(Path) then
    Result.Assign(Path);
end;

function TVXMovement.DeletePath: TVXMovementPath;
begin
  Result := DeletePath(FActivePathIndex);
end;

function TVXMovement.DeletePath(Path: TVXMovementPath): TVXMovementPath;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Path = Paths[I] then
    begin
      Result := DeletePath(I);
      break;
    end;
  end;
end;

function TVXMovement.DeletePath(Index: integer): TVXMovementPath;
begin
  Result := nil;
  if (Index <0) or (Index >=FPaths.Count) then
    exit;

  if Index >=0 then
  begin
    TVXMovementPath(FPaths[Index]).Free;
    FPaths.Delete(Index);
    if FActivePathIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FPaths.Count > 0 then
        FActivePathIndex := 0
      else
        FActivePathIndex := -1;
    end 
    else
    begin
      //one has been deleted, so the index should be equal to FPathList.Count
      if Index =FPaths.Count then
        FActivePathIndex := Index -1
      else
        FActivePathIndex := Index;
    end;
    Result := ActivePath;
  end;
end;

procedure TVXMovement.SetActivePathIndex(Value: integer);
begin
  if FActivePathIndex = Value then
    exit;
  //if current has a Active path in travelling, then exit the method
  if (Assigned(ActivePath)) and (ActivePath.InTravel) then
    exit;
  if (Value >= 0) and (Value < FPaths.Count) then
  begin
    FActivePathIndex := Value;
    //Start the new path or wait for the start-command
  end 
  else if Value < 0 then
  begin
    FActivePathIndex := -1;
    //Stop all the running path
  end;
end;

function TVXMovement.NextPath: integer;
begin
  ActivePathIndex := FActivePathIndex + 1;
  Result           := FActivePathIndex;
end;

function TVXMovement.PrevPath: integer;
begin
  ActivePathIndex := FActivePathIndex - 1;
  if (FActivePathIndex < 0) and (FPaths.Count > 0) then
    Result := 0
  else
    Result := FActivePathIndex;
end;

function TVXMovement.FirstPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := 0;
  Result              := FActivePathIndex;
end;

function TVXMovement.LastPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := FPaths.Count - 1;
  Result              := FActivePathIndex;
end;

function TVXMovement.GetActivePath: TVXMovementPath;
begin
  if FActivePathIndex >= 0 then
    Result := Paths[FActivePathIndex]
  else
    Result := nil;
end;

procedure TVXMovement.SetActivePath(Value: TVXMovementPath);
var
  I: integer;
begin
  ActivePathIndex := -1;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Value = Paths[I] then
    begin
      ActivePathIndex := I;
      break;
    end;
  end;
end;

function TVXMovement.GetPathCount: integer;
begin
  Result := FPaths.Count;
end;

procedure TVXMovement.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TVXMovement then
  begin
    ClearPaths;
    for I := 0 to TVXMovement(Source).PathCount - 1 do
    begin
      AddPath;
      Paths[I].Assign(TVXMovement(Source).Paths[I]);
    end;
    FAutoStartNextPath := TVXMovement(Source).FAutoStartNextPath;
  end;
end;

class function TVXMovement.FriendlyName: string;
begin
  Result := 'Movement controls'
end;

class function TVXMovement.FriendlyDescription: string;
begin
  Result := 'Object movement path controls'
end;

class function TVXMovement.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TVXMovement.StartPathTravel;
begin
  if FActivePathIndex < 0 then
    exit;
  //convert the time to second
  Paths[FActivePathIndex].TravelPath(True, 0);
end;

procedure TVXMovement.StopPathTravel;
var
  I: Integer;
begin
  if FPaths.Count <> 0 then
    for I := 0 to FPaths.Count - 1 do
      Paths[I].TravelPath(False);
end;

//Calculate functions add into this method
procedure TVXMovement.DoProgress(const progressTime : TVXProgressTimes);
var
  Path: TVXMovementPath;
begin
  if (FActivePathIndex >= 0) and (Paths[FActivePathIndex].InTravel) then
    begin
      Path := Paths[FActivePathIndex];
      Path.CalculateState(progressTime.newTime);
      if Assigned(Path.CurrentNode) then
      begin
        if Owner.Owner is TVXBaseSceneObject then
          with TVXBaseSceneObject(Owner.Owner) do
          begin
            Position.AsVector := Path.CurrentNode.FPosition;
            Scale.AsVector    := Path.CurrentNode.FScale;

            case Path.FRotationMode of
              rmTurnPitchRoll:
              begin
                PitchAngle := Path.CurrentNode.PitchAngle;
                TurnAngle := Path.CurrentNode.TurnAngle;
                RollAngle := Path.CurrentNode.RollAngle;
              end;

              rmUpDirection:
              begin
                Direction.AsVector := Path.CurrentNode.FDirection;
                Up.AsVector := Path.CurrentNode.FUp;
              end;
            else
              Assert(False, strErrorEx + strUnknownType);
            end
          end;
      end;
    end;
end;


function GetMovement(const behaviours: TVXBehaviours): TVXMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVXMovement);
  if i >= 0 then
    Result := TVXMovement(behaviours[i])
  else
    Result := nil;
end;

function GetMovement(const obj: TVXBaseSceneObject): TVXMovement; overload;
begin
  Result := GetMovement(obj.behaviours);
end;

function GetOrCreateMovement(const behaviours: TVXBehaviours): TVXMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVXMovement);
  if i >= 0 then
    Result := TVXMovement(behaviours[i])
  else
    Result := TVXMovement.Create(behaviours);
end;

function GetOrCreateMovement(const obj: TVXBaseSceneObject): TVXMovement; overload;
begin
  Result := GetOrCreateMovement(obj.behaviours);
end;

procedure StartStopTravel(const Obj: TVXBaseSceneObject; Start: Boolean; ChangeCameras, ChangeObjects: Boolean);
var
  NewObj: TVXBaseSceneObject;
  I: Integer;
  Movement: TVXMovement;
begin
  if ((Obj is TVXCamera)and(ChangeCameras))or
     ((not(Obj is TVXCamera))and(ChangeObjects))  then
  begin
    Movement := GetMovement(Obj);
    if Assigned(Movement) then
      if Start then
      begin
        if (Movement.PathCount>0) and (Movement.ActivePathIndex=-1) then
          Movement.ActivePathIndex := 0;
        Movement.StartPathTravel;
      end else
        Movement.StopPathTravel;
  end;
  for I:=0 to Obj.Count-1 do
  begin
    NewObj := Obj.Children[I];
    StartStopTravel(NewObj, Start, ChangeCameras, ChangeObjects);
  end;
end;

procedure StartAllMovements(const Scene: TVXScene; const StartCamerasMove, StartObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StartCamerasMove or StartObjectsMove then
      StartStopTravel(Scene.Objects, True, StartCamerasMove, StartObjectsMove);
  end;
end;

procedure StopAllMovements(const Scene: TVXScene; const StopCamerasMove, StopObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StopCamerasMove or StopObjectsMove then
      StartStopTravel(Scene.Objects, False, StopCamerasMove, StopObjectsMove);
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TVXMovement);

finalization

  UnregisterXCollectionItemClass(TVXMovement);

end.



