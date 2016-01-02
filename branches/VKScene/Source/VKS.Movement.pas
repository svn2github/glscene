//
// This unit is part of the GLScene Project   
//
{: VKS.Movement<p>

   Movement path behaviour by Roger Cao<p>

   Note: It is recommended to set TVKMovementPath.RotationMode = rmUpDirection,
   but the default value is rmTurnPitchRoll for backwards compatibility.

   <b>Historique : </b><font size=-1><ul>
      <li>10/11/12 - PW - Added CPP compatibility: changed vector arrays to records
                          Replaced direct access to properties by methods
                          GetPositionCoordinate, GetRotationCoordinate, GetScaleCoordinate,
                          GetDirectionCoordinate and GetUpCoordinate
      <li>21/01/01 - DanB - Added "inherited" call to TVKMovement.WriteToFiler
      <li>23/08/10 - Yar - Added VKS.OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>12/11/09 - DaStr - Bugfix after previous commit
      <li>25/10/09 - DaStr - Bugfixed TVKMovementPath.StartTime (thanks Zsolt Laky)
      <li>14/03/09 - DanB - Changes to Start/StopAllMovements due to TVKScene.Cameras removal
      <li>05/10/08 - DaStr - Added Delphi5 compatibility
      <li>21/06/08 - DaStr - A lot of cosmetic fixes
                             Bugfixed same position rotation / scale interpolation
                               in TVKMovementPath.CalculateState()
                             Bugfixed destroying TVKMovement with multiple paths.
                             Added TVKMovementPath.RotationMode
                            (Bugtracker ID = 1999464) (thanks VirusX)
      <li>02/04/07 - DaStr - All cross-version stuff abstracted into VKS.CrossPlatform
      <li>25/03/07 - DaStr - Small fix for Delphi5 compatibility
      <li>15/02/07 - DaStr - Fixed TVKMovementPath.SetShowPath - SubComponent support
      <li>27/10/06 - LC - Fixed memory leak in TVKMovementPath. Bugtracker ID=1548615 (thanks Da Stranger)
      <li>28/09/04 - Mrqzzz - Fixed bug in proc. Interpolation (skipped a line from Carlos' code, oops)
      <li>09/09/04 - Mrqzzz - CalculateState change by Carlos (NG) to make speed interpolated between nodes
      <li>20/11/01 - Egg - DoProgress fix suggested by Philipp Pammler (NG)
      <li>14/01/01 - Egg - Minor changes, integrated to v0.8RC2, still needed:
                           use of standard classes and documentation
      <li>22/09/00 - RoC - Added StartAllPathTravel and StopAllPathTravel methods
      <li>24/08/00 - RoC - TVKMovement and relative class added
   </ul></font>
}
unit VKS.Movement;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,

  VKS.Scene, VKS.VectorGeometry, VKS.XCollection, VKS.OpenGLTokens, VKS.Spline, VKS.Objects,
  VKS.CrossPlatform, VKS.Strings, VKS.BaseClasses, VKS.VectorTypes;

type

  // TVKPathNode
  //
  TVKPathNode = class (TCollectionItem)
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

    function GetPositionCoordinate(const Index: Integer): TVKFloat;
    procedure SetPositionCoordinate(const Index: integer; const AValue: TVKFloat);
    function GetRotationCoordinate(const Index: Integer): TVKFloat;
    procedure SetRotationCoordinate(const Index: integer; const AValue: TVKFloat);
    function GetScaleCoordinate(const Index: Integer): TVKFloat;
    procedure SetScaleCoordinate(const Index: integer; const AValue: TVKFloat);

    procedure SetSpeed(const Value: single);

    function GetDirectionCoordinate(const Index: Integer): TVKFloat;
    procedure SetDirectionCoordinate(const Index: integer; const AValue: TVKFloat);
    function GetUpCoordinate(const Index: Integer): TVKFloat;
    procedure SetUpCoordinate(const Index: integer; const AValue: TVKFloat);
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

    procedure InitializeByObject(const Obj: TVKBaseSceneObject);

    {: Warning: does not take speed into account. }
    function EqualNode(const aNode: TVKPathNode): boolean;

    {: Rotation.X means PitchAngle, Rotation.Y means TurnAngle, Rotation.Z means RollAngle.}
    property RotationAsVector: TVector Read FRotation Write SetRotationAsVector;
    property PositionAsVector: TVector Read FPosition Write SetPositionAsVector;
    property ScaleAsVector: TVector Read FScale Write SetScaleAsVector;
    property UpAsVector: TVector read FUp write FUp;
    property DirectionAsVector: TVector read FDirection write FDirection;

  published
    property X: TVKFloat index 0 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Y: TVKFloat index 1 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Z: TVKFloat index 2 Read GetPositionCoordinate Write SetPositionCoordinate;

    //Rotation.X means PitchAngle;
    //Rotation.Y means TurnAngle;
    //Rotation.Z means RollAngle;
    property PitchAngle: TVKFloat index 0 Read GetRotationCoordinate Write SetRotationCoordinate;
    property TurnAngle: TVKFloat index 1 Read GetRotationCoordinate Write SetRotationCoordinate;
    property RollAngle: TVKFloat index 2 Read GetRotationCoordinate Write SetRotationCoordinate;

    property ScaleX: TVKFloat index 0 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleY: TVKFloat index 1 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleZ: TVKFloat index 2 Read GetScaleCoordinate Write SetScaleCoordinate;

    property DirectionX: TVKFloat index 0 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionY: TVKFloat index 1 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionZ: TVKFloat index 2 Read GetDirectionCoordinate Write SetDirectionCoordinate;

    property UpX: TVKFloat index 0 Read GetUpCoordinate Write SetUpCoordinate;
    property UpY: TVKFloat index 1 Read GetUpCoordinate Write SetUpCoordinate;
    property UpZ: TVKFloat index 2 Read GetUpCoordinate Write SetUpCoordinate;

    property Speed: single Read FSpeed Write SetSpeed;
  end;


  TVKMovementRotationMode = (rmTurnPitchRoll, rmUpDirection);


  TVKMovementPath = class;

  // TVKPathNodes
  //
  TVKPathNodes = class (TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TVKPathNode);
    function GetItems(const index: integer): TVKPathNode;
  public
    constructor Create(aOwner: TVKMovementPath);
    function GetOwnerMovementPath: TVKMovementPath;
    function Add: TVKPathNode;
    function FindItemID(const ID: integer): TVKPathNode;
    property Items[const index: integer]: TVKPathNode Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  TVKMovement = class;
  TVKMovementPaths = class;

  TVKMovementPath = class(TCollectionItem)
  private
    FPathLine: TVKLines;
    FShowPath: Boolean;
    FPathSplineMode: TLineSplineMode;

    FNodes: TVKPathNodes;

    //All the time saved in ms
    FStartTimeApplied: Boolean;
    FStartTime: double;
    FInitialTime: Double;
    FEstimateTime: double;
    FCurrentNode: TVKPathNode;
    FInTravel: boolean;
    FLooped: boolean;
    FName: string;
    FRotationMode: TVKMovementRotationMode;


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
    function GetCollection: TVKMovementPaths;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function GetMovement: TVKMovement;

    function AddNode: TVKPathNode; overload;
    function AddNode(const Node: TVKPathNode): TVKPathNode; overload;

    function AddNodeFromObject(const Obj: TVKBaseSceneObject): TVKPathNode;
    function InsertNodeFromObject(const Obj: TVKBaseSceneObject; const Index: integer): TVKPathNode;

    function InsertNode(const Node: TVKPathNode; const Index: integer): TVKPathNode; overload;
    function InsertNode(const Index: integer): TVKPathNode; overload;
    function DeleteNode(const Index: integer): TVKPathNode; overload;
    function DeleteNode(const Node: TVKPathNode): TVKPathNode; overload;
    procedure ClearNodes;
    procedure UpdatePathLine;

    function NodeDistance(const Node1, Node2: TVKPathNode): double;

    procedure CalculateState(const CurrentTime: double);

    procedure TravelPath(const Start: boolean); overload;
    procedure TravelPath(const Start: boolean; const aStartTime: double); overload;

    property NodeCount: integer Read GetNodeCount;
    property CurrentNode: TVKPathNode Read FCurrentNode;
    property InTravel: boolean Read FInTravel;

    function PrevNode: integer;
    function NextNode: integer;

    property CurrentNodeIndex: integer Read FCurrentNodeIndex Write SetCurrentNodeIndex;

    property OnTravelStart: TNotifyEvent Read FOnTravelStart Write FOnTravelStart;
    property OnTravelStop: TNotifyEvent Read FOnTravelStop Write FOnTravelStop;
  published
    property Name: string Read FName Write FName;

    {: This property is currently ignored. }
    property PathSplineMode: TLineSplineMode read FPathSplineMode write SetPathSplineMode default lsmLines;
    property RotationMode: TVKMovementRotationMode read FRotationMode write FRotationMode default rmTurnPitchRoll;

    property StartTime: double Read FStartTime Write SetStartTime;
    property EstimateTime: double Read FEstimateTime;

    property Looped: boolean Read FLooped Write FLooped;
    property Nodes: TVKPathNodes Read FNodes;
    property ShowPath: Boolean read FShowPath write SetShowPath;
  end;

  TVKMovementPaths = class(TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TVKMovementPath);
    function GetItems(const index: integer): TVKMovementPath;
    function GetMovement: TVKMovement;
  public
    constructor Create(aOwner: TVKMovement);
    function Add: TVKMovementPath;
    function FindItemID(const ID: integer): TVKMovementPath;
    property Items[const index: integer]: TVKMovementPath Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;


  //Event for path related event
  TPathTravelStartEvent = procedure (Sender: TObject; 
    Path: TVKMovementPath) of object;
  TPathTravelStopEvent = procedure (Sender: TObject;
    Path: TVKMovementPath; var Looped: boolean) of object;

  TVKMovement = class(TVKBehaviour)
  private
    FPaths: TVKMovementPaths;
    FAutoStartNextPath: boolean;
    FActivePathIndex: integer;

    FOnAllPathTravelledOver: TNotifyEvent;
    FOnPathTravelStart: TPathTravelStartEvent;
    FOnPathTravelStop: TPathTravelStopEvent;
    {
    function GetMovementPath(Index: integer): TVKMovementPath;
    procedure SetMovementPath(Index: integer; AValue: TVKMovementPath);
    }
    function GetPathCount: integer;
    procedure SetActivePathIndex(Value: integer);

    function GetActivePath: TVKMovementPath;
    procedure SetActivePath(Value: TVKMovementPath);
  protected
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure PathTravelStart(Sender: TObject);
    procedure PathTravelStop(Sender: TObject);

    function GetSceneObject: TVKBaseSceneObject;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;

      //add an empty path;
    function AddPath: TVKMovementPath; overload;
    //add an path with one node, and the node is based on aObject
    function AddPath(aObject: TVKBaseSceneObject): TVKMovementPath; overload;
    //add one path to the new one
    function AddPath(Path: TVKMovementPath): TVKMovementPath; overload;
    procedure ClearPaths;
      //Result is current path
    function DeletePath(Path: TVKMovementPath): TVKMovementPath; overload;
    function DeletePath(Index: integer): TVKMovementPath; overload;
    function DeletePath: TVKMovementPath; overload;

    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;

    procedure StartPathTravel;
    procedure StopPathTravel;

    procedure DoProgress(const progressTime : TProgressTimes); override;

    function NextPath: integer;
    function PrevPath: integer;
    function FirstPath: integer;
    function LastPath: integer;

      //property Paths[index: Integer]: TVKMovementPath read GetMovementPath write SetMovementPath;
    property PathCount: integer Read GetPathCount;

    //why do these property can't be saved in IDE ?
    property OnAllPathTravelledOver: TNotifyEvent Read FOnAllPathTravelledOver Write FOnAllPathTravelledOver;
    property OnPathTravelStart: TPathTravelStartEvent Read FOnPathTravelStart Write FOnPathTravelStart;
    property OnPathTravelStop: TPathTravelStopEvent Read FOnPathTravelStop Write FOnPathTravelStop;
  published
    property Paths: TVKMovementPaths Read FPaths;
    property AutoStartNextPath: boolean Read FAutoStartNextPath Write FAutoStartNextPath;
    property ActivePathIndex: integer Read FActivePathIndex Write SetActivePathIndex;
    property ActivePath: TVKMovementPath Read GetActivePath Write SetActivePath;
  end;

function GetMovement(const behaviours: TVKBehaviours): TVKMovement; overload;
function GetMovement(const obj: TVKBaseSceneObject): TVKMovement; overload;
function GetOrCreateMovement(const behaviours: TVKBehaviours): TVKMovement; overload;
function GetOrCreateMovement(const obj: TVKBaseSceneObject): TVKMovement; overload;
procedure StartAllMovements(const Scene: TVKScene; const StartCamerasMove, StartObjectsMove: Boolean);
procedure StopAllMovements(const Scene: TVKScene; const StopCamerasMove, StopObjectsMove: Boolean);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
//----------------------------- TVKPathNode ------------------------------------
constructor TVKPathNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition  := VectorMake(0, 0, 0, 1);
  FRotation  := VectorMake(0, 0, 0, 1);
  FScale     := VectorMake(1, 1, 1, 1);
  FDirection := ZHmgVector;
  FUp        := YHmgVector;
  FSpeed     := 0;
end;

destructor TVKPathNode.Destroy;
begin
  inherited Destroy;
end;

procedure TVKPathNode.SetPositionAsVector(const Value: TVector);
begin
  FPosition := Value;
    (Collection as TVKPathNodes).NotifyChange;
end;

procedure TVKPathNode.SetRotationAsVector(const Value: TVector);
begin
  FRotation := Value;
    (Collection as TVKPathNodes).NotifyChange;
end;

procedure TVKPathNode.SetScaleAsVector(const Value: TVector);
begin
  FScale := Value;
    (Collection as TVKPathNodes).NotifyChange;
end;

function TVKPathNode.PositionAsAddress: PGLFloat;
begin
  Result := @FPosition;
end;

function TVKPathNode.RotationAsAddress: PGLFloat;
begin
  Result := @FRotation;
end;

function TVKPathNode.ScaleAsAddress: PGLFloat;
begin
  Result := @FScale;
end;

procedure TVKPathNode.WriteToFiler(writer : TWriter);
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

procedure TVKPathNode.ReadFromFiler(reader : TReader);
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

procedure TVKPathNode.InitializeByObject(const Obj: TVKBaseSceneObject);
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

procedure TVKPathNode.Assign(Source: TPersistent);
begin
  if Source is TVKPathNode then
  begin
    FPosition := TVKPathNode(Source).FPosition;
    FRotation := TVKPathNode(Source).FRotation;
    FScale    := TVKPathNode(Source).FScale;
    FSpeed    := TVKPathNode(Source).FSpeed;

    FDirection := TVKPathNode(Source).FDirection;
    FUp        := TVKPathNode(Source).FUp;
  end else
    inherited Assign(Source);
end;

function TVKPathNode.EqualNode(const aNode: TVKPathNode): boolean;
begin
  Result := VectorEquals(FPosition, aNode.FPosition) and
            VectorEquals(FRotation, aNode.FRotation) and
            VectorEquals(FScale, aNode.FScale) and
            VectorEquals(FDirection, aNode.FDirection) and
            VectorEquals(FUp, aNode.FUp);
end;

procedure TVKPathNode.SetSpeed(const Value: single);
begin
  FSpeed := Value;
end;

function TVKPathNode.GetDisplayName: string;
begin
  Result := 'PathNode';
end;

function TVKPathNode.GetPositionCoordinate(const Index: Integer): TVKFloat;
begin
  result := FPosition.V[Index];
end;

procedure TVKPathNode.SetPositionCoordinate(const Index: integer; const AValue: TVKFloat);
begin
  FPosition.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVKPathNodes).NotifyChange;
end;

function TVKPathNode.GetRotationCoordinate(const Index: Integer): TVKFloat;
begin
  result := FRotation.V[Index];
end;

procedure TVKPathNode.SetRotationCoordinate(const Index: integer; const AValue: TVKFloat);
begin
  FRotation.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVKPathNodes).NotifyChange;
end;

function TVKPathNode.GetScaleCoordinate(const Index: Integer): TVKFloat;
begin
  result := FScale.V[Index];
end;

procedure TVKPathNode.SetScaleCoordinate(const Index: integer; const AValue: TVKFloat);
begin
  FScale.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVKPathNodes).NotifyChange;
end;


function TVKPathNode.GetDirectionCoordinate(const Index: Integer): TVKFloat;
begin
  result := FDirection.V[Index];
end;


procedure TVKPathNode.SetDirectionCoordinate(const Index: integer;
  const AValue: TVKFloat);
begin
  FDirection.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVKPathNodes).NotifyChange;
end;

function TVKPathNode.GetUpCoordinate(const Index: Integer): TVKFloat;
begin
  result := FUp.V[Index];
end;

procedure TVKPathNode.SetUpCoordinate(const Index: integer; const AValue: TVKFloat);
begin
  FUp.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TVKPathNodes).NotifyChange;
end;

//--------------------------- TVKPathNodes -------------------------------------
constructor TVKPathNodes.Create(aOwner: TVKMovementPath);
begin
  inherited Create(aOwner, TVKPathNode);
end;

procedure TVKPathNodes.SetItems(const index: integer; const val: TVKPathNode);
begin
  inherited Items[index] := val;
end;

function TVKPathNodes.GetItems(const index: integer): TVKPathNode;
begin
  Result := TVKPathNode(inherited Items[index]);
end;

function TVKPathNodes.Add: TVKPathNode;
begin
  Result := (inherited Add) as TVKPathNode;
end;

function TVKPathNodes.GetOwnerMovementPath: TVKMovementPath;
begin
  Result := TVKMovementPath(GetOwner);
end;

function TVKPathNodes.FindItemID(const ID: integer): TVKPathNode;
begin
  Result := (inherited FindItemID(ID)) as TVKPathNode;
end;

procedure TVKPathNodes.NotifyChange;
begin
  // Update the path-line if avalible in TVKMovementPath.
  GetOwnerMovementPath.UpdatePathLine;
end;

//--------------------------- TVKMovementPath ----------------------------------
constructor TVKMovementPath.Create(Collection: TCollection);
begin
  // This object can only be added to a TVKMovement class.
  inherited Create(Collection);

  FNodes := TVKPathNodes.Create(Self);
  FCurrentNodeIndex := -1;
  FRotationMode := rmTurnPitchRoll;
  FPathSplineMode := lsmCubicSpline;
  FStartTimeApplied := False;  
end;

destructor TVKMovementPath.Destroy;
begin
  // Make sure the splines are freed.
  FLooped:= false;
  
  ClearNodes;
  FNodes.Free;

  inherited Destroy;
end;

procedure TVKMovementPath.WriteToFiler(writer : TWriter);
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

procedure TVKMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Node: TVKPathNode;
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
        FRotationMode := TVKMovementRotationMode(ReadInteger);
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

procedure TVKMovementPath.SetPathSplineMode(const Value: TLineSplineMode);
begin
  if Value<>FPathSplineMode then
  begin
    FPathSplineMode := Value;
    if FShowPath then
      FPathLine.SplineMode := FPathSplineMode;
  end;
end;

procedure TVKMovementPath.UpdatePathLine;
var
  I: Integer;
  Node: TVKPathNode;
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

procedure TVKMovementPath.SetShowPath(const Value: Boolean);
var
  OwnerObj: TVKBaseSceneObject;
begin
  if FShowPath<>Value then
  begin
    FShowPath := Value;
    OwnerObj := GetMovement.GetSceneObject;
    if FShowPath then
    begin
      FPathLine := TVKLines.Create(OwnerObj);
      MakeSubComponent(FPathLine, True);
      OwnerObj.Scene.Objects.AddChild(FPathLine);
      FPathLine.SplineMode := FPathSplineMode;
      UpdatePathLine;
    end
    else
      FreeAndNil(FPathLine);
  end;
end;

procedure TVKMovementPath.ClearNodes;
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

procedure TVKMovementPath.SetCurrentNodeIndex(const Value: integer);
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
      FCurrentNode := TVKPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[FCurrentNodeIndex]);
  end;
end;

function TVKMovementPath.InsertNode(const Node: TVKPathNode; const Index: integer): TVKPathNode;
var
  N: TVKPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Node)) and (Assigned(Nodes[Index])) then
  begin
    N := TVKPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
  end
  else
    //add to the tail of list
    N    := FNodes.Add;
  Result := N;
  UpdatePathLine;
end;

function TVKMovementPath.InsertNode(const Index: integer): TVKPathNode;
var
  N: TVKPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Nodes[Index])) then
  begin
    N := TVKPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
    Result := N;
  end
  else
    //add to the tail of list
    Result := AddNode;
  UpdatePathLine;
end;

function TVKMovementPath.AddNodeFromObject(const Obj: TVKBaseSceneObject): TVKPathNode;
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

function TVKMovementPath.InsertNodeFromObject(const Obj: TVKBaseSceneObject; const Index: integer): TVKPathNode;
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

function TVKMovementPath.DeleteNode(const Index: integer): TVKPathNode;
var
  Node: TVKPathNode;
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

function TVKMovementPath.DeleteNode(const Node: TVKPathNode): TVKPathNode;
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

function TVKMovementPath.PrevNode: integer;
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

function TVKMovementPath.NextNode: integer;
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

function TVKMovementPath.NodeDistance(const Node1, Node2: TVKPathNode): double;
begin
  Result := VectorDistance(Node1.FPosition, Node2.FPosition);
end;

//need to do
//1 No acceleration implemented
//2 The travel-time of a segment is based a simple linear movement, at the start and the end
//  of the segment, the speed will be more high than in the middle
//3 Rotation Interpolation has not been tested
procedure TVKMovementPath.CalculateState(const CurrentTime: double);
var
  I:       integer;
  SumTime: double;
  L, L2:       single;
  Interpolated: boolean;
  T:       double;
  a:double;

  procedure Interpolation(ReturnNode: TVKPathNode; Time1, Time2: double; Index: integer);
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


function TVKMovementPath.CanTravel: boolean;
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

function TVKMovementPath.GetCollection: TVKMovementPaths;
begin
  Result := TVKMovementPaths(GetOwner);
end;

function TVKMovementPath.GetMovement: TVKMovement;
begin
  Result := GetCollection.GetMovement;
end;

procedure TVKMovementPath.TravelPath(const Start: boolean);
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
      PFloatArray(x)[I]  := Nodes[I].FPosition.V[0];
      PFloatArray(y)[I]  := Nodes[I].FPosition.V[1];
      PFloatArray(z)[I]  := Nodes[I].FPosition.V[2];
      PFloatArray(p)[I]  := Nodes[I].FRotation.V[0];
      PFloatArray(t)[I]  := Nodes[I].FRotation.V[1];
      PFloatArray(r)[I]  := Nodes[I].FRotation.V[2];
      PFloatArray(sx)[I] := Nodes[I].FScale.V[0];
      PFloatArray(sy)[I] := Nodes[I].FScale.V[1];
      PFloatArray(sz)[I] := Nodes[I].FScale.V[2];

      PFloatArray(dx)[I] := Nodes[I].FDirection.V[0];
      PFloatArray(dy)[I] := Nodes[I].FDirection.V[1];
      PFloatArray(dz)[I] := Nodes[I].FDirection.V[2];

      PFloatArray(ux)[I] := Nodes[I].FUp.V[0];
      PFloatArray(uy)[I] := Nodes[I].FUp.V[1];
      PFloatArray(uz)[I] := Nodes[I].FUp.V[2];

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
    FCurrentNode := TVKPathNode.Create(nil);
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

procedure TVKMovementPath.TravelPath(const Start: boolean; const aStartTime: double);
begin
  if FInTravel = Start then
    exit;
  FInitialTime := aStartTime;
  FStartTimeApplied := False;
  TravelPath(Start);
end;

function TVKMovementPath.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

//-------------------------- This function need modified -----------------------
procedure TVKMovementPath.SetStartTime(const Value: double);
begin
  FStartTime := Value;
end;

procedure TVKMovementPath.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TVKMovementPath then
  begin
    ClearNodes;
    for I := 0 to TVKMovementPath(Source).NodeCount - 1 do
    begin
      AddNode;
      Nodes[I].Assign(TVKMovementPath(Source).Nodes[I]);
      FStartTime := TVKMovementPath(Source).FStartTime;
      //FEstimateTime := TVKMovementPath(Source).FEstimateTime;
      FLooped := TVKMovementPath(Source).FLooped;
      FRotationMode := TVKMovementPath(Source).FRotationMode;
    end;
  end;
end;

function TVKMovementPath.AddNode: TVKPathNode;
var
  Node: TVKPathNode;
  I:    integer;
begin
  //Add a empty node, if it's not the first one, try locate the node to the previous one
  Node := FNodes.Add;
  I    := FNodes.Count;
  if I > 1 then
    Node.Assign(Nodes[I - 2]);
  Result := Node;
end;

function TVKMovementPath.AddNode(const Node: TVKPathNode): TVKPathNode;
begin
  Result := AddNode;
  if Assigned(Node) then
    Result.Assign(Node);
end;

//------------------------- TVKMovementPaths ----------------------------------
constructor TVKMovementPaths.Create(aOwner: TVKMovement);
begin
  inherited Create(aOwner, TVKMovementPath);
end;

procedure TVKMovementPaths.SetItems(const index: integer; const val: TVKMovementPath);
begin
  inherited Items[index] := val;
end;

function TVKMovementPaths.GetItems(const index: integer): TVKMovementPath;
begin
  Result := TVKMovementPath(inherited Items[index]);
end;

function TVKMovementPaths.Add: TVKMovementPath;
begin
  Result := (inherited Add) as TVKMovementPath;
end;

function TVKMovementPaths.FindItemID(const ID: integer): TVKMovementPath;
begin
  Result := (inherited FindItemID(ID)) as TVKMovementPath;
end;

procedure TVKMovementPaths.NotifyChange;
begin
  // Do nothing here.
end;

function TVKMovementPaths.GetMovement: TVKMovement;
begin
  Result := TVKMovement(GetOwner);
end;


//--------------------------- TVKMovement --------------------------------------
constructor TVKMovement.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FPaths           := TVKMovementPaths.Create(Self);
  FAutoStartNextPath := True;
  FActivePathIndex := -1;
  FOnAllPathTravelledOver := nil;
  FOnPathTravelStart := nil;
  FOnPathTravelStop := nil;
end;

destructor TVKMovement.Destroy;
begin
  ClearPaths;
  FPaths.Free;
  inherited Destroy;
end;

procedure TVKMovement.WriteToFiler(writer : TWriter);
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

procedure TVKMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Path: TVKMovementPath;
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

procedure TVKMovement.ClearPaths;
begin
  StopPathTravel;
  FPaths.Clear;
  FActivePathIndex := -1;
end;

procedure TVKMovement.PathTravelStart(Sender: TObject);
begin
  if Assigned(FOnPathTravelStart) then
    FOnPathTravelStart(Self, TVKMovementPath(Sender));
end;

procedure TVKMovement.PathTravelStop(Sender: TObject);
begin
  if Assigned(FOnPathTravelStop) then
    FOnPathTravelStop(Self, TVKMovementPath(Sender), TVKMovementPath(Sender).FLooped);
  if TVKMovementPath(Sender).FLooped then
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

function TVKMovement.GetSceneObject: TVKBaseSceneObject;
begin
  Result := TVKBaseSceneObject(Owner{TVKBehavours}.Owner);
end;

function TVKMovement.AddPath: TVKMovementPath;
var
  Path: TVKMovementPath;
begin
  Path   := FPaths.Add;
  Path.OnTravelStart := PathTravelStart;
  Path.OnTravelStop := PathTravelStop;
  Result := Path;
end;

function TVKMovement.AddPath(aObject: TVKBaseSceneObject): TVKMovementPath;
begin
  Result := AddPath;
  Result.AddNodeFromObject(aObject);
end;

function TVKMovement.AddPath(Path: TVKMovementPath): TVKMovementPath;
begin
  Result := AddPath;
  if Assigned(Path) then
    Result.Assign(Path);
end;

function TVKMovement.DeletePath: TVKMovementPath;
begin
  Result := DeletePath(FActivePathIndex);
end;

function TVKMovement.DeletePath(Path: TVKMovementPath): TVKMovementPath;
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

function TVKMovement.DeletePath(Index: integer): TVKMovementPath;
begin
  Result := nil;
  if (Index <0) or (Index >=FPaths.Count) then
    exit;

  if Index >=0 then
  begin
    TVKMovementPath(FPaths[Index]).Free;
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

procedure TVKMovement.SetActivePathIndex(Value: integer);
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

function TVKMovement.NextPath: integer;
begin
  ActivePathIndex := FActivePathIndex + 1;
  Result           := FActivePathIndex;
end;

function TVKMovement.PrevPath: integer;
begin
  ActivePathIndex := FActivePathIndex - 1;
  if (FActivePathIndex < 0) and (FPaths.Count > 0) then
    Result := 0
  else
    Result := FActivePathIndex;
end;

function TVKMovement.FirstPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := 0;
  Result              := FActivePathIndex;
end;

function TVKMovement.LastPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := FPaths.Count - 1;
  Result              := FActivePathIndex;
end;

function TVKMovement.GetActivePath: TVKMovementPath;
begin
  if FActivePathIndex >= 0 then
    Result := Paths[FActivePathIndex]
  else
    Result := nil;
end;

procedure TVKMovement.SetActivePath(Value: TVKMovementPath);
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

function TVKMovement.GetPathCount: integer;
begin
  Result := FPaths.Count;
end;

procedure TVKMovement.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TVKMovement then
  begin
    ClearPaths;
    for I := 0 to TVKMovement(Source).PathCount - 1 do
    begin
      AddPath;
      Paths[I].Assign(TVKMovement(Source).Paths[I]);
    end;
    FAutoStartNextPath := TVKMovement(Source).FAutoStartNextPath;
  end;
end;

class function TVKMovement.FriendlyName: string;
begin
  Result := 'Movement controls'
end;

class function TVKMovement.FriendlyDescription: string;
begin
  Result := 'Object movement path controls'
end;

class function TVKMovement.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TVKMovement.StartPathTravel;
begin
  if FActivePathIndex < 0 then
    exit;
  //convert the time to second
  Paths[FActivePathIndex].TravelPath(True, 0);
end;

procedure TVKMovement.StopPathTravel;
var
  I: Integer;
begin
  if FPaths.Count <> 0 then
    for I := 0 to FPaths.Count - 1 do
      Paths[I].TravelPath(False);
end;

//Calculate functions add into this method
procedure TVKMovement.DoProgress(const progressTime : TProgressTimes);
var
  Path: TVKMovementPath;
begin
  if (FActivePathIndex >= 0) and (Paths[FActivePathIndex].InTravel) then
    begin
      Path := Paths[FActivePathIndex];
      Path.CalculateState(progressTime.newTime);
      if Assigned(Path.CurrentNode) then
      begin
        if Owner.Owner is TVKBaseSceneObject then
          with TVKBaseSceneObject(Owner.Owner) do
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
              Assert(False, glsErrorEx + glsUnknownType);
            end
          end;
      end;
    end;
end;


function GetMovement(const behaviours: TVKBehaviours): TVKMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKMovement);
  if i >= 0 then
    Result := TVKMovement(behaviours[i])
  else
    Result := nil;
end;

function GetMovement(const obj: TVKBaseSceneObject): TVKMovement; overload;
begin
  Result := GetMovement(obj.behaviours);
end;

function GetOrCreateMovement(const behaviours: TVKBehaviours): TVKMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKMovement);
  if i >= 0 then
    Result := TVKMovement(behaviours[i])
  else
    Result := TVKMovement.Create(behaviours);
end;

function GetOrCreateMovement(const obj: TVKBaseSceneObject): TVKMovement; overload;
begin
  Result := GetOrCreateMovement(obj.behaviours);
end;

procedure StartStopTravel(const Obj: TVKBaseSceneObject; Start: Boolean; ChangeCameras, ChangeObjects: Boolean);
var
  NewObj: TVKBaseSceneObject;
  I: Integer;
  Movement: TVKMovement;
begin
  if ((Obj is TVKCamera)and(ChangeCameras))or
     ((not(Obj is TVKCamera))and(ChangeObjects))  then
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

procedure StartAllMovements(const Scene: TVKScene; const StartCamerasMove, StartObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StartCamerasMove or StartObjectsMove then
      StartStopTravel(Scene.Objects, True, StartCamerasMove, StartObjectsMove);
  end;
end;

procedure StopAllMovements(const Scene: TVKScene; const StopCamerasMove, StopObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StopCamerasMove or StopObjectsMove then
      StartStopTravel(Scene.Objects, False, StopCamerasMove, StopObjectsMove);
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  // class registrations
  RegisterXCollectionItemClass(TVKMovement);

finalization

  UnregisterXCollectionItemClass(TVKMovement);

end.



