//
// GLScene on Vulkan, http://glscene.sourceforge.net 
//
{
   FPS-like movement behaviour and manager. 
  
}
unit GLS.FPSMovement;

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils, System.UITypes,
  FMX.Graphics,

  GLS.OpenGLTokens, GLS.Context, GLS.CrossPlatform, GLS.VectorGeometry,
  GLS.Scene, GLS.VectorFileObjects, GLS.VectorLists, GLS.XCollection,
  GLS.GeomObjects, GLS.Navigator, GLS.RenderContextInfo, GLS.BaseClasses,
  GLS.Manager, GLS.State;

type
  TContactPoint = record
    intPoint, intNormal: TVector;
  end;

  TCollisionState = class
  public
    Position: TVector;
    Contact: TContactPoint;
    Time: Int64;
  end;

  TCollisionStates = class(TList)
  end;

  TVKBFPSMovement = class;

  TVKMapCollectionItem = class(TVKXCollectionItem)
  private
    FMap: TVKFreeForm;
    FMapName: string;
    FCollisionGroup: integer;

    procedure setMap(value: TVKFreeForm);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(aOwner: TVKXCollection); override;
    class function FriendlyName: String; override;
  published

    property Map: TVKFreeForm read FMap write setMap;

    { Indicates the collision group of this map. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1).
    }
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
  end;

  TVKMapCollectionItemClass = class of TVKMapCollectionItem;

  TVKMapCollection = class(TVKXCollection)
  public
    class function ItemsClass: TVKXCollectionItemClass; override;
    function addMap(Map: TVKFreeForm; CollisionGroup: integer = 0)
      : TVKMapCollectionItem;
    function findMap(mapFreeForm: TVKFreeForm): TVKMapCollectionItem;
  end;

  TVKFPSMovementManager = class(TComponent)
  private
    FNavigator: TVKNavigator;
    FDisplayTime: integer;
    FMovementScale: single;
    FMaps: TVKMapCollection;
    FScene: TVKScene;

    procedure SetNavigator(value: TVKNavigator);
    procedure setScene(value: TVKScene);
    procedure DrawArrows(intPoint, intNormal, Ray: TVector;
      Arrow1, Arrow2: TVKArrowLine);
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteMaps(stream: TStream);
    procedure ReadMaps(stream: TStream);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Basic idea is to OctreeSphereSweepIntersect to plane, update position then change
    // velocity to slide along the plane
    // Camera can collide with multiple planes (e.g. floor + multiple walls + ceiling)
    // limit iterations to 4 or 5 for now, may need to be higher for more complex maps or fast motion
    function SphereSweepAndSlide(freeform: TVKFreeForm;
      behaviour: TVKBFPSMovement; SphereStart: TVector;
      var Velocity, newPosition: TVector; sphereRadius: single)
      : boolean; overload;

    procedure SphereSweepAndSlide(behaviour: TVKBFPSMovement;
      SphereStart: TVector; var Velocity, newPosition: TVector;
      sphereRadius: single); overload;

  published
    property Maps: TVKMapCollection read FMaps write FMaps;
    property Navigator: TVKNavigator read FNavigator write SetNavigator;
    property Scene: TVKScene read FScene write setScene;

    { Display Time for the arrow lines. }
    property DisplayTime: integer read FDisplayTime write FDisplayTime;
    property MovementScale: single read FMovementScale write FMovementScale;
  end;

  TVKBFPSMovement = class(TVKBehaviour)
  private
    FManager: TVKFPSMovementManager;
    CollisionStates: TCollisionStates;
    ArrowLine1, ArrowLine2, ArrowLine3, ArrowLine4, ArrowLine5,
      ArrowLine6: TVKArrowLine;
    dirGl: TVKDirectOpenGL;
    tickCount: Int64;

    oldPosition: TVector;

    FGravityEnabled: boolean;

    FSphereRadius: single;
    FShowArrows: boolean;
    FCollisionGroup: integer;
    FManagerName: string;

    procedure setShowArrows(value: boolean);
    procedure RenderArrowLines(Sender: TObject; var rci: TVKRenderContextInfo);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    Velocity: TVector;

    constructor Create(aOwner: TVKXCollection); override;
    destructor Destroy; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    class function FriendlyName: string; override;

    Procedure TurnHorizontal(Angle: single);
    Procedure TurnVertical(Angle: single);
    Procedure MoveForward(Distance: single);
    Procedure StrafeHorizontal(Distance: single);
    Procedure StrafeVertical(Distance: single);
    Procedure Straighten;
  published
    property Manager: TVKFPSMovementManager read FManager write FManager;

    { 
      Radius to execute the testing with. A value < 0 indicates to use
      the boundingSphereRadius of the object.
    }
    property sphereRadius: single read FSphereRadius write FSphereRadius;

    { Show Arrows and trailing for debuging. }
    property ShowArrows: boolean read FShowArrows write setShowArrows;

    { Indicates the collision group of this behaviour. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1).
    }
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;

    property GravityEnabled: boolean read FGravityEnabled write FGravityEnabled;
  end;

function GetFPSMovement(behaviours: TVKBehaviours): TVKBFPSMovement; overload;
function GetFPSMovement(obj: TVKBaseSceneObject): TVKBFPSMovement; overload;
function GetOrCreateFPSMovement(behaviours: TVKBehaviours)
  : TVKBFPSMovement; overload;
function GetOrCreateFPSMovement(obj: TVKBaseSceneObject)
  : TVKBFPSMovement; overload;

//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------

function GetFPSMovement(behaviours: TVKBehaviours): TVKBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKBFPSMovement);
  if i >= 0 then
    Result := TVKBFPSMovement(behaviours[i])
  else
    Result := nil;
end;

function GetFPSMovement(obj: TVKBaseSceneObject): TVKBFPSMovement; overload;
begin
  Result := GetFPSMovement(obj.behaviours);
end;

function GetOrCreateFPSMovement(behaviours: TVKBehaviours)
  : TVKBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TVKBFPSMovement);
  if i >= 0 then
    Result := TVKBFPSMovement(behaviours[i])
  else
    Result := TVKBFPSMovement.Create(behaviours);
end;

function GetOrCreateFPSMovement(obj: TVKBaseSceneObject)
  : TVKBFPSMovement; overload;
begin
  Result := GetOrCreateFPSMovement(obj.behaviours);
end;

// ------------------
// ------------------ TVKMapCollectionItem ------------------
// ------------------
constructor TVKMapCollectionItem.Create(aOwner: TVKXCollection);
begin
  inherited Create(aOwner);

  FCollisionGroup := 0;
end;

procedure TVKMapCollectionItem.setMap(value: TVKFreeForm);
begin
  assert(owner.owner.InheritsFrom(TVKFPSMovementManager));
  if assigned(FMap) then
    FMap.RemoveFreeNotification(TComponent(owner.owner));
  FMap := value;
  if assigned(FMap) then
    FMap.FreeNotification(TComponent(owner.owner));
end;

procedure TVKMapCollectionItem.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    writeInteger(0); // ArchiveVersion
    writeInteger(FCollisionGroup);
    if assigned(FMap) then
      WriteString(FMap.Name)
    else
      WriteString('');
  end;
end;

procedure TVKMapCollectionItem.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TVKMapCollectionItem');
    FCollisionGroup := readInteger;
    FMapName := ReadString;
  end;
end;

procedure TVKMapCollectionItem.Loaded;
begin
  if FMapName <> '' then
  begin
    assert(owner.owner.InheritsFrom(TVKFPSMovementManager));
    Map := TVKFreeForm(TVKFPSMovementManager(owner.owner)
      .Scene.FindSceneObject(FMapName));
  end;
end;

class function TVKMapCollectionItem.FriendlyName: String;
begin
  Result := 'FPSMovementMap';
end;

// ------------------
// ------------------ TVKMapCollection ------------------
// ------------------
class function TVKMapCollection.ItemsClass: TVKXCollectionItemClass;
begin
  Result := TVKMapCollectionItem;
end;

function TVKMapCollection.addMap(Map: TVKFreeForm; CollisionGroup: integer = 0)
  : TVKMapCollectionItem;
begin
  // no repeated maps (would only present delays...)
  Result := findMap(Map);
  if assigned(Result) then
    exit;

  Result := TVKMapCollectionItem.Create(self);
  Result.Map := Map;
  Result.CollisionGroup := CollisionGroup;
  add(Result);
end;

function TVKMapCollection.findMap(mapFreeForm: TVKFreeForm)
  : TVKMapCollectionItem;
var
  i: integer;
  aux: TVKMapCollectionItem;
begin
  Result := nil;
  for i := 0 to count - 1 do
  begin
    aux := TVKMapCollectionItem(Items[i]);
    if aux.Map = mapFreeForm then
    begin
      Result := aux;
      break;
    end;
  end;
end;

// ------------------
// ------------------ TVKFPSMovementManager ------------------
// ------------------
constructor TVKFPSMovementManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Maps := TVKMapCollection.Create(self);

  MovementScale := 4.0;
  DisplayTime := 2000;

  RegisterManager(self);
end;

destructor TVKFPSMovementManager.Destroy;
begin
  DeRegisterManager(self);
  Maps.Free;
  inherited Destroy;
end;

procedure TVKFPSMovementManager.Loaded;
begin
  inherited Loaded;
  if assigned(FMaps) then
    Maps.Loaded;
end;

// DefineProperties
//
procedure TVKFPSMovementManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  { FOriginalFiler := Filer; }

  Filer.DefineBinaryProperty('MapsData', ReadMaps, WriteMaps,
    (assigned(FMaps) and (FMaps.count > 0)));
  { FOriginalFiler:=nil; }
end;

// WriteBehaviours
//
procedure TVKFPSMovementManager.WriteMaps(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Maps.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadBehaviours
//
procedure TVKFPSMovementManager.ReadMaps(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Maps.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TVKFPSMovementManager.SetNavigator(value: TVKNavigator);
begin
  if assigned(FNavigator) then
    FNavigator.RemoveFreeNotification(self);
  FNavigator := value;
  if assigned(value) then
    value.FreeNotification(self);
end;

procedure TVKFPSMovementManager.setScene(value: TVKScene);
begin
  if assigned(FScene) then
    FScene.RemoveFreeNotification(self);
  FScene := value;
  if assigned(FScene) then
    FScene.FreeNotification(self);
end;

procedure TVKFPSMovementManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Map: TVKMapCollectionItem;
begin
  inherited Notification(AComponent, Operation);

  if Operation <> opRemove then
    exit;

  if (AComponent = FNavigator) then
    Navigator := nil;
  if (AComponent = FScene) then
    FScene := nil;
  if AComponent.InheritsFrom(TVKFreeForm) then
  begin
    Map := Maps.findMap(TVKFreeForm(AComponent));
    if assigned(Map) then
      Map.Map := nil;
  end;
end;

procedure TVKFPSMovementManager.DrawArrows(intPoint, intNormal, Ray: TVector;
  Arrow1, Arrow2: TVKArrowLine);
begin
  Arrow1.Position.AsVector := intPoint;
  Arrow1.Direction.AsVector := intNormal;
  Arrow1.Scale.z := VectorLength(intNormal);
  Arrow1.Move(Arrow1.Scale.z / 2);
  Arrow1.Visible := True;

  Arrow2.Position.AsVector := intPoint;
  Arrow2.Direction.AsVector := Ray;
  Arrow2.Visible := True;
end;

procedure TVKFPSMovementManager.SphereSweepAndSlide(behaviour: TVKBFPSMovement;
  SphereStart: TVector; var Velocity, newPosition: TVector;
  sphereRadius: single);
var
  i: integer;
  Map: TVKMapCollectionItem;
begin
  for i := 0 to Maps.count - 1 do
  begin
    Map := TVKMapCollectionItem(Maps.GetItems(i));
    if Map.CollisionGroup = behaviour.CollisionGroup then
      SphereSweepAndSlide(Map.Map, behaviour, SphereStart, Velocity,
        newPosition, sphereRadius)
  end;
end;

function TVKFPSMovementManager.SphereSweepAndSlide(freeform: TVKFreeForm;
  behaviour: TVKBFPSMovement; SphereStart: TVector;
  var Velocity, newPosition: TVector; sphereRadius: single): boolean;
var
  oldPosition, Ray: TVector;
  vel, slidedistance: single;
  intPoint, intNormal: TVector;
  newDirection, newRay, collisionPosition, pointOnSphere,
    point2OnSphere: TVector;
  i: integer;
  CollisionState: TCollisionState;
  SphereRadiusRel: single; // mrqzzz
begin
  SphereRadiusRel := sphereRadius / freeform.Scale.x;
  // could be Scale.y, or Scale.z assuming they are the same

  oldPosition := SphereStart;

  Result := True;

  // Direction sphere is moving in
  Ray := VectorSubtract(newPosition, oldPosition);
  // ray:=Velocity;
  // newPosition:=VectorAdd(newPosition,ray);
  // Speed of sphere
  vel := VectorLength(Ray);

  // if the Sphere is not moving, nothing is required
  // else do up to 7 loops

  if vel > 0 then
    for i := 0 to 6 do
    begin
      // if an intersection occurs, will need to do further calculations
      if (freeform.OctreeSphereSweepIntersect(oldPosition, Ray, vel,
        SphereRadiusRel, @intPoint, @intNormal)) then
      begin
        if VectorDistance2(oldPosition, intPoint) <= sqr(sphereRadius) then
        begin
          // sphere is intersecting triangle
          intNormal := VectorScale(VectorSubtract(oldPosition,
            intPoint), 1.0001);
        end
        else
        begin
          // sphere is not intersecting triangle
          // intNormal:=VectorSubtract(oldPosition,intPoint);  //not correct but works okay at small time steps
          // intNormal:=VectorScale(VectorNormalize(intNormal),SphereRadius+0.0001);
          if RayCastSphereInterSect(intPoint, VectorNormalize(VectorNegate(Ray)
            ), oldPosition, sphereRadius, pointOnSphere, point2OnSphere) > 0
          then
            intNormal := VectorScale(VectorSubtract(oldPosition,
              pointOnSphere), 1.0001)
            // intNormal:=VectorScale(VectorNormalize(VectorSubtract(oldPosition,PointOnSphere)),SphereRadius+0.001)//VectorDistance(oldPosition,PointOnSphere));
          else
          begin
            // Assert(False);  //this shouldn't happen (this is here for debugging)
            intNormal := VectorScale(VectorSubtract(oldPosition,
              intPoint), 1.0001);
          end;

        end;

        // calculate position of centre of sphere when collision occurs
        collisionPosition := VectorAdd(intPoint, intNormal);
        oldPosition := collisionPosition;

        // calculate distance that wasn't travelled, due to obstacle
        newRay := VectorSubtract(newPosition, collisionPosition);

        // calculate new direction when a wall is hit (could add bouncing to this)
        newDirection := VectorCrossProduct(intNormal,
          VectorCrossProduct(newRay, intNormal));
        if VectorNorm(newDirection) > 0 then
          NormalizeVector(newDirection);

        // calculate distance that it should slide (depends on angle between plane & ray)
        slidedistance := vectorDotProduct(newRay, newDirection);
        // still need to implement friction properly
        // if abs(SlideDistance)<10*deltaTime then SlideDistance:=0;
        ScaleVector(newDirection, slidedistance);

        // calculate new position sphere is heading towards
        newPosition := VectorAdd(collisionPosition, newDirection);
        Ray := newDirection;
        vel := VectorLength(Ray);

        // display arrows for collision normals & slide direction
        if (i = 0) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine1,
            behaviour.ArrowLine4)
        else if (i = 1) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine2,
            behaviour.ArrowLine5)
        else if (i = 2) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine3,
            behaviour.ArrowLine6)
        else if i = 6 then
        begin
          // caption:=FloatToStr(vectordistance(newPosition,oldPosition));
          newPosition := oldPosition;
          break;
        end;

        // check if very small motion (e.g. when stuck in a corner)
        if vel < 1E-10 then // deltaTime then
        begin
          newPosition := oldPosition;
          break;
        end;

        CollisionState := TCollisionState.Create();
        CollisionState.Position := oldPosition;
        CollisionState.Contact.intNormal := intNormal;
        CollisionState.Contact.intPoint := intPoint;
        CollisionState.Time := GLGetTickCount();

        behaviour.CollisionStates.add(CollisionState);

      end
      else // no collision occured, so quit loop
      begin
        if i = 0 then
          Result := false;
        break;
      end;
    end; // end i loop
  Velocity := Ray;
end;


// ------------------
// ------------------ TVKBFPSMovement ------------------
// ------------------

constructor TVKBFPSMovement.Create(aOwner: TVKXCollection);

  procedure setupArrow(arrow: TVKArrowLine; color: TColor); //TDelphiColor
  begin
    with arrow do
    begin
      slices := 16;
      stacks := 4;
      TopArrowHeadHeight := 0.1;
      TopArrowHeadRadius := 0.04;
      TopRadius := 0.02;
      BottomArrowHeadHeight := 0.05;
      BottomArrowHeadRadius := 0.02;
      BottomRadius := 0.02;
      Material.FrontProperties.Diffuse.AsWinColor := color;
    end;
  end;

begin
  inherited Create(aOwner);

  Velocity := NullHmgVector;
  sphereRadius := -1;
  CollisionGroup := 0;

  CollisionStates := TCollisionStates.Create;

  // FIXME: Creating arrows here, but they should be only added when
  // a "showArrows" property changed
  ArrowLine1 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine1, TColors.Red);
  ArrowLine2 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine2, TColors.Green);
  ArrowLine3 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine3, TColors.Blue);
  ArrowLine4 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine4, TColors.Silver);
  ArrowLine5 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine5, TColors.Silver);
  ArrowLine6 := TVKArrowLine.Create(nil);
  setupArrow(ArrowLine6, TColors.Silver);

  dirGl := TVKDirectOpenGL.Create(nil);
  dirGl.OnRender := RenderArrowLines;

  oldPosition := OwnerBaseSceneObject.Position.AsVector;
  FManagerName := '';
end;

destructor TVKBFPSMovement.Destroy;
var
  i: integer;
begin
  // remove all states
  for i := 0 to CollisionStates.count - 1 do
    TCollisionState(CollisionStates[i]).Free;
  FreeAndNil(CollisionStates);
  // remove all objects used to display graphical results of collisions
  FreeAndNil(ArrowLine1);
  FreeAndNil(ArrowLine2);
  FreeAndNil(ArrowLine3);
  FreeAndNil(ArrowLine4);
  FreeAndNil(ArrowLine5);
  FreeAndNil(ArrowLine6);
  FreeAndNil(dirGl);
  inherited Destroy;
end;

class function TVKBFPSMovement.FriendlyName: String;
begin
  Result := 'FPS Movement';
end;

procedure TVKBFPSMovement.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    writeInteger(0); // ArchiveVersion 0 (initial)
    writeInteger(FCollisionGroup);
    WriteSingle(FSphereRadius);
    WriteBoolean(FGravityEnabled);
    WriteBoolean(FShowArrows);
    if assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TVKBFPSMovement.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TVKBFPSMovement');
    CollisionGroup := readInteger;
    sphereRadius := ReadSingle;
    GravityEnabled := ReadBoolean;
    ShowArrows := ReadBoolean;
    FManagerName := ReadString;
  end;
end;

procedure TVKBFPSMovement.Loaded;
var
  mng: TComponent;
begin
  inherited Loaded;
  if FManagerName <> '' then
  begin
    mng := FindManager(TVKFPSMovementManager, FManagerName);
    if assigned(mng) then
      Manager := TVKFPSMovementManager(mng);
    FManagerName := '';
  end;
end;

procedure TVKBFPSMovement.setShowArrows(value: boolean);
begin
  FShowArrows := value;
  dirGl.Visible := value;
  if (OwnerBaseSceneObject <> nil) and
    not(csDesigning in OwnerBaseSceneObject.ComponentState) then
  begin
    ArrowLine1.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine2.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine3.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine4.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine5.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine6.MoveTo(OwnerBaseSceneObject.Parent);
    dirGl.MoveTo(OwnerBaseSceneObject.Parent);
  end;
end;

procedure TVKBFPSMovement.MoveForward(Distance: single);
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.MoveForward(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.StrafeHorizontal(Distance: single);
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeHorizontal(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.StrafeVertical(Distance: single);
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeVertical(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.TurnHorizontal(Angle: single);
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnHorizontal(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.TurnVertical(Angle: single);
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnVertical(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.Straighten;
var
  prevObj: TVKBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TVKBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.Straighten;
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TVKBFPSMovement.DoProgress(const progressTime: TProgressTimes);
var
  newPosition: TVector;
  CollisionState: TCollisionState;
begin
  inherited DoProgress(progressTime);

  assert(assigned(Manager), 'FPS Manager not assigned to behaviour.');

  // make arrowlines invisible (they are made visible in SphereSweepAndSlide)
  ArrowLine1.Visible := false;
  ArrowLine2.Visible := false;
  ArrowLine3.Visible := false;
  ArrowLine4.Visible := false;
  ArrowLine5.Visible := false;
  ArrowLine6.Visible := false;

  CollisionState := TCollisionState.Create();
  CollisionState.Position := oldPosition;
  CollisionStates.add(CollisionState);

  // this is the position we are trying to move to with controls
  newPosition := OwnerBaseSceneObject.Position.AsVector;

  // Change in position = velocity * time taken
  if GravityEnabled then
    newPosition.V[1] := newPosition.V[1] - Manager.MovementScale * 0.5 *
      progressTime.deltaTime;

  // do some magic!!!  and store new position in newPosition
  if sphereRadius < 0 then
    Manager.SphereSweepAndSlide(self, oldPosition, Velocity, newPosition,
      OwnerBaseSceneObject.boundingSphereRadius)
  else
    Manager.SphereSweepAndSlide(self, oldPosition, Velocity, newPosition,
      sphereRadius);

  OwnerBaseSceneObject.Position.AsVector := newPosition;
  oldPosition := newPosition;

  if CollisionStates.count > 0 then
  begin
    CollisionState := TCollisionState(CollisionStates.First);
    tickCount := GLGetTickCount();
    // remove all old states
    while (CollisionState <> nil) and
      (CollisionState.Time < tickCount - Manager.DisplayTime) do
    begin
      CollisionStates.Remove(CollisionState);
      CollisionState.Free;
      if CollisionStates.count = 0 then
        exit;
      CollisionState := TCollisionState(CollisionStates.First);
    end;
  end;
end;

procedure TVKBFPSMovement.RenderArrowLines(Sender: TObject;
  var rci: TVKRenderContextInfo);
var
  x, y, z, t: single;
  i: integer;
  CollisionState: TCollisionState;
begin
  // caption:= IntToStr(CollisionStates.Count);
  GL.Color3f(1, 1, 1);
  rci.GLStates.Disable(stLighting);
  // draw position trail
  GL.Begin_(GL_LINE_STRIP);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TCollisionState(CollisionStates.Items[i]);
    x := CollisionState.Position.V[0];
    y := CollisionState.Position.V[1];
    z := CollisionState.Position.V[2];
    GL.Vertex3f(x, y, z);
  end;
  GL.End_();
  // draw normals trail
  GL.Begin_(GL_LINES);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TCollisionState(CollisionStates.Items[i]);
    t := (Manager.DisplayTime - (tickCount - CollisionState.Time)) /
      Manager.DisplayTime;
    GL.Color3f(t, t, t);
    GL.Vertex3f(CollisionState.Contact.intPoint.V[0],
      CollisionState.Contact.intPoint.V[1], CollisionState.Contact.intPoint.V[2]);
    GL.Vertex3f(CollisionState.Contact.intPoint.V[0] +
      CollisionState.Contact.intNormal.V[0], //GLSphere4.Radius,
      CollisionState.Contact.intPoint.V[1] + CollisionState.Contact.intNormal.V[1],
      //GLSphere4.Radius,
      CollisionState.Contact.intPoint.V[2] + CollisionState.Contact.intNormal.V[2]);
    //GLSphere4.Radius);
  end;
  GL.End_();
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TVKMapCollectionItem);
RegisterXCollectionItemClass(TVKBFPSMovement);

finalization

UnregisterXCollectionItemClass(TVKMapCollectionItem);
UnregisterXCollectionItemClass(TVKBFPSMovement);

end.
