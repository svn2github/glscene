{: GLDCE<p>

  How to use:
  - Add a DCEManager to you form and configure its properties
  - Add a Dynamic Collision Behavior to you object
  - Choose the shape of your object (csEllipsoid, csBox, csFreeform, csTerrain)
    - csEllipsoid can be any glscene object, and is the only that can be dynamic
      By dynamic I mean that it can move in the world.
    - csBox isn't implemented yet and works like an Ellipsoid
    - csFreeform MUST BE A TGLFreeform, otherwise will raise errors
    - csTerrain MUST BE A TGLTerrainRenderer, same condition above
  - Friction is a value aprox. between 0 (no friction) and 500 (no movement)
  - IsDynamic can be True if the object has an ellipsoid shape
  - Layer = An object collides only with lower or equal layers
  - Size is used for Ellipsoids (Radius) / Boxes (Dimensions)
  - Solid = An object can still get the collision event but it "walks-thru"

  <b>History : </b><font size=-1><ul>
    <li>03/09/04 - LucasG. - First release
    <li>29/07/04 - LucasG. - Creation
  </ul></font>
}

unit GLDCE;

interface

uses Classes, GLScene, XCollection, VectorGeometry, VectorLists, GLVectorFileObjects,
   GeometryBB, GLCrossPlatform, GLMisc, GLDCEMisc, GLEllipseCollision, GLTerrainRenderer;

type
  {Only csEllipsoid can have dynamic behaviour}
  TDCEShape = (csEllipsoid, csBox, csFreeform, csTerrain);

  TDCECollision = record
    Point: TAffineVector;
    Normal: TAffineVector; //Surface normal
    Bounce: TAffineVector; //Surface reflection
    Nearest: Boolean;
  end;

  TGLBDCEBody = class;

  TDCECollisionEvent = procedure (Sender : TObject; object1, object2 : TGLBaseSceneObject;
                                       CollisionInfo: TDCECollision) of object;
  TDCEObjectCollisionEvent = procedure (Sender : TObject; ObjectCollided : TGLBaseSceneObject;
                                       CollisionInfo: TDCECollision) of object;

  TGLDCEManager = class (TComponent)
  private
    { Private Declarations }
    FClients : TList;
    FOnCollision : TDCECollisionEvent;
    FGravity: TGLCoordinates;
    FMovimentScale: Single;
    procedure SetGravity(const Value: TGLCoordinates);
  protected
    { Protected Declarations }
    procedure RegisterClient(aClient : TGLBDCEBody);
    procedure DeRegisterClient(aClient : TGLBDCEBody);
    procedure DeRegisterAllClients;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MoveByDistance(var Body: TGLBDCEBody;
       deltaDistance: TAffineVector; deltaTime: Double): Single;
  published
    { Published Declarations }
    property OnCollision : TDCECollisionEvent read FOnCollision write FOnCollision;
    property Gravity : TGLCoordinates read FGravity write SetGravity;
    property MovimentScale : Single read FMovimentScale write FMovimentScale;
	end;

  TGLBDCEBody = class (TGLBehaviour)
	private
		{ Private Declarations }
    FManager : TGLDCEManager;
    FManagerName : String; // NOT persistent, temporarily used for persistence
    FShape: TDCEShape;
    FLayer: Integer; //Collides only with lower or equal layers
    FDynamic: Boolean; //Only ellipsoid shape can be dynamic
    FSolid: Boolean; //Collide and slide if true, otherwise it "walk thru walls"
    FFriction: Single; //aprox. 0 to 10
    FSize: TGLCoordinates;
    FTerrainRenderer: TGLTerrainRenderer;
    //Movement
    FForce: TAffineVector; //Current acceleration
    FSpeed: TAffineVector; //Current speed
    FInGround: Boolean;
    FGroundNormal: TAffineVector;
    FGroundBounce: TAffineVector;
    FJumpPos, FJumpHeight, FJumpForce: Single;
    FJumpSpeed: TAffineVector;
    FJumping: Boolean;
    //Events
    FOnCollision : TDCEObjectCollisionEvent;
    procedure SetShape(const Value: TDCEShape);
    procedure SetDynamic(const Value: Boolean);
    procedure SetFriction(const Value: Single);
    procedure SetSize(const Value: TGLCoordinates);
  protected
    { Protected Declarations }
    procedure SetManager(const val : TGLDCEManager);
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure Loaded; override;
  public
    { Public Declarations }
    constructor Create(aOwner : TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName : String; override;
    class function FriendlyDescription : String; override;
    procedure AddForce(NewForce: TAffineVector);
    procedure Jump(jHeight, jSpeed: Single);
    procedure DoMove(deltaTime: Double);
    procedure DoProgress(const progressTime : TProgressTimes); override;
    //Runtime only
    property Speed : TAffineVector read FSpeed write FSpeed;
    property InGround : Boolean read FInGround;
  published
    { Published Declarations }
    property Manager : TGLDCEManager read FManager write SetManager;
    property Shape : TDCEShape read FShape write SetShape;
    property Layer : Integer read FLayer write FLayer;
    property IsDynamic : Boolean read FDynamic write SetDynamic;
    property Solid : Boolean read FSolid write FSolid;
    property Friction : Single read FFriction write SetFriction;
    property Size : TGLCoordinates read FSize write SetSize;
    property OnCollision : TDCEObjectCollisionEvent read FOnCollision write FOnCollision;
  end;

function GetOrCreateDCECollision(behaviours : TGLBehaviours) : TGLBDCEBody; overload;
function GetOrCreateDCECollision(obj : TGLBaseSceneObject) : TGLBDCEBody; overload;

procedure Register;

implementation

uses SysUtils;

procedure register;
begin
  RegisterComponents('GLScene Utils', [TGLDCEManager]);
end;

constructor TGLDCEManager.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FClients:=TList.Create;
  FGravity:=TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FMovimentScale := 1;
  RegisterManager(Self);
end;

// Destroy
//
destructor TGLDCEManager.Destroy;
begin
	DeRegisterAllClients;
  DeRegisterManager(Self);
  FClients.Free;
  FGravity.Free;
	inherited Destroy;
end;

function TGLDCEManager.MoveByDistance(var Body: TGLBDCEBody;
   deltaDistance: TAffineVector; deltaTime: Double): Single;
var NewPosition: TVector;
    AbsGravity, AbsDistance: TAffineVector;
    AvgFriction: Single;
    ColInfo: TDCECollision;

  function MoveEllipsoid: TVector;
  var i, oi : Integer;
     MovePack: TECMovementPacket;
  begin
    MovePack.Radius := Body.Size.AsAffineVector;
    MovePack.Position := AffineVectorMake(Body.OwnerBaseSceneObject.AbsolutePosition);
    MovePack.Velocity := AbsDistance;
    MovePack.Gravity := AbsGravity;
    MovePack.Solid := Body.Solid;
    MovePack.InGround := Body.InGround;
    SetLength(MovePack.Triangles,0);
    SetLength(MovePack.Colliders,0);
    SetLength(MovePack.Terrains,0);

    for i:=0 to FClients.Count-1 do
    with TGLBDCEBody(FClients[i]) do begin

      if (Layer <= Body.Layer) and (TGLBDCEBody(FClients[i]) <> Body) then
      begin
        case Shape of
          csFreeform: AddFreeFormToMovePack(MovePack,
                        TGLFreeform(OwnerBaseSceneObject),i,Friction,Solid);
          csEllipsoid: AddEllipsoidToMovePack(MovePack,
                        AffineVectorMake(OwnerBaseSceneObject.AbsolutePosition),
                        Size.AsAffineVector,i,Friction,Solid);
          csTerrain:
            begin
              FTerrainRenderer := TGLTerrainRenderer(OwnerBaseSceneObject);
              AddTerrainToMovePack(MovePack,
                        FTerrainRenderer,i,Friction,Solid);
            end;

        end;
      end;

    end;
    CollideAndSlide(MovePack);
    result := VectorMake(MovePack.ResultPos);
    Body.FInGround := MovePack.InGround;
    Body.FGroundNormal := MovePack.ColNormal;
    Body.FGroundBounce := MovePack.ColBounce;

    //Get friction
    if MovePack.FoundCollision then
      AvgFriction := TGLBDCEBody(FClients[MovePack.ColObjectIndex]).Friction
    else
      AvgFriction := -1;

    //Generate events
    for i := 0 to High(MovePack.CollisionList) do
    begin
      ColInfo.Point := MovePack.CollisionList[i].Point;
      ColInfo.Normal := MovePack.CollisionList[i].Normal;
      ColInfo.Bounce := MovePack.CollisionList[i].Bounce;
      ColInfo.Nearest := MovePack.CollisionList[i].ObjectIndex = MovePack.ColObjectIndex;
      oi := MovePack.CollisionList[i].ObjectIndex;
      if Assigned(FOnCollision) then
        FOnCollision(Self,Body.OwnerBaseSceneObject,
                     TGLBDCEBody(FClients[oi]).OwnerBaseSceneObject,ColInfo);
      if Assigned(Body.FOnCollision) then
        Body.FOnCollision(Self,TGLBDCEBody(FClients[oi]).OwnerBaseSceneObject,ColInfo);
    end;
  end;

begin
  AbsGravity := VectorScale(FGravity.AsAffineVector,deltaTime * FMovimentScale);
  //AbsGravity := VectorScale(FGravity.AsAffineVector,0.1*FMovimentScale);
  AbsDistance := VectorScale(deltaDistance,FMovimentScale);
  with Body do
  begin
    case Shape of
      csEllipsoid: NewPosition := MoveEllipsoid;
    end;
    OwnerBaseSceneObject.AbsolutePosition := NewPosition;
    result := AvgFriction;
  end;
end;

procedure TGLDCEManager.SetGravity(const Value: TGLCoordinates);
begin
  FGravity := Value;
end;

// RegisterClient
//
procedure TGLDCEManager.RegisterClient(aClient : TGLBDCEBody);
begin
   if Assigned(aClient) then
      if FClients.IndexOf(aClient)<0 then begin
         FClients.Add(aClient);
         aClient.FManager:=Self;
      end;
end;

// DeRegisterClient
//
procedure TGLDCEManager.DeRegisterClient(aClient : TGLBDCEBody);
begin
   if Assigned(aClient) then begin
      aClient.FManager:=nil;
      FClients.Remove(aClient);
   end;
end;

// DeRegisterAllClients
//
procedure TGLDCEManager.DeRegisterAllClients;
var
   i : Integer;
begin
   // Fast deregistration
   for i:=0 to FClients.Count-1 do
      TGLBDCEBody(FClients[i]).FManager:=nil;
   FClients.Clear;
end;

{ TGLBDCEBody }

constructor TGLBDCEBody.Create(aOwner : TXCollection);
begin
   inherited Create(aOwner);
   FSize:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
   FShape := csEllipsoid;
   FSolid := True;
   FFriction := 1;
   FForce := NullVector;
end;

destructor TGLBDCEBody.Destroy;
begin
   Manager:=nil;
   FSize.Free;
   inherited Destroy;
end;

class function TGLBDCEBody.FriendlyName : String;
begin
   Result:='Dynamic Collision';
end;

class function TGLBDCEBody.FriendlyDescription : String;
begin
   Result:='Dynamic Collision-detection registration';
end;

procedure TGLBDCEBody.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(0); // ArchiveVersion 0
      if Assigned(FManager) then
         WriteString(FManager.GetNamePath)
      else WriteString('');
      WriteInteger(Integer(FShape));
      WriteInteger(FLayer);
      WriteBoolean(FDynamic);
      WriteBoolean(FSolid);
      WriteSingle(FFriction);
      FSize.WriteToFiler(writer);
   end;
end;

// ReadFromFiler
//
procedure TGLBDCEBody.ReadFromFiler(reader : TReader);
var
   archiveVersion : Integer;
begin
   with reader do begin
      archiveVersion:=ReadInteger;
      Assert(archiveVersion = 0);
      FManagerName:=ReadString;
      Manager:=nil;
      FShape := TDCEShape(ReadInteger);
      FLayer := ReadInteger;
      FDynamic := ReadBoolean;
      FSolid := ReadBoolean;
      Friction := ReadSingle;
      FSize.ReadFromFiler(reader);
   end;
end;

// Loaded
//
procedure TGLBDCEBody.Loaded;
var
   mng : TComponent;
begin
   inherited;
   if FManagerName<>'' then begin
      mng:=FindManager(TGLDCEManager, FManagerName);
      if Assigned(mng) then
         Manager:=TGLDCEManager(mng);
      FManagerName:='';
   end;
end;

// Assign
//
procedure TGLBDCEBody.Assign(Source: TPersistent);
begin
   if Source is TGLBDCEBody then begin
      Manager:=TGLBDCEBody(Source).Manager;
      Shape := TGLBDCEBody(Source).Shape;
      Layer := TGLBDCEBody(Source).Layer;
      IsDynamic := TGLBDCEBody(Source).IsDynamic;
      Solid := TGLBDCEBody(Source).Solid;
      Size.Assign(TGLBDCEBody(Source).Size);
      Friction := TGLBDCEBody(Source).Friction;
   end;
   inherited Assign(Source);
end;

// SetManager
//
procedure TGLBDCEBody.SetManager(const val : TGLDCEManager);
begin
   if val<>FManager then begin
      if Assigned(FManager) then
         FManager.DeRegisterClient(Self);
      if Assigned(val) then
         val.RegisterClient(Self);
   end;
end;

procedure TGLBDCEBody.AddForce(NewForce: TAffineVector);
begin
  AddVector(FForce,NewForce);
end;

procedure TGLBDCEBody.Jump(jHeight, jSpeed: Single);
begin
  if (not FJumping) and (FInGround) then
  begin
    FJumpPos := OwnerBaseSceneObject.AbsolutePosition[1];
    FJumpHeight := OwnerBaseSceneObject.AbsolutePosition[1] + jHeight;
    FJumpForce := jSpeed;
    FJumpSpeed[0] := 0;//jSpeed * FGroundBounce[0];
    FJumpSpeed[1] := jSpeed * FGroundNormal[1];
    FJumpSpeed[2] := 0;//jSpeed * FGroundBounce[2];
    FJumping := True;
  end;
end;

procedure TGLBDCEBody.DoMove(deltaTime: Double);
var Fat,fAvg, fAbs: Single;
    Dir, Up, Left, Distance: TAffineVector;
begin
  //if not FInGround then ScaleVector(FForce, 0.5);

  FSpeed[0] := (FSpeed[0]) + (FForce[0] * deltaTime);
  FSpeed[1] := (FSpeed[1]) + (FForce[1] * deltaTime);
  FSpeed[2] := (FSpeed[2]) + (FForce[2] * deltaTime);

  //Do jump according to the ground normal
  if FJumping then
  begin
    FJumpPos := FJumpPos + ((FJumpForce+FManager.Gravity.Y) * deltaTime);
    FJumping := FJumpPos < FJumpHeight;
  end;

  Dir := OwnerBaseSceneObject.Direction.AsAffineVector;
  Up := OwnerBaseSceneObject.Up.AsAffineVector;
  Left := AffineVectorMake(OwnerBaseSceneObject.LeftVector);
  Distance[0] := (Left[0] * FSpeed[0]) + (Up[0] * FSpeed[1]) + (Dir[0] * FSpeed[2]);
  Distance[1] := (Left[1] * FSpeed[0]) + (Up[1] * FSpeed[1]) + (Dir[1] * FSpeed[2]);
  Distance[2] := (Left[2] * FSpeed[0]) + (Up[2] * FSpeed[1]) + (Dir[2] * FSpeed[2]);
  //Add jump vector
  AddVector(Distance,FJumpSpeed);

  ScaleVector(Distance,deltaTime);

  //Returns the friction average of all collided objects
  //** The average is wrong, but is simpler and returns an aproximated result
  fAvg := FManager.MoveByDistance(Self, Distance, deltaTime);
  //Apply friction
  if fAvg < 0 then fAvg := 0;
  fAbs := (fAvg + FFriction); //Sum of all friction
  Fat := 1 - (deltaTime * fAbs);
  if Fat < 0 then Fat := 0;
  ScaleVector(FSpeed,Fat);
  if not FJumping then ScaleVector(FJumpSpeed,Fat);

  FForce := NullVector;
end;

procedure TGLBDCEBody.DoProgress(const progressTime : TProgressTimes);
begin
  inherited doProgress(progressTime);
  assert(assigned(manager), 'DCE Manager not assigned to behaviour.');
  if FDynamic then
    DoMove(progressTime.deltaTime);
end;

procedure TGLBDCEBody.SetSize(const Value: TGLCoordinates);
begin
  FSize := Value;
end;

procedure TGLBDCEBody.SetDynamic(const Value: Boolean);
begin
  if not ((FShape <> csEllipsoid) and Value) then
    FDynamic := Value;
end;

procedure TGLBDCEBody.SetFriction(const Value: Single);
begin
  if Value >= 0 then FFriction := Value
  else FFriction := 0;
end;

procedure TGLBDCEBody.SetShape(const Value: TDCEShape);
begin
  if not ((Value <> csEllipsoid) and FDynamic) then
    FShape := Value;

end;


// ----------------------------------------------------------------
// ----------------------------------------------------------------
// ----------------------------------------------------------------

// GetOrCreateCollision (TGLBehaviours)
//
function GetOrCreateDCECollision(behaviours : TGLBehaviours) : TGLBDCEBody;
var
	i : Integer;
begin
	i:=behaviours.IndexOfClass(TGLBDCEBody);
	if i>=0 then
		Result:=TGLBDCEBody(behaviours[i])
	else Result:=TGLBDCEBody.Create(behaviours);
end;

// GetOrCreateCollision (TGLBaseSceneObject)
//
function GetOrCreateDCECollision(obj : TGLBaseSceneObject) : TGLBDCEBody;
begin
	Result:=GetOrCreateDCECollision(obj.Behaviours);
end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
	RegisterXCollectionItemClass(TGLBDCEBody);


end.
