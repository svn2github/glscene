//
// This unit is part of the GLScene Project, http://glscene.org
//

{: GLCameraController<p>

  Component for animating camera movement.
  Can be used to zoom in/out, for linear movement, orbiting and Google Earth - like "fly-to"
  Main purpose was the SafeOrbitAndZoomToPos method, the others are usable as well

  <b>History : </b><font size=-1><ul>
      <li>20/05/11 - YanP - GLCameraController refactored as a Job manager, each camera movement is a job in a list
      <li>10/05/11 - Vince - Add OnMove event
      <li>04/05/11 - Vince - Add OrbitToPosAdvanced function to support OrbitToPos when axis are different from -1,0 or 1
      <li>24/07/09 - DaStr - Got rid of compiler hints 
      <li>20/03/09 - DanB - Donated to GLScene by Bogdan Deaky.
    </ul></font>
}

//  ######## NOTE: *MAY* STILL BE WORK IN PROGRESS ON THIS COMPONENT ##########

//GLCameraController v1.1
//Bogdan Deaky / Bluemind Software
//Bluemind Software allows free usage and distribution of this component
//Do let the author know if you do code changes/improvements
//bogdan@bluemind-software.ro
//v1.0 2007
//v1.1 2009 (for GLScene, ships with glscene_icon_TGLCameraController.bmp)


//IMPORTANT!
//You should block user GUI access to the GLSceneViewer
//while movement is being done, check the AllowUserAction property!
//Block user GUI access while AllowUserAction is false to avoid behaviour errors
//simply put
//if GLCameraController1.AllowUserAction then
// //do whatever you want on mouse move, form wheel etc

// methods and properties are explained in the interface section (through comments)
// additional comments might apear in implementation section where needed

unit GLCameraController;

interface

uses GLScene, Classes, SysUtils, Contnrs, GLCadencer, VectorGeometry;

type

  EGLCameraController = class(Exception);

  // Forward declaration of the camera controller main class
  TGLCameraController = class;

  // Forward declaration of a generic camera job
  TGLCameraJob = class;

  TGLCameraJobList = class(TObjectList)
  private
    FController : TGLCameraController;
    function GetCameraJob(AIndex: integer): TGLCameraJob;
    procedure SetCameraJob(AIndex: integer; const Value: TGLCameraJob);
  public
    constructor Create(AController: TGLCameraController);
    function Add(ACameraJob: TGLCameraJob): integer;
    property Items[AIndex: integer]: TGLCameraJob read GetCameraJob write SetCameraJob; default;
    function First: TGLCameraJob;
    function Last: TGLCameraJob;
    function Camera : TGLCamera;
  end;

  TGLCameraJob = class
  private
    FJoblist : TGLCameraJobList;
  protected
    FAbort         : boolean;
    FInit          : boolean;
    FRunning       : Boolean;

    FInitialPos    : TVector;
    FInitialUp     : TVector;
    FInitialDir    : TVector;

    FFinalPos      : TVector;

    FElapsedTime   : Double;
    FDiffTime      : Double;
    FStartTime     : Double;
    FProceedTime   : Double;
  public
    constructor Create(AJoblist : TGLCameraJobList);
    destructor Destroy; override;

    procedure Abort;
    procedure Step; virtual; abstract;
    procedure Init; virtual; abstract;
  end;

  TGLMoveToPosJob = class(TGLCameraJob)
  public
    X : Double;
    Y : Double;
    Z : Double;
    Time : Double;
    procedure Step; override;
    procedure Init; override;
  end;

  TGLZoomToDistanceJob = class(TGLCameraJob)
  public
    Distance : Double;
    Time : Double;
    procedure Step; override;
    procedure Init; override;
  end;

  TGLOrbitToPosJob = class(TGLCameraJob)
  private
    FRotateSpeedX : Double;
    FRotateSpeedZ : Double;
  public
    X : Double;
    Y : Double;
    Z : Double;
    Time : Double;
    procedure Step; override;
    procedure Init; override;
  end;


  TGLOrbitToPosAdvJob = class(TGLCameraJob)
  private
    FRotAxis : TVector;
    FAngle   : Double;
  public
    X : Double;
    Y : Double;
    Z : Double;
    Time : Double;
    PreferUpAxis : Boolean;
    procedure Step; override;
    procedure Init; override;
  end;

  TGLCameraJobEvent = procedure(Sender : TGLCameraJob) of object;

  TGLCameraController = class(TComponent)
  private
    //private variables - explained at the respective properties
    FCamera:TGLCamera;
    FCadencer:TGLCadencer;
    FOnJobAdded: TGLCameraJobEvent;
    FOnJobFinished: TGLCameraJobEvent;
    FOnJobStep: TGLCameraJobEvent;

    //fields used by SafeOrbitAndZoomToPos
    FsoSafeDist,FsoTimeToSafePlacement,FsoTimeToOrbit,FsoTimeToZoomBackIn:double;

    //private methods
    //used to test whether camera and cadencer are assigned
    //Extended = true -> will test also for Camera.TargetObject
    procedure CheckAssignments(Extended:boolean);
    //this adjusts camera depth of view after each movement
    //contains a call to Application.Processmessages for not blocking the app
    procedure AdjustScene;
    //after AdjustScene the Camera.DepthofView will be modified
    //if you want to zoom back in from GUI
    //you should use something like
    //  Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
    procedure SetOnJobAdded(const Value: TGLCameraJobEvent);
    procedure SetOnJobFinished(const Value: TGLCameraJobEvent);
    procedure SetOnJobStep(const Value: TGLCameraJobEvent);
  public
    CameraJobList : TGLCameraJobList;
    //constructor
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    //methods
    //linear movement from current pos
    procedure MoveToPos(x,y,z,time:double);
    //orbiting from current pos to the pos where
    //the camera points at the camera.targetObject TROUGH the given point
    //it will not move to the given point(!), use SafeOrbitAndZoomToPos instead
    //there has to be a camera.targetObject assigned!
    procedure OrbitToPos(x,y,z,time:double);
    //Same function as OrbitToPos but support all camera states
    //PreferUpAxis value is to setup if function use Camera Up based rotation axis
    //instead of Camera direction based rotation axis when destination and camera
    //position are opposite from Camera Target
    procedure OrbitToPosAdvanced(x,y,z,time:double; PreferUpAxis: Boolean = True);
    //zooms in/out by moving to the given distance from camera.targetObject
    //there has to be a camera.targetObject assigned!
    procedure ZoomToDistance(Distance,Time:double);
    //google earth - like "fly-to"
    // = zoom out to safe distance, orbit, and then zoom in to the given point
    //there has to be a camera.targetObject assigned!
    procedure SafeOrbitAndZoomToPos(x,y,z:double);
    //Dan Bartlett said in the GLScene newsgroup that it might be a good idea
    //to introduce ability to stop movement and return control to user
    //here it is
    procedure StopMovement;
    //<alled by the cadencer to animate the camera
    procedure Step(const deltaTime, newTime: Double);
  published
    //properties
    //assign a TGLCamera instance to this
    property Camera:TGLCamera read FCamera write FCamera;
    //assign a TGLCadencer instance to this

    property Cadencer:TGLCadencer read FCadencer write FCadencer;

    //specifies whether user should be able interract with the GLSceneViewer
    //it is set to false while the camera is moving and
    //coders should check this value and block GUI access to GLSceneViewer
    //property AllowUserAction:boolean read FAllowUserAction;

    //safe distance to avoid moving the camera trough the camera.targetObject
    //while performing  SafeOrbitAndZoomToPos
    property soSafeDistance:double read FsoSafeDist write FsoSafeDist;
    //time to zoom in/out to the safe position while performing  SafeOrbitAndZoomToPos
    property soTimeToSafePlacement:double read FsoTimeToSafePlacement write FsoTimeToSafePlacement;
    //time to orbit while performing  SafeOrbitAndZoomToPos
    property soTimeToOrbit:double read FsoTimeToOrbit write FsoTimeToOrbit;
    //time to zoom in/out to the given final position while performing  SafeOrbitAndZoomToPos
    property soTimeToZoomBackIn:double read FsoTimeToZoomBackIn write FsoTimeToZoomBackIn;

    //this event is triggered when a job is init
    property OnJobAdded : TGLCameraJobEvent read FOnJobAdded write SetOnJobAdded;

    //this event is triggered when a job is step (like an OnMove)
    property OnJobStep : TGLCameraJobEvent read FOnJobStep write SetOnJobStep;

    //this event is triggered when a job is finished (not canceled)
    property OnJobFinished : TGLCameraJobEvent read FOnJobFinished write SetOnJobFinished;
  end;

implementation


uses
  {$IFNDEF GLS_DELPHI} VectorTypes{$ENDIF};

const
  cGLCAMERACONTROLLER_CHECK_EXTENDED = TRUE;

{ TGLCameraController }


constructor TGLCameraController.Create(AOwner:TComponent);
begin
  inherited;
  //create the job list container
  CameraJobList := TGLCameraJobList.Create(Self);
  CameraJobList.OwnsObjects := true;

  //initialize values
  soSafeDistance:=10;
  soTimeToSafePlacement:=1;
  soTimeToOrbit:=2;
  soTimeToZoomBackIn:=1;
end;

destructor TGLCameraController.Destroy;
begin
  //delete job list and all jobs inside
  CameraJobList.Free;
  inherited;
end;


procedure TGLCameraController.CheckAssignments(Extended:boolean);
begin
  /// Check camera assignment
  if not Assigned(Camera) then
  begin
    Raise EGLCameraController.CreateFmt('%s (%s) needs to have a Camera assigned',[Self.Name, Self.ClassName]);
  end;
  /// Check cadencer assignament
  if not Assigned(Cadencer) then
  begin
    Raise EGLCameraController.CreateFmt('%s (%s) needs to have a Cadencer assigned',[Self.Name, Self.ClassName]);
  end;
  if Extended then
    /// Check camera;TargetObject assignment
    if not Assigned(Camera.TargetObject) then
    begin
      Raise EGLCameraController.CreateFmt('%s (%s) needs Camera to have a TargetObject assigned',[Self.Name, Self.ClassName]);
    end;
end;

procedure TGLCameraController.AdjustScene;
begin
  Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
  Camera.TransformationChanged;
end;

procedure TGLCameraController.Step(const deltaTime, newTime: Double);
var
  CurrentJob : TGLCameraJob;
begin

  if CameraJobList.Count > 0 then
  begin
    CurrentJob := CameraJobList.First;

    if CurrentJob.FInit then
    begin
      CurrentJob.Init;
      CurrentJob.FStartTime := newTime;
      CurrentJob.FRunning := True;
      CurrentJob.FInit := False;

      // Notify job
      if Assigned(FOnJobAdded) then
        FOnJobAdded(CurrentJob);
    end;

    if CurrentJob.FRunning then
    begin
      CurrentJob.FElapsedTime := newTime - CurrentJob.FStartTime;
      CurrentJob.FDiffTime := deltaTime;// newTime - CurrentJob.FElapsedTime;
      CurrentJob.Step;

      // Notify job
      if Assigned(FOnJobStep) then
        FOnJobStep(CurrentJob);
    end;

    if not CurrentJob.FRunning then
    begin
      CameraJobList.Remove(CurrentJob);

      // Notify job
      if Assigned(FOnJobFinished) then
        FOnJobFinished(CurrentJob);
    end;

  end;

  //AdjustScene;
end;


procedure TGLCameraController.MoveToPos(x,y,z, time:double);
var
  Job : TGLMoveToPosJob;
begin
  Job := TGLMoveToPosJob.Create(CameraJobList);

  Job.X := x;
  Job.Y := y;
  Job.Z := z;
  Job.Time := time;
end;


procedure TGLCameraController.ZoomToDistance(Distance, Time:double);
var
  Job : TGLZoomToDistanceJob;
begin
  Job := TGLZoomToDistanceJob.Create(CameraJobList);
  Job.Distance := Distance;
  Job.Time := Time;
end;


procedure TGLCameraController.OrbitToPos(x,y,z,time:double);
var
  Job : TGLOrbitToPosJob;
begin
  Job := TGLOrbitToPosJob.Create(CameraJobList);

  Job.X := x;
  Job.Y := y;
  Job.Z := z;
  Job.Time := time;
end;

procedure TGLCameraController.OrbitToPosAdvanced(x,y,z,time:double; PreferUpAxis: Boolean = True);
var
  Job : TGLOrbitToPosAdvJob;
begin
  Job := TGLOrbitToPosAdvJob.Create(CameraJobList);

  Job.X := x;
  Job.Y := y;
  Job.Z := z;
  Job.PreferUpAxis := PreferUpAxis;
  Job.Time := time;
end;


procedure TGLCameraController.SafeOrbitAndZoomToPos(x,y,z:double);
begin
  //this was the main purpose of this component
  //as you can see, it actually is a combination of the other 3 methods
  CheckAssignments(cGLCAMERACONTROLLER_CHECK_EXTENDED);
  ZoomToDistance(soSafeDistance,soTimeToSafePlacement);
  OrbitToPos(x,y,z,soTimeToOrbit);
  MoveToPos(x,y,z,soTimeToZoomBackIn);
end;


procedure TGLCameraController.StopMovement;
begin
  CameraJobList.Clear;
end;


procedure TGLCameraController.SetOnJobAdded(const Value: TGLCameraJobEvent);
begin
  FOnJobAdded := Value;
end;

procedure TGLCameraController.SetOnJobStep(const Value: TGLCameraJobEvent);
begin
  FOnJobStep := Value;
end;

procedure TGLCameraController.SetOnJobFinished(const Value: TGLCameraJobEvent);
begin
  FOnJobFinished := Value;
end;


{ TGLCameraJobList }


constructor TGLCameraJobList.Create(AController: TGLCameraController);
begin
  inherited Create;
  FController := AController;
end;

function TGLCameraJobList.GetCameraJob(AIndex: integer): TGLCameraJob;
begin
  Result := inherited Get(AIndex);
end;

procedure TGLCameraJobList.SetCameraJob(AIndex: integer;
  const Value: TGLCameraJob);
begin
  inherited Put(AIndex, Value);
end;

function TGLCameraJobList.Add(ACameraJob: TGLCameraJob): integer;
begin
  Result := inherited Add(ACameraJob);
end;

function TGLCameraJobList.First: TGLCameraJob;
begin
  Result := TGLCameraJob(inherited First);
end;

function TGLCameraJobList.Last: TGLCameraJob;
begin
  Result := TGLCameraJob(inherited Last);
end;

function TGLCameraJobList.Camera: TGLCamera;
begin
  Result := FController.Camera;
end;

{ TGLCameraJob }

constructor TGLCameraJob.Create(AJoblist : TGLCameraJobList);
begin
  FJoblist := AJoblist;
  FJoblist.Add(Self);

  FInit := True;
  FStartTime := 0;
  FProceedTime := 0;
end;

destructor TGLCameraJob.Destroy;
begin

  inherited;
end;

procedure TGLCameraJob.Abort;
begin

end;


{ TGLMoveToPosJob }

procedure TGLMoveToPosJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.Camera.AbsolutePosition, FJoblist.Camera.TargetObject.AbsolutePosition);
  MakeVector(FFinalPos, X, Y, Z);
end;

procedure TGLMoveToPosJob.Step;
var
  Vect : TVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime/FProceedTime);
  end
    else
  begin
    Vect := FFinalPos;
    FRunning := false;
  end;

  if Assigned(FJoblist.Camera.Parent) then
    Vect:=FJoblist.Camera.Parent.AbsoluteToLocal(Vect);

  FJoblist.Camera.Position.AsVector := Vect;
end;

{ TGLZoomToDistanceJob }

procedure TGLZoomToDistanceJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.Camera.AbsolutePosition, FJoblist.Camera.TargetObject.AbsolutePosition);
  // To determine final position, we normalize original position and scale it with final distance
  SetVector(FFinalPos, FInitialPos);
  NormalizeVector(FFinalPos);
  ScaleVector(FFinalPos, Distance);
end;

procedure TGLZoomToDistanceJob.Step;
var
  Vect : TVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime/FProceedTime);
  end
    else
  begin
    Vect := FFinalPos;
    FRunning := false;
  end;

  if Assigned(FJoblist.Camera.Parent) then
    Vect:=FJoblist.Camera.Parent.AbsoluteToLocal(Vect);

  FJoblist.Camera.Position.AsVector := Vect;
end;



{ TGLOrbitToPosJob }

procedure TGLOrbitToPosJob.Init;
var
  pitchangle0,pitchangle1,turnangle0,turnangle1,
  pitchangledif,turnangledif,
  dx0,dy0,dz0,dx1,dy1,dz1:double;
  sign:shortint;
begin

  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.Camera.AbsolutePosition, FJoblist.Camera.TargetObject.AbsolutePosition);
  // Determine final position
  MakeVector(FFinalPos, X, Y, Z);
  FFinalPos := VectorSubtract(FFinalPos, FJoblist.Camera.TargetObject.AbsolutePosition);
  NormalizeVector(FFinalPos);
  ScaleVector(FFinalPos, VectorLength(FInitialPos)); // Scale to radius


  //determine relative positions to determine the lines which form the angles
  //distances from initial camera pos to target object
  with FJoblist.Camera do
  begin
    dx0 := Position.X - TargetObject.Position.x;
    dy0 := Position.Y - TargetObject.Position.Y;
    dz0 := Position.Z - TargetObject.Position.Z;
  end;

  //distances from final camera pos to target object
  with FJoblist.Camera do
  begin
    dx1 := X - TargetObject.Position.x;
    dy1 := Y - TargetObject.Position.Y;
    dz1 := Z - TargetObject.Position.Z;
  end;

  //just to make sure we don't get division by 0 exceptions
  if dx0=0 then dx0:=0.001;
  if dy0=0 then dy0:=0.001;
  if dz0=0 then dz0:=0.001;
  if dx1=0 then dx1:=0.001;
  if dy1=0 then dy1:=0.001;
  if dz1=0 then dz1:=0.001;


  //determine "pitch" and "turn" angles for the initial and  final camera position
  //the formulas differ depending on the camera.Up vector
  //I tested all quadrants for all possible integer FJoblist.Camera.Up directions
  if abs(FJoblist.Camera.Up.AsAffineVector[2])=1 then  //Z=1/-1
  begin
    sign:= round(FJoblist.Camera.Up.AsAffineVector[2]/abs(FJoblist.Camera.Up.AsAffineVector[2]));
    pitchangle0:=arctan(dz0/sqrt(sqr(dx0)+sqr(dy0)));
    pitchangle1:=arctan(dz1/sqrt(sqr(dx1)+sqr(dy1)));
    turnangle0:=arctan(dy0/dx0);
    if (dx0<0) and (dy0<0) then turnangle0:=-(pi-turnangle0)
    else  if (dx0<0) and (dy0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=arctan(dy1/dx1);
    if (dx1<0) and (dy1<0) then turnangle1:=-(pi-turnangle1)
    else  if (dx1<0) and (dy1>0) then turnangle1:=-(pi-turnangle1);
  end
  else if abs(FJoblist.Camera.Up.AsAffineVector[1])=1 then  //Y=1/-1
  begin
    sign:= round(FJoblist.Camera.Up.AsAffineVector[1]/abs(FJoblist.Camera.Up.AsAffineVector[1]));
    pitchangle0:=arctan(dy0/sqrt(sqr(dx0)+sqr(dz0)));
    pitchangle1:=arctan(dy1/sqrt(sqr(dx1)+sqr(dz1)));
    turnangle0:=-arctan(dz0/dx0);
    if (dx0<0) and (dz0<0) then turnangle0:=-(pi-turnangle0)
    else  if (dx0<0) and (dz0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=-arctan(dz1/dx1);
    if (dx1<0) and (dz1<0) then turnangle1:=-(pi-turnangle1)
    else  if (dx1<0) and (dz1>0) then turnangle1:=-(pi-turnangle1);
  end
  else if abs(FJoblist.Camera.Up.AsAffineVector[0])=1 then //X=1/-1
  begin
    sign:= round(FJoblist.Camera.Up.AsAffineVector[0]/abs(FJoblist.Camera.Up.AsAffineVector[0]));
    pitchangle0:=arctan(dx0/sqrt(sqr(dz0)+sqr(dy0)));
    pitchangle1:=arctan(dx1/sqrt(sqr(dz1)+sqr(dy1)));
    turnangle0:=arctan(dz0/dy0);
    if (dz0>0) and (dy0>0) then turnangle0:=-(pi-turnangle0)
    else  if (dz0<0) and (dy0>0) then turnangle0:=-(pi-turnangle0);
    turnangle1:=arctan(dz1/dy1);
    if (dz1>0) and (dy1>0) then turnangle1:=-(pi-turnangle1)
    else  if (dz1<0) and (dy1>0) then turnangle1:=-(pi-turnangle1);
  end
  else
  begin
    Raise EGLCameraController.Create('The Camera.Up vector may contain only -1, 0 or 1');
  end;

  //determine pitch and turn angle differences
  pitchangledif:=sign*(pitchangle1-pitchangle0);
  turnangledif:=sign*(turnangle1-turnangle0);

  if abs(turnangledif)>pi then
    turnangledif:=-abs(turnangledif)/turnangledif*(2*pi-abs(turnangledif));

  // Determine rotation speeds
  FRotateSpeedX := -pitchangledif/time;
  FRotateSpeedZ := turnangledif/time;

end;

procedure TGLOrbitToPosJob.Step;
var
  Vect : TVector;
begin

  if FElapsedTime < FProceedTime then
  begin
    FJoblist.Camera.MoveAroundTarget(radtodeg(FRotateSpeedX)*FDiffTime, radtodeg(FRotateSpeedZ)*FDiffTime);
  end
    else
  begin
    Vect := FFinalPos;

    if Assigned(FJoblist.Camera.Parent) then
      Vect:=FJoblist.Camera.Parent.AbsoluteToLocal(Vect);

    FJoblist.Camera.Position.AsVector := Vect;
    FRunning := false;
  end;

end;


{ TGLOrbitToPosAdvJob }

procedure TGLOrbitToPosAdvJob.Init;
var
  Right: TVector;
begin

  FProceedTime := time;
  FInitialPos:= VectorSubtract(FJoblist.Camera.AbsolutePosition, FJoblist.Camera.TargetObject.AbsolutePosition);

  if Assigned(FJoblist.Camera.Parent) then
    FFinalPos := VectorSubtract(FJoblist.Camera.Parent.LocalToAbsolute(VectorMake(x,y,z,1)), FJoblist.Camera.TargetObject.AbsolutePosition)
  else
    FFinalPos := VectorSubtract(VectorMake(x,y,z,1), FJoblist.Camera.TargetObject.AbsolutePosition);

  //if destination is Target Pos, we can't compute
  if VectorLength(FFinalPos)<0.001 then
  begin
    //FAllowUserAction := True;
    Exit;
  end;

  //Compute Angle of Rotation
  FAngle:= ArcCos(VectorAngleCosine(Vector3fMake(FFinalPos), Vector3fMake(FInitialPos)));

  Right := VectorNormalize(VectorCrossProduct(FJoblist.Camera.AbsoluteVectorToTarget, FJoblist.Camera.AbsoluteUp));

  FInitialDir := FJoblist.Camera.AbsoluteDirection;
  FInitialUp := FJoblist.Camera.AbsoluteUp;

  // Determine rotation Axis
  // if Angle equals 0°
  if FAngle < 0.001 then
    if PreferUpAxis then
      FRotAxis := VectorNormalize(VectorCrossProduct(
                   VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
    else
      FRotAxis := Right
  else
    // if Angle equals 180°
    if FAngle >Pi - 0.001  then
      if PreferUpAxis then
        FRotAxis := VectorNormalize(VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
      else
        FRotAxis := Right
    else
      FRotAxis:= VectorNormalize(VectorCrossProduct(FFinalPos, FInitialPos));

end;

procedure TGLOrbitToPosAdvJob.Step;
var
  tempUp, tempDir, tempPos : TVector;
begin

  if FElapsedTime < FProceedTime then
  begin
    //Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJoblist.Camera.AbsolutePosition:= VectorAdd(FJoblist.Camera.TargetObject.AbsolutePosition, tempPos);
    //Compute Up Vector
    tempUp := FInitialUp;
    RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJoblist.Camera.AbsoluteUp := tempUp;
    //Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJoblist.Camera.AbsoluteDirection := tempDir;
  end
    else
  begin

    FRunning := false;
  end;

end;



end.
