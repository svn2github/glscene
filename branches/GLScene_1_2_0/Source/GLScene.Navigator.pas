//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLNavigator<p>

  Unit for navigating TGLBaseObjects.<p>

  <b>History : </b><font size=-1><ul>
  <li>20/02/07 - DaStr - Moved Notification(), SetObject(), SetUseVirtualUp(),
  SetVirtualUp(), CalcRight() to the "protected" section
  Private "point1" renamed to FPrevPoint
  Updated comments
  TGLNavigator.SetObject made virtual
  <li>19/02/07 - DaStr - TGLNavigator.Create - FVirtualUp creation fixed
  Added default values to TGLNavigator and TGLUserInterface
  <li>29/01/07 - DaStr - Moved registration to GLSceneRegister.pas
  <li>08/03/06 - ur - Fixed warnigs for Delphi 2006
  <li>31/10/05 - Mathx - Fixed bug 1340637 relating to freeNotifications on
  the TGLUserInterface component.
  <li>18/12/04 - PhP - Added FlyForward
  <li>03/07/04 - LR - Added GLShowCursor, GLSetCursorPos, GLGetCursorPos,
  GLGetScreenWidth, GLGetScreenHeight for Linux compatibility
  <li>11/05/04 - JAJ - Added some features and fixed a bug.
  <li>01/06/03 - JAJ - Added notification to movingobject...
  <li>01/06/03 - fig - CurrentHangle implementet...
  <li>14/07/02 - EG - InvertMouse (Joen A. Joensen)
  <li>18/03/02 - EG - Added MouseLookActive property, Fixed framerate dependency
  <li>15/03/02 - JAJ - Structure Change - Mouselook moved to newly created TGLUserInterface.
  <li>15/03/02 - RMCH - Added Mouselook capability.
  <li>09/11/00 - JAJ - First submitted. Base Class TGLNavigator included.
  </ul></font>
}
unit GLScene.Navigator;

interface

{$I GLScene.inc}

uses
  // VCL
  SysUtils,
  Classes,

  // GLScene.Core
  GLScene.Base.Vector.Geometry,
  GLScene.Core,
  GLScene.Platform,
  GLScene.Base.Coordinates,
  GLScene.Screen
{$IFDEF GLS_DELPHI},
  GLScene.Base.Vector.Types{$ENDIF};

type

  // TGLNavigator
  //
  { : TGLNavigator is the component for moving a TGLBaseSceneObject, and all Classes based on it,
    this includes all the objects from the Scene Editor.<p>

    The four calls to get you started is
    <ul>
    <li>TurnHorisontal : it turns left and right.
    <li>TurnVertical : it turns up and down.
    <li>MoveForward :	moves back and forth.
    <li>FlyForward : moves back and forth in the movingobject's direction
    </ul>
    The three properties to get you started is
    <ul>
    <li>MovingObject : The Object that you are moving.
    <li>UseVirtualUp : When UseVirtualUp is set you navigate Quake style. If it isn't
    it's more like Descent.
    <li>AngleLock : Allows you to block the Vertical angles. Should only be used in
    conjunction with UseVirtualUp.
    <li>MoveUpWhenMovingForward : Changes movement from Quake to Arcade Airplane...
    (no tilt and flying)
    <li>InvertHorizontalSteeringWhenUpsideDown : When using virtual up, and vertically
    rotating beyond 90 degrees, will make steering seem inverted, so we "invert" back
    to normal.
    </ul>
  }
  TGLNavigator = class(TComponent)
  private
    FObject: TGLBaseSceneObject;
    FVirtualRight: TVector;
    FVirtualUp: TGLCoordinates;
    FUseVirtualUp: boolean;
    FAutoUpdateObject: boolean;
    FMaxAngle: single;
    FMinAngle: single;
    FCurrentVAngle: single;
    FCurrentHAngle: single;
    FAngleLock: boolean;
    FMoveUpWhenMovingForward: boolean;
    FInvertHorizontalSteeringWhenUpsideDown: boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetObject(NewObject: TGLBaseSceneObject); virtual;
    procedure SetUseVirtualUp(UseIt: boolean);
    procedure SetVirtualUp(Up: TGLCoordinates);
    function CalcRight: TVector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure TurnHorizontal(Angle: single);
    procedure TurnVertical(Angle: single);
    procedure MoveForward(Distance: single);
    procedure StrafeHorizontal(Distance: single);
    procedure StrafeVertical(Distance: single);
    procedure Straighten;
    procedure FlyForward(Distance: single);

    procedure LoadState(Stream: TStream);
    procedure SaveState(Stream: TStream);

    property CurrentVAngle: single read FCurrentVAngle;
    property CurrentHAngle: single read FCurrentHAngle;
  published
    property MoveUpWhenMovingForward: boolean read FMoveUpWhenMovingForward
      write FMoveUpWhenMovingForward default False;
    property InvertHorizontalSteeringWhenUpsideDown: boolean
      read FInvertHorizontalSteeringWhenUpsideDown
      write FInvertHorizontalSteeringWhenUpsideDown default False;
    property VirtualUp: TGLCoordinates read FVirtualUp write SetVirtualUp;
    property MovingObject: TGLBaseSceneObject read FObject write SetObject;
    property UseVirtualUp: boolean read FUseVirtualUp write SetUseVirtualUp
      default False;
    property AutoUpdateObject: boolean read FAutoUpdateObject
      write FAutoUpdateObject default False;
    property MaxAngle: single read FMaxAngle write FMaxAngle;
    property MinAngle: single read FMinAngle write FMinAngle;
    property AngleLock: boolean read FAngleLock write FAngleLock default False;
  end;

  // TGLUserInterface
  //
  { : TGLUserInterface is the component which reads the userinput and transform it into action.<p>

    The four calls to get you started is
    <ul>
    <li>MouseLookActivate : set us up the bomb.
    <li>MouseLookDeActivate : defuses it.
    <li>Mouselook(deltaTime: double) : handles mouse look... Should be called in the Cadencer event. (Though it works every where!)
    <li>MouseUpdate : Resets mouse position so that you don't notice that the mouse is limited to the screen should be called after Mouselook.
    </ul>
    The four properties to get you started are:
    <ul>
    <li>InvertMouse     : Inverts the mouse Y axis.
    <li>MouseSpeed      : Also known as mouse sensitivity.
    <li>GLNavigator     : The Navigator which receives the user movement.
    <li>GLVertNavigator : The Navigator which if set receives the vertical user movement. Used mostly for cameras....
    </ul>
  }

  TGLUserInterface = class(TComponent)
  private
    FPrevPoint: TGLPoint;
    midScreenX, midScreenY: integer;
    FMouseActive: boolean;
    FMouseSpeed: single;
    FGLNavigator: TGLNavigator;
    FGLVertNavigator: TGLNavigator;
    FInvertMouse: boolean;
    procedure MouseInitialize;
    procedure SetMouseLookActive(const val: boolean);
    procedure setNavigator(val: TGLNavigator);
    procedure setVertNavigator(val: TGLNavigator);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUpdate;
    function MouseLook: boolean;
    procedure MouseLookActiveToggle;
    procedure MouseLookActivate;
    procedure MouseLookDeactivate;
    function IsMouseLookOn: boolean;
    procedure TurnHorizontal(Angle: Double);
    procedure TurnVertical(Angle: Double);
    property MouseLookActive: boolean read FMouseActive
      write SetMouseLookActive;
  published
    property InvertMouse: boolean read FInvertMouse write FInvertMouse
      default False;
    property MouseSpeed: single read FMouseSpeed write FMouseSpeed;
    property GLNavigator: TGLNavigator read FGLNavigator write setNavigator;
    property GLVertNavigator: TGLNavigator read FGLVertNavigator
      write setVertNavigator;
  end;

implementation

Constructor TGLNavigator.Create(AOwner: TComponent);
Begin
  inherited;
  FVirtualUp := TGLCoordinates.CreateInitialized(Self, ZHmgVector, csPoint);
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;
End;

Destructor TGLNavigator.Destroy;

Begin
  FVirtualUp.Free;
  inherited;
End;

Procedure TGLNavigator.SetObject(NewObject: TGLBaseSceneObject);
Begin
  If FObject <> NewObject then
  Begin
    If Assigned(FObject) then
      FObject.RemoveFreeNotification(Self);

    FObject := NewObject;
    If Assigned(FObject) then
    Begin
      if csdesigning in componentstate then
      Begin
        If VectorLength(FVirtualUp.AsVector) = 0 then
        Begin
          FVirtualUp.AsVector := FObject.Up.AsVector;
        End;
        Exit;
      End;

      If FUseVirtualUp Then
        FVirtualRight := CalcRight;

      FObject.FreeNotification(Self);
    End;
  End;
End;

procedure TGLNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);

Begin
  If Operation = opRemove then
    If AComponent = FObject then
      MovingObject := Nil;

  inherited;
End;

Function TGLNavigator.CalcRight: TVector;

Begin
  If Assigned(FObject) then
    If FUseVirtualUp Then
    Begin
      VectorCrossProduct(FObject.Direction.AsVector,
        FVirtualUp.AsVector, Result);
      ScaleVector(Result, 1 / VectorLength(Result));
    End
    else
      VectorCrossProduct(FObject.Direction.AsVector, FObject.Up.AsVector,
        Result); { automaticly length(1), if not this is a bug }
End;

Procedure TGLNavigator.TurnHorizontal(Angle: single);

Var
  T: TVector;
  U: TAffineVector;
  TempVal: single;

Begin
  If InvertHorizontalSteeringWhenUpsideDown and
    ((CurrentVAngle < -90) or (CurrentVAngle > 90)) then
    Angle := -Angle;

  FCurrentHAngle := (FCurrentHAngle - Angle);

  If (FCurrentHAngle < 0) or (FCurrentHAngle > 360) then
  Begin
    TempVal := (FCurrentHAngle) / 360;
    FCurrentHAngle := (TempVal - Floor(TempVal)) * 360;
  End;

  Angle := DegToRad(Angle); { make it ready for Cos and Sin }
  If FUseVirtualUp Then
  Begin
    SetVector(U, VirtualUp.AsVector);
    T := FObject.Up.AsVector;
    RotateVector(T, U, Angle);
    FObject.Up.AsVector := T;

    T := FObject.Direction.AsVector;
    RotateVector(T, U, Angle);
    FObject.Direction.AsVector := T;
  End
  else
    FObject.Direction.AsVector := VectorCombine(FObject.Direction.AsVector,
      CalcRight, Cos(Angle), Sin(Angle));
End;

Procedure TGLNavigator.TurnVertical(Angle: single);

Var
  ExpectedAngle: single;
  CosAngle, SinAngle: single;
  TempVal: single;
  Direction: TVector;

Begin
  ExpectedAngle := FCurrentVAngle + Angle;
  If FAngleLock then
  Begin
    If ExpectedAngle > FMaxAngle then
    Begin
      If FCurrentVAngle = FMaxAngle then
        Exit;
      Angle := FMaxAngle - FCurrentVAngle;
      ExpectedAngle := FMaxAngle;
    End
    else
    Begin
      If ExpectedAngle < FMinAngle then
      Begin
        If FCurrentVAngle = FMinAngle then
          Exit;
        Angle := FMinAngle - FCurrentVAngle;
        ExpectedAngle := FMinAngle;
      End;
    End;
  End;
  FCurrentVAngle := ExpectedAngle;

  If (FCurrentVAngle < -180) or (FCurrentVAngle > 180) then
  Begin
    TempVal := (FCurrentVAngle + 180) / 360;
    FCurrentVAngle := (TempVal - Floor(TempVal)) * 360 - 180;
  End;

  Angle := DegToRad(Angle); { make it ready for Cos and Sin }
  SinCos(Angle, SinAngle, CosAngle);
  Direction := VectorCombine(MovingObject.Direction.AsVector,
    MovingObject.Up.AsVector, CosAngle, SinAngle);
  MovingObject.Up.AsVector := VectorCombine(MovingObject.Direction.AsVector,
    MovingObject.Up.AsVector, SinAngle, CosAngle);
  MovingObject.Direction.AsVector := Direction;
End;

Procedure TGLNavigator.MoveForward(Distance: single);
Begin
  If (FUseVirtualUp and (not MoveUpWhenMovingForward)) Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      VectorCrossProduct(FVirtualUp.AsVector, CalcRight), 1, Distance);
  End
  else
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FObject.Direction.AsVector, 1, Distance);
End;

Procedure TGLNavigator.StrafeHorizontal(Distance: single);
Begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
    CalcRight, 1, Distance);
End;

Procedure TGLNavigator.StrafeVertical(Distance: single);
Begin
  If UseVirtualUp Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FVirtualUp.AsVector, 1, Distance);
  End
  else
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FObject.Up.AsVector, 1, Distance);
End;

procedure TGLNavigator.FlyForward(Distance: single);
begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
    FObject.Direction.AsVector, 1, Distance);
end;

Procedure TGLNavigator.Straighten;

Var
  R: TVector;
  D: TVector;
  A: single;

Begin
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;

  R := CalcRight;
  A := VectorAngleCosine(AffineVectorMake(MovingObject.Up.AsVector),
    AffineVectorMake(VirtualUp.AsVector));
  MovingObject.Up.AsVector := VirtualUp.AsVector;

  VectorCrossProduct(R, FVirtualUp.AsVector, D);

  If A >= 0 then
    ScaleVector(D, -1 / VectorLength(D))
  else
    ScaleVector(D, 1 / VectorLength(D));

  MovingObject.Direction.AsVector := D;
End;

Procedure TGLNavigator.SetUseVirtualUp(UseIt: boolean);

Begin
  FUseVirtualUp := UseIt;
  if csdesigning in componentstate then
    Exit;
  If FUseVirtualUp then
    FVirtualRight := CalcRight;
End;

Procedure TGLNavigator.SetVirtualUp(Up: TGLCoordinates);
Begin
  FVirtualUp.Assign(Up);
  if csdesigning in componentstate then
    Exit;
  If FUseVirtualUp then
    FVirtualRight := CalcRight;
End;

Procedure TGLNavigator.LoadState(Stream: TStream);

Var
  Vector: TAffineVector;
  B: ByteBool;
  S: single;

Begin
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Position.AsAffineVector := Vector;
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Direction.AsAffineVector := Vector;
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Up.AsAffineVector := Vector;
  Stream.Read(B, SizeOf(ByteBool));
  UseVirtualUp := B;
  Stream.Read(B, SizeOf(ByteBool));
  FAngleLock := B;
  Stream.Read(S, SizeOf(single));
  FMaxAngle := S;
  Stream.Read(S, SizeOf(single));
  FMinAngle := S;
  Stream.Read(S, SizeOf(single));
  FCurrentVAngle := S;
  Stream.Read(S, SizeOf(single));
  FCurrentHAngle := S;
End;

Procedure TGLNavigator.SaveState(Stream: TStream);

Var
  Vector: TAffineVector;
  B: ByteBool;
  S: single;

Begin
  Vector := FObject.Position.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  Vector := FObject.Direction.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  Vector := FObject.Up.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  B := UseVirtualUp;
  Stream.Write(B, SizeOf(ByteBool));
  B := FAngleLock;
  Stream.Write(B, SizeOf(ByteBool));
  S := FMaxAngle;
  Stream.Write(S, SizeOf(single));
  S := FMinAngle;
  Stream.Write(S, SizeOf(single));
  S := FCurrentVAngle;
  Stream.Write(S, SizeOf(single));
  S := FCurrentHAngle;
  Stream.Write(S, SizeOf(single));
End;

function TGLUserInterface.IsMouseLookOn: boolean;
begin
  Result := FMouseActive;
end;

Procedure TGLUserInterface.TurnHorizontal(Angle: Double);

Begin
  GLNavigator.TurnHorizontal(Angle);
End;

Procedure TGLUserInterface.TurnVertical(Angle: Double);

Begin
  If Assigned(GLVertNavigator) then
    GLVertNavigator.TurnVertical(Angle)
  else
    GLNavigator.TurnVertical(Angle);
End;

procedure TGLUserInterface.MouseLookActiveToggle;
begin
  if FMouseActive then
    MouseLookDeactivate
  else
    MouseLookActivate;
end;

procedure TGLUserInterface.MouseLookActivate;
begin
  if not FMouseActive then
  begin
    FMouseActive := True;
    MouseInitialize;
    GLShowCursor(False);
  end;
end;

procedure TGLUserInterface.MouseLookDeactivate;
begin
  if FMouseActive then
  begin
    FMouseActive := False;
    GLShowCursor(True);
  end;
end;

procedure TGLUserInterface.MouseInitialize;
begin
  midScreenX := GLGetScreenWidth div 2;
  midScreenY := GLGetScreenHeight div 2;

  FPrevPoint.x := midScreenX;
  FPrevPoint.Y := midScreenY;
  GLSetCursorPos(midScreenX, midScreenY);
end;

// SetMouseLookActive
//
procedure TGLUserInterface.SetMouseLookActive(const val: boolean);
begin
  if val <> FMouseActive then
    if val then
      MouseLookActivate
    else
      MouseLookDeactivate;
end;

procedure TGLUserInterface.MouseUpdate;
begin
  if FMouseActive then
    GLGetCursorPos(FPrevPoint);
end;

// Mouselook
//
function TGLUserInterface.MouseLook: boolean;
var
  deltaX, deltaY: single;
begin
  Result := False;
  if not FMouseActive then
    Exit;

  deltaX := (FPrevPoint.x - midScreenX) * MouseSpeed;
  deltaY := -(FPrevPoint.Y - midScreenY) * MouseSpeed;
  If InvertMouse then
    deltaY := -deltaY;

  if deltaX <> 0 then
  begin
    TurnHorizontal(deltaX * 0.01);
    Result := True;
  end;
  if deltaY <> 0 then
  begin
    TurnVertical(deltaY * 0.01);
    Result := True;
  end;

  if (FPrevPoint.x <> midScreenX) or (FPrevPoint.Y <> midScreenY) then
    GLSetCursorPos(midScreenX, midScreenY);
end;

Constructor TGLUserInterface.Create(AOwner: TComponent);
Begin
  inherited;
  FMouseSpeed := 0;
  FMouseActive := False;
  midScreenX := GLGetScreenWidth div 2;
  midScreenY := GLGetScreenHeight div 2;
  FPrevPoint.x := midScreenX;
  FPrevPoint.Y := midScreenY;
End;

Destructor TGLUserInterface.Destroy;

Begin
  if FMouseActive then
    MouseLookDeactivate; // added by JAJ
  inherited;
End;

procedure TGLUserInterface.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FGLNavigator then
      setNavigator(nil);
    if AComponent = FGLVertNavigator then
      setVertNavigator(nil);
  end;
  inherited;
end;

procedure TGLUserInterface.setNavigator(val: TGLNavigator);
begin
  if Assigned(FGLNavigator) then
    FGLNavigator.RemoveFreeNotification(Self);
  FGLNavigator := val;
  if Assigned(val) then
    val.FreeNotification(Self);
end;

procedure TGLUserInterface.setVertNavigator(val: TGLNavigator);
begin
  if Assigned(FGLVertNavigator) then
    FGLVertNavigator.RemoveFreeNotification(Self);
  FGLVertNavigator := val;
  if Assigned(val) then
    val.FreeNotification(Self);
end;

end.
