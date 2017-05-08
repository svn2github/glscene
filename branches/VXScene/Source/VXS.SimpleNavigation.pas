//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    A simple component written by request from someone at the www.glscene.ru forums. 
    Allows to view the FPS and do the usual Zoom and MoveAroundTarget stuff  
    that all demos usually have in themselves. All that is just by dropping  
    this component on the form. 

}

unit VXS.SimpleNavigation;

interface

{$I VXScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
  FMX.Forms,
  FMX.Controls,
  FMX.ExtCtrls,
  FMX.Types,
  VXS.VectorGeometry,
  VXS.Scene,
  VXS.Win64Viewer,
  VXS.Strings,
  VXS.CrossPlatform;

type

  TPoint = VXS.CrossPlatform.TVXPoint; // for Mouse Wheel

  TVXSimpleNavigationOption = (
    snoInvertMoveAroundX, snoInvertMoveAroundY, // MoveAroundTarget.
    snoInvertZoom, snoInvertMouseWheel, // Zoom.
    snoInvertRotateX, snoInvertRotateY, // RotateTarget.
    snoMouseWheelHandled, // MouseWheel.
    snoShowFPS // Show FPS
    );

  TVXSimpleNavigationOptions = set of TVXSimpleNavigationOption;

  TVXSimpleNavigationAction = (snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom);

  TVXSimpleNavigationKeyCombination = class;
  TSimpleNavigationCustomActionEvent =
    procedure(Sender: TVXSimpleNavigationKeyCombination; Shift: TShiftState; X, Y: Single) of object;

  TVXSimpleNavigationKeyCombination = class(TCollectionItem)
  private
    FExitOnMatch: Boolean;
    FAction: TVXSimpleNavigationAction;
    FOnCustomAction: TSimpleNavigationCustomActionEvent;
    FShiftState: TShiftState;
  protected
    function GetDisplayName: string; override;
    procedure DoOnCustomAction(Shift: TShiftState; X, Y: Single); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShiftState: TShiftState read FShiftState write FShiftState default [];
    property ExitOnMatch: Boolean read FExitOnMatch write FExitOnMatch default True;
    property Action: TVXSimpleNavigationAction read FAction write FAction default snaNone;
    property OnCustomAction: TSimpleNavigationCustomActionEvent read FOnCustomAction write FOnCustomAction;
  end;

  TVXSimpleNavigationKeyCombinations = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TVXSimpleNavigationKeyCombination;
    procedure SetItems(Index: Integer; const Value: TVXSimpleNavigationKeyCombination);
  public
    function Add: TVXSimpleNavigationKeyCombination; overload;
    function Add(const AShiftState: TShiftState; const AAction: TVXSimpleNavigationAction; const AExitOnMatch: Boolean = True): TVXSimpleNavigationKeyCombination; overload;
    property Items[Index: Integer]: TVXSimpleNavigationKeyCombination read GetItems write SetItems; default;
  end;

  TVXSimpleNavigation = class(TComponent)
  private
    FTimer: TTimer;
    FForm: TCustomForm;
    FVXSceneViewer: TVXSceneViewer;

    FOldX, FOldY: Single;
    FFormCaption: string;
    FMoveAroundTargetSpeed: Single;
    FZoomSpeed: Single;
    FOptions: TVXSimpleNavigationOptions;
    FKeyCombinations: TVXSimpleNavigationKeyCombinations;
    FRotateTargetSpeed: Single;
    FOnMouseMove: TMouseMoveEvent;
    procedure ShowFPS(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Single);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TVXPoint; var Handled: Boolean);

    procedure SetVXSceneViewer(const Value: TVXSceneViewer);
    procedure SetForm(const Value: TCustomForm);
    function StoreFormCaption: Boolean;
    function StoreMoveAroundTargetSpeed: Boolean;
    function StoreZoomSpeed: Boolean;
    procedure SetKeyCombinations(const Value: TVXSimpleNavigationKeyCombinations);
    function StoreRotateTargetSpeed: Boolean;
    procedure SetOptions(const Value: TVXSimpleNavigationOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Form: TCustomForm read FForm write SetForm;
    property VXSceneViewer: TVXSceneViewer read FVXSceneViewer write SetVXSceneViewer;

    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed stored StoreZoomSpeed;
    property MoveAroundTargetSpeed: Single read FMoveAroundTargetSpeed write FMoveAroundTargetSpeed stored StoreMoveAroundTargetSpeed;
    property RotateTargetSpeed: Single read FRotateTargetSpeed write FRotateTargetSpeed stored StoreRotateTargetSpeed;

    property FormCaption: string read FFormCaption write FFormCaption stored StoreFormCaption;
    property Options: TVXSimpleNavigationOptions read FOptions write SetOptions default [snoMouseWheelHandled, snoShowFPS];
    property KeyCombinations: TVXSimpleNavigationKeyCombinations read FKeyCombinations write SetKeyCombinations;

    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

implementation

const
  vFPSString = '%FPS';
  EPS = 0.001;

  { TVXSimpleNavigation }

procedure TVXSimpleNavigation.Assign(Source: TPersistent);
begin
  if Source is TVXSimpleNavigation then
  begin
    { Don't do that, because that might overide the original component's event handlers
    SetForm(TVXSimpleNavigation(Source).FForm);
    SetVXSceneViewer(TVXSimpleNavigation(Source).FVXSceneViewer);
    }
    FZoomSpeed := TVXSimpleNavigation(Source).FZoomSpeed;
    FMoveAroundTargetSpeed := TVXSimpleNavigation(Source).FMoveAroundTargetSpeed;
    FRotateTargetSpeed := TVXSimpleNavigation(Source).FRotateTargetSpeed;

    FFormCaption := TVXSimpleNavigation(Source).FFormCaption;
    FOptions := TVXSimpleNavigation(Source).FOptions;
    FKeyCombinations.Assign(TVXSimpleNavigation(Source).FKeyCombinations);
  end
  else
    inherited; // Die!
end;

constructor TVXSimpleNavigation.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FKeyCombinations := TVXSimpleNavigationKeyCombinations.Create(Self, TVXSimpleNavigationKeyCombination);
  FKeyCombinations.Add([ssLeft, ssRight], snaZoom, True);
  FKeyCombinations.Add([ssLeft], snaMoveAroundTarget, True);
  FKeyCombinations.Add([ssRight], snaMoveAroundTarget, True);

  FMoveAroundTargetSpeed := 1;
  FRotateTargetSpeed := 1;
  FZoomSpeed := 1.5;
  FOptions := [snoMouseWheelHandled, snoShowFPS];
  FFormCaption := vFPSString;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := ShowFPS;

  FOnMouseMove := nil;
  //Detect form
  if AOwner is TCustomForm then SetForm(TCustomForm(AOwner));

  //Detect SceneViewer
  if FForm <> nil then
  begin
    if FForm.ComponentCount <> 0 then
      for I := 0 to FForm.ComponentCount - 1 do
        if FForm.Components[I] is TVXSceneViewer then
        begin
          SetVXSceneViewer(TVXSceneViewer(FForm.Components[I]));
          Exit;
        end;
  end;
end;

destructor TVXSimpleNavigation.Destroy;
begin
  FTimer.Free;
  FKeyCombinations.Free;

  if FForm <> nil then
    TForm(FForm).OnMouseWheel := nil;

  if FVXSceneViewer <> nil then
    FVXSceneViewer.OnMouseMove := nil;

  inherited;
end;

procedure TVXSimpleNavigation.ViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TVXPoint;
  var Handled: Boolean);
var
  Sign: SmallInt;
begin
  if (csDesigning in ComponentState) or (WheelDelta = 0) then
    Exit;

  if snoInvertMouseWheel in FOptions then
    Sign := 1
  else
    Sign := -1;

  if FVXSceneViewer <> nil then
    if FVXSceneViewer.Camera <> nil then
      FVXSceneViewer.Camera.AdjustDistanceToTarget(
                      Power(FZoomSpeed, Sign * WheelDelta div Abs(WheelDelta)));

  Handled := snoMouseWheelHandled in FOptions;
end;

procedure TVXSimpleNavigation.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);

  procedure DoZoom;
  var
    Sign: SmallInt;
  begin
    if snoInvertZoom in FOptions then
      Sign := -1
    else
      Sign := 1;
    FVXSceneViewer.Camera.AdjustDistanceToTarget(
                                    Power(FZoomSpeed, Sign * (Y - FOldY) / 20));
  end;

  procedure DoMoveAroundTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertMoveAroundX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertMoveAroundY in FOptions then
      SignY := -1
    else
      SignY := 1;

    FVXSceneViewer.Camera.MoveAroundTarget(SignX * FMoveAroundTargetSpeed * (FOldY - Y),
                                           SignY * FMoveAroundTargetSpeed * (FOldX - X));
  end;

  procedure DoRotateTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertRotateX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertRotateY in FOptions then
      SignY := -1
    else
      SignY := 1;

    FVXSceneViewer.Camera.RotateTarget(SignY * FRotateTargetSpeed * (FOldY - Y),
                                       SignX * FRotateTargetSpeed * (FOldX - X));
  end;

var
  I: Integer;

begin
  if csDesigning in ComponentState then
    exit;

  if FVXSceneViewer <> nil then
    if FVXSceneViewer.Camera <> nil then
    begin
    if FKeyCombinations.Count <> 0 then
      for I := 0 to FKeyCombinations.Count - 1 do
        if FKeyCombinations[I].FShiftState <= Shift then
        begin
          case FKeyCombinations[I].FAction of
            snaNone: ; //Ignore.
            snaMoveAroundTarget: DoMoveAroundTarget;
            snaZoom: DoZoom;
            snaRotateTarget: DoRotateTarget;
            snaCustom: FKeyCombinations[I].DoOnCustomAction(Shift, X, Y);
          else
            Assert(False, strErrorEx + strUnknownType);
          end;

          if FKeyCombinations[I].FExitOnMatch then
            Break;
        end;
  end;

  FOldX := X;
  FOldY := Y;

  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TVXSimpleNavigation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FVXSceneViewer) and (Operation = opRemove) then
    FVXSceneViewer := nil;
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;

procedure TVXSimpleNavigation.SetKeyCombinations(
  const Value: TVXSimpleNavigationKeyCombinations);
begin
  FKeyCombinations.Assign(Value);
end;

procedure TVXSimpleNavigation.SetForm(const Value: TCustomForm);
begin
  if FForm <> nil then
  begin
    FForm.RemoveFreeNotification(Self);
    TForm(FForm).OnMouseWheel := nil;
    TForm(FForm).OnMouseMove := nil;
    if FFormCaption = vFPSString then
      FFormCaption := FForm.Caption + ' - ' + vFPSString;
    FForm.FreeNotification(Self);
  end;
  FForm := Value;

end;

procedure TVXSimpleNavigation.SetVXSceneViewer(
  const Value: TVXSceneViewer);
begin
  if FVXSceneViewer <> nil then
  begin
    FVXSceneViewer.RemoveFreeNotification(Self);
    FVXSceneViewer.OnMouseMove := nil;
  end;

  FVXSceneViewer := Value;

  if FVXSceneViewer <> nil then
  begin
    FVXSceneViewer.OnMouseMove := ViewerMouseMove;
    FVXSceneViewer.FreeNotification(Self);
  end;
end;

procedure TVXSimpleNavigation.ShowFPS(Sender: TObject);
var
  Index: Integer;
  Temp: string;
begin
  if (FVXSceneViewer <> nil) and
     (FForm <> nil) and
     not(csDesigning in ComponentState) and
     (snoShowFPS in FOptions) then
  begin
    Temp := FFormCaption;
    Index := Pos(vFPSString, Temp);
    if Index <> 0 then
    begin
      Delete(Temp, Index, Length(vFPSString));
      Insert(FVXSceneViewer.FramesPerSecondText, Temp, Index);
    end;
    FForm.Caption := Temp;
    FVXSceneViewer.ResetPerformanceMonitor;
  end;
end;

function TVXSimpleNavigation.StoreFormCaption: Boolean;
begin
  Result := (FFormCaption <> vFPSString);
end;

function TVXSimpleNavigation.StoreMoveAroundTargetSpeed: Boolean;
begin
  Result := Abs(FMoveAroundTargetSpeed - 1) > EPS;
end;

function TVXSimpleNavigation.StoreZoomSpeed: Boolean;
begin
  Result := Abs(FZoomSpeed - 1.5) > EPS;
end;

function TVXSimpleNavigation.StoreRotateTargetSpeed: Boolean;
begin
  Result := Abs(FRotateTargetSpeed - 1) > EPS;
end;

procedure TVXSimpleNavigation.SetOptions(
  const Value: TVXSimpleNavigationOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;

  end;
end;

{ TVXSimpleNavigationKeyCombination }

procedure TVXSimpleNavigationKeyCombination.Assign(Source: TPersistent);
begin
  if Source is TVXSimpleNavigationKeyCombination then
  begin
    FExitOnMatch := TVXSimpleNavigationKeyCombination(Source).FExitOnMatch;
    FAction := TVXSimpleNavigationKeyCombination(Source).FAction;
    FOnCustomAction := TVXSimpleNavigationKeyCombination(Source).FOnCustomAction;
    FShiftState := TVXSimpleNavigationKeyCombination(Source).FShiftState;
  end
  else
    inherited; // Die!
end;

constructor TVXSimpleNavigationKeyCombination.Create(Collection: TCollection);
begin
  inherited;
  FAction := snaNone;
  FExitOnMatch := True;
end;

procedure TVXSimpleNavigationKeyCombination.DoOnCustomAction(
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnCustomAction) then
    FOnCustomAction(Self, Shift, X, Y);
end;

function TVXSimpleNavigationKeyCombination.GetDisplayName: string;
begin
  Result := GetSetProp(Self, 'ShiftState', True) + '  -  ' +
    GetEnumName(TypeInfo(TVXSimpleNavigationAction), Integer(FAction));
end;

{ TVXSimpleNavigationKeyCombinations }

function TVXSimpleNavigationKeyCombinations.Add: TVXSimpleNavigationKeyCombination;
begin
  Result := TVXSimpleNavigationKeyCombination(inherited Add);
end;

function TVXSimpleNavigationKeyCombinations.Add(
  const AShiftState: TShiftState; const AAction: TVXSimpleNavigationAction;
  const AExitOnMatch: Boolean): TVXSimpleNavigationKeyCombination;
begin
  Result := Add;
  with Result do
  begin
    FShiftState := AShiftState;
    FAction := AAction;
    FExitOnMatch := AExitOnMatch;
  end;
end;

function TVXSimpleNavigationKeyCombinations.GetItems(
  Index: Integer): TVXSimpleNavigationKeyCombination;
begin
  Result := TVXSimpleNavigationKeyCombination(inherited GetItem(Index));
end;

procedure TVXSimpleNavigationKeyCombinations.SetItems(Index: Integer;
  const Value: TVXSimpleNavigationKeyCombination);
begin
  inherited SetItem(Index, Value);
end;

end.

