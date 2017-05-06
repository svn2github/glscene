//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
    A simple component written by request from someone at the www.glscene.ru forums. 
    Allows to view the FPS and do the usual Zoom and MoveAroundTarget stuff  
    that all demos usually have in themselves. All that is just by dropping  
    this component on the form. 

}

unit VKS.SimpleNavigation;

interface

{$I VKScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
  FMX.Forms,
  FMX.Controls,
  FMX.ExtCtrls,
  FMX.Types,
  VKS.VectorGeometry,
  VKS.Scene,
  VKS.Win64Viewer,
  VKS.Strings,
  VKS.CrossPlatform;

type

  TPoint = VKS.CrossPlatform.TVKPoint; // for Mouse Wheel

  TVKSimpleNavigationOption = (
    snoInvertMoveAroundX, snoInvertMoveAroundY, // MoveAroundTarget.
    snoInvertZoom, snoInvertMouseWheel, // Zoom.
    snoInvertRotateX, snoInvertRotateY, // RotateTarget.
    snoMouseWheelHandled, // MouseWheel.
    snoShowFPS // Show FPS
    );

  TVKSimpleNavigationOptions = set of TVKSimpleNavigationOption;

  TVKSimpleNavigationAction = (snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom);

  TVKSimpleNavigationKeyCombination = class;
  TSimpleNavigationCustomActionEvent =
    procedure(Sender: TVKSimpleNavigationKeyCombination; Shift: TShiftState; X, Y: Single) of object;

  TVKSimpleNavigationKeyCombination = class(TCollectionItem)
  private
    FExitOnMatch: Boolean;
    FAction: TVKSimpleNavigationAction;
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
    property Action: TVKSimpleNavigationAction read FAction write FAction default snaNone;
    property OnCustomAction: TSimpleNavigationCustomActionEvent read FOnCustomAction write FOnCustomAction;
  end;

  TVKSimpleNavigationKeyCombinations = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TVKSimpleNavigationKeyCombination;
    procedure SetItems(Index: Integer; const Value: TVKSimpleNavigationKeyCombination);
  public
    function Add: TVKSimpleNavigationKeyCombination; overload;
    function Add(const AShiftState: TShiftState; const AAction: TVKSimpleNavigationAction; const AExitOnMatch: Boolean = True): TVKSimpleNavigationKeyCombination; overload;
    property Items[Index: Integer]: TVKSimpleNavigationKeyCombination read GetItems write SetItems; default;
  end;

  TVKSimpleNavigation = class(TComponent)
  private
    FTimer: TTimer;
    FForm: TCustomForm;
    FVKSceneViewer: TVKSceneViewer;

    FOldX, FOldY: Single;
    FFormCaption: string;
    FMoveAroundTargetSpeed: Single;
    FZoomSpeed: Single;
    FOptions: TVKSimpleNavigationOptions;
    FKeyCombinations: TVKSimpleNavigationKeyCombinations;
    FRotateTargetSpeed: Single;
    FOnMouseMove: TMouseMoveEvent;
    procedure ShowFPS(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Single);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TVKPoint; var Handled: Boolean);

    procedure SetVKSceneViewer(const Value: TVKSceneViewer);
    procedure SetForm(const Value: TCustomForm);
    function StoreFormCaption: Boolean;
    function StoreMoveAroundTargetSpeed: Boolean;
    function StoreZoomSpeed: Boolean;
    procedure SetKeyCombinations(const Value: TVKSimpleNavigationKeyCombinations);
    function StoreRotateTargetSpeed: Boolean;
    procedure SetOptions(const Value: TVKSimpleNavigationOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Form: TCustomForm read FForm write SetForm;
    property VKSceneViewer: TVKSceneViewer read FVKSceneViewer write SetVKSceneViewer;

    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed stored StoreZoomSpeed;
    property MoveAroundTargetSpeed: Single read FMoveAroundTargetSpeed write FMoveAroundTargetSpeed stored StoreMoveAroundTargetSpeed;
    property RotateTargetSpeed: Single read FRotateTargetSpeed write FRotateTargetSpeed stored StoreRotateTargetSpeed;

    property FormCaption: string read FFormCaption write FFormCaption stored StoreFormCaption;
    property Options: TVKSimpleNavigationOptions read FOptions write SetOptions default [snoMouseWheelHandled, snoShowFPS];
    property KeyCombinations: TVKSimpleNavigationKeyCombinations read FKeyCombinations write SetKeyCombinations;

    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

implementation

const
  vFPSString = '%FPS';
  EPS = 0.001;

  { TVKSimpleNavigation }

procedure TVKSimpleNavigation.Assign(Source: TPersistent);
begin
  if Source is TVKSimpleNavigation then
  begin
    { Don't do that, because that might overide the original component's event handlers
    SetForm(TVKSimpleNavigation(Source).FForm);
    SetVKSceneViewer(TVKSimpleNavigation(Source).FVKSceneViewer);
    }
    FZoomSpeed := TVKSimpleNavigation(Source).FZoomSpeed;
    FMoveAroundTargetSpeed := TVKSimpleNavigation(Source).FMoveAroundTargetSpeed;
    FRotateTargetSpeed := TVKSimpleNavigation(Source).FRotateTargetSpeed;

    FFormCaption := TVKSimpleNavigation(Source).FFormCaption;
    FOptions := TVKSimpleNavigation(Source).FOptions;
    FKeyCombinations.Assign(TVKSimpleNavigation(Source).FKeyCombinations);
  end
  else
    inherited; // Die!
end;

constructor TVKSimpleNavigation.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FKeyCombinations := TVKSimpleNavigationKeyCombinations.Create(Self, TVKSimpleNavigationKeyCombination);
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
        if FForm.Components[I] is TVKSceneViewer then
        begin
          SetVKSceneViewer(TVKSceneViewer(FForm.Components[I]));
          Exit;
        end;
  end;
end;

destructor TVKSimpleNavigation.Destroy;
begin
  FTimer.Free;
  FKeyCombinations.Free;

  if FForm <> nil then
    TForm(FForm).OnMouseWheel := nil;

  if FVKSceneViewer <> nil then
    FVKSceneViewer.OnMouseMove := nil;

  inherited;
end;

procedure TVKSimpleNavigation.ViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TVKPoint;
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

  if FVKSceneViewer <> nil then
    if FVKSceneViewer.Camera <> nil then
      FVKSceneViewer.Camera.AdjustDistanceToTarget(
                      Power(FZoomSpeed, Sign * WheelDelta div Abs(WheelDelta)));

  Handled := snoMouseWheelHandled in FOptions;
end;

procedure TVKSimpleNavigation.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);

  procedure DoZoom;
  var
    Sign: SmallInt;
  begin
    if snoInvertZoom in FOptions then
      Sign := -1
    else
      Sign := 1;
    FVKSceneViewer.Camera.AdjustDistanceToTarget(
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

    FVKSceneViewer.Camera.MoveAroundTarget(SignX * FMoveAroundTargetSpeed * (FOldY - Y),
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

    FVKSceneViewer.Camera.RotateTarget(SignY * FRotateTargetSpeed * (FOldY - Y),
                                       SignX * FRotateTargetSpeed * (FOldX - X));
  end;

var
  I: Integer;

begin
  if csDesigning in ComponentState then
    exit;

  if FVKSceneViewer <> nil then
    if FVKSceneViewer.Camera <> nil then
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

procedure TVKSimpleNavigation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FVKSceneViewer) and (Operation = opRemove) then
    FVKSceneViewer := nil;
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;

procedure TVKSimpleNavigation.SetKeyCombinations(
  const Value: TVKSimpleNavigationKeyCombinations);
begin
  FKeyCombinations.Assign(Value);
end;

procedure TVKSimpleNavigation.SetForm(const Value: TCustomForm);
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

procedure TVKSimpleNavigation.SetVKSceneViewer(
  const Value: TVKSceneViewer);
begin
  if FVKSceneViewer <> nil then
  begin
    FVKSceneViewer.RemoveFreeNotification(Self);
    FVKSceneViewer.OnMouseMove := nil;
  end;

  FVKSceneViewer := Value;

  if FVKSceneViewer <> nil then
  begin
    FVKSceneViewer.OnMouseMove := ViewerMouseMove;
    FVKSceneViewer.FreeNotification(Self);
  end;
end;

procedure TVKSimpleNavigation.ShowFPS(Sender: TObject);
var
  Index: Integer;
  Temp: string;
begin
  if (FVKSceneViewer <> nil) and
     (FForm <> nil) and
     not(csDesigning in ComponentState) and
     (snoShowFPS in FOptions) then
  begin
    Temp := FFormCaption;
    Index := Pos(vFPSString, Temp);
    if Index <> 0 then
    begin
      Delete(Temp, Index, Length(vFPSString));
      Insert(FVKSceneViewer.FramesPerSecondText, Temp, Index);
    end;
    FForm.Caption := Temp;
    FVKSceneViewer.ResetPerformanceMonitor;
  end;
end;

function TVKSimpleNavigation.StoreFormCaption: Boolean;
begin
  Result := (FFormCaption <> vFPSString);
end;

function TVKSimpleNavigation.StoreMoveAroundTargetSpeed: Boolean;
begin
  Result := Abs(FMoveAroundTargetSpeed - 1) > EPS;
end;

function TVKSimpleNavigation.StoreZoomSpeed: Boolean;
begin
  Result := Abs(FZoomSpeed - 1.5) > EPS;
end;

function TVKSimpleNavigation.StoreRotateTargetSpeed: Boolean;
begin
  Result := Abs(FRotateTargetSpeed - 1) > EPS;
end;

procedure TVKSimpleNavigation.SetOptions(
  const Value: TVKSimpleNavigationOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;

  end;
end;

{ TVKSimpleNavigationKeyCombination }

procedure TVKSimpleNavigationKeyCombination.Assign(Source: TPersistent);
begin
  if Source is TVKSimpleNavigationKeyCombination then
  begin
    FExitOnMatch := TVKSimpleNavigationKeyCombination(Source).FExitOnMatch;
    FAction := TVKSimpleNavigationKeyCombination(Source).FAction;
    FOnCustomAction := TVKSimpleNavigationKeyCombination(Source).FOnCustomAction;
    FShiftState := TVKSimpleNavigationKeyCombination(Source).FShiftState;
  end
  else
    inherited; // Die!
end;

constructor TVKSimpleNavigationKeyCombination.Create(Collection: TCollection);
begin
  inherited;
  FAction := snaNone;
  FExitOnMatch := True;
end;

procedure TVKSimpleNavigationKeyCombination.DoOnCustomAction(
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnCustomAction) then
    FOnCustomAction(Self, Shift, X, Y);
end;

function TVKSimpleNavigationKeyCombination.GetDisplayName: string;
begin
  Result := GetSetProp(Self, 'ShiftState', True) + '  -  ' +
    GetEnumName(TypeInfo(TVKSimpleNavigationAction), Integer(FAction));
end;

{ TVKSimpleNavigationKeyCombinations }

function TVKSimpleNavigationKeyCombinations.Add: TVKSimpleNavigationKeyCombination;
begin
  Result := TVKSimpleNavigationKeyCombination(inherited Add);
end;

function TVKSimpleNavigationKeyCombinations.Add(
  const AShiftState: TShiftState; const AAction: TVKSimpleNavigationAction;
  const AExitOnMatch: Boolean): TVKSimpleNavigationKeyCombination;
begin
  Result := Add;
  with Result do
  begin
    FShiftState := AShiftState;
    FAction := AAction;
    FExitOnMatch := AExitOnMatch;
  end;
end;

function TVKSimpleNavigationKeyCombinations.GetItems(
  Index: Integer): TVKSimpleNavigationKeyCombination;
begin
  Result := TVKSimpleNavigationKeyCombination(inherited GetItem(Index));
end;

procedure TVKSimpleNavigationKeyCombinations.SetItems(Index: Integer;
  const Value: TVKSimpleNavigationKeyCombination);
begin
  inherited SetItem(Index, Value);
end;

end.

