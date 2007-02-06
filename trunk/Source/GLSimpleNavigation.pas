//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSimpleNavigation<p>

    A simple component written by request from someone at the www.glscene.ru forums.<p>
    Allows to view the FPS and do the usual Zoom and MoveAroundTarget stuff <p>
    that all demos usually have in themselves. All that is just by dropping <p>
    this component on the form.<p>

   <b>History : </b><font size=-1><ul>
	    <li>06/02/07 - DaStr - Creation (donated to GLScene)
	</ul></font><p>



Previous version History:
           v1.0   08 May        '2006  Creation
           v1.1   04 September  '2006  FreeNotification fix
                                       Automatic Form detection fix
           v1.2   11 September  '2006  Automatic SceneViewer detection
                                       CaptionString added
           v1.3   06 February   '2007  FPS is only updated in Run-Time now
                                       Donated to GLScene
}

unit GLSimpleNavigation;

interface

uses
  //VCL
  Classes, Forms, ExtCtrls, Types, SysUtils,

  //GLSCene
  VectorGeometry, GLScene, GLWin32Viewer;

type

  TGLSimpleNavigation = class(TComponent)
  private
    FTimer: TTimer;
    FForm: TCustomForm;
    FGLSceneViewer: TGLSceneViewer;

    FOldx, FOldy: Integer;
    FCaptionString: string;
    FMoveAroundTargetParam: Single;
    FZoomParam: Single;
    procedure ShowFPS(Sender: TObject);
    procedure GLSceneViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure SetGLSceneViewer(const Value: TGLSceneViewer);
    procedure SetForm(const Value: TCustomForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Form: TCustomForm read FForm write SetForm;
    property GLSceneViewer: TGLSceneViewer read FGLSceneViewer write SetGLSceneViewer;
    property ZoomParam: Single read FZoomParam write FZoomParam;
    property MoveAroundTargetParam: Single read FMoveAroundTargetParam write FMoveAroundTargetParam;
    property CaptionString: string read FCaptionString write FCaptionString;
  end;

implementation

const
  FPSString = '%FPS';

{ TGLSimpleNavigation }

constructor TGLSimpleNavigation.Create(AOwner: TComponent);
const
  STR_SCENE_VIEWER = 'SceneViewer';
  STR_GL = 'GL';
var
  Temp: TComponent;
begin
  inherited;
  FMoveAroundTargetParam := 1;
  FZoomParam := 1.5;
  FCaptionString := FPSString;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := ShowFPS;

  //Detect form
  if AOwner is TCustomForm then SetForm(TCustomForm(AOwner));

  //Detect SceneViewer
  if FForm <> nil then
  begin
    Temp := FForm.FindComponent(STR_SCENE_VIEWER);
    if Temp <> nil then
    begin
      SetGLSceneViewer(TGLSceneViewer(Temp));
      Exit;
    end;

    Temp := FForm.FindComponent(STR_GL + STR_SCENE_VIEWER);
    if Temp <> nil then
    begin
      SetGLSceneViewer(TGLSceneViewer(Temp));
      Exit;
    end;

    Temp := FForm.FindComponent(STR_GL + STR_SCENE_VIEWER + IntToStr(1));
    if Temp <> nil then
    begin
      SetGLSceneViewer(TGLSceneViewer(Temp));
      Exit;
    end;
  end;
end;

destructor TGLSimpleNavigation.Destroy;
begin
  FTimer.Destroy;

  if FForm <> nil then
    TForm(FForm).OnMouseWheel := nil;

  if FGLSceneViewer <> nil then
    FGLSceneViewer.OnMouseMove := nil;

  inherited;
end;

procedure TGLSimpleNavigation.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if FGLSceneViewer <> nil then
    if FGLSceneViewer.Camera <> nil then
      FGLSceneViewer.Camera.AdjustDistanceToTarget(Power(FZoomParam, WheelDelta div Abs(WheelDelta)));
end;

procedure TGLSimpleNavigation.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FGLSceneViewer <> nil then
    if FGLSceneViewer.Camera <> nil then
    begin
      if (ssLeft in Shift) and (ssRight in Shift) then
        FGLSceneViewer.Camera.AdjustDistanceToTarget(Power(FZoomParam, (y - FOldy) / 20))
      else if (ssLeft in Shift) or (ssRight in Shift) then
        FGLSceneViewer.Camera.MoveAroundTarget(FMoveAroundTargetParam * (FOldy - y),FMoveAroundTargetParam * (FOldx - x));
    end;

  FOldx :=x;
  FOldy :=y;
end;

procedure TGLSimpleNavigation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FGLSceneViewer) and (Operation = opRemove) then
    FGLSceneViewer := nil;
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;

procedure TGLSimpleNavigation.SetForm(const Value: TCustomForm);
begin
  if FForm <> nil then
  begin
    TForm(FForm).OnMouseWheel := nil;
    FForm.RemoveFreeNotification(Self);
  end;

  FForm := Value;

  if FForm <> nil then
  begin
    TForm(FForm).OnMouseWheel := FormMouseWheel;
    FForm.FreeNotification(Self);
  end;
end;

procedure TGLSimpleNavigation.SetGLSceneViewer(
  const Value: TGLSceneViewer);
begin
  if FGLSceneViewer <> nil then
  begin
    FGLSceneViewer.RemoveFreeNotification(Self);
    FGLSceneViewer.OnMouseMove := nil;
  end;

  FGLSceneViewer := Value;

  if FGLSceneViewer <> nil then
  begin
    FGLSceneViewer.OnMouseMove := GLSceneViewerMouseMove;
    FGLSceneViewer.FreeNotification(Self);
  end;
end;

procedure TGLSimpleNavigation.ShowFPS(Sender: TObject);
var
  Index: Integer;
  Temp: string;
begin
  if (FGLSceneViewer <> nil) and (FForm <> nil) and not(csDesigning in ComponentState) then
  begin
    Temp := FCaptionString;
    Index := Pos(FPSString, Temp);
    Delete(Temp, Index, Length(FPSString));
    Insert(FGLSceneViewer.FramesPerSecondText, Temp, Index);
    FForm.Caption := Temp;
    FGLSceneViewer.ResetPerformanceMonitor;
  end;
end;

end.
