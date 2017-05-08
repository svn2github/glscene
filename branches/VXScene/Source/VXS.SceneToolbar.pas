//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Added a toolbar to Delphi IDE. 
  
}

unit VXS.SceneToolbar;

interface

implementation

uses
  System.Classes,
  System.SysUtils,
  FMX.Graphics,
  FMX.ImgList,
  FMX.Controls,
  FMX.ComCtrls,
  FMX.ExtCtrls,
  FMX.ActnList,

  ToolsAPI,

  VXS.Scene,
  VXS.Generics;

const
  cVXSceneViewerToolbar = 'VXSceneViewerToolbar';

type

  TVXSToolButtonReceiver = class
  protected
    FActionList: GList<TBasicAction>;
    procedure OnClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  vReciver: TVXSToolButtonReceiver;

function MsgServices: IOTAMessageServices;
  begin
    Result := (BorlandIDEServices as IOTAMessageServices);
    Assert(Result <> nil, 'IOTAMessageServices not available');
  end;

procedure AddGLSceneToolbar;

  var
    Services: INTAServices;
    T: Integer;
    GLToolbar: TToolBar;

    procedure AddButton(const AHint, AResName: string);
      var
        Bmp: TBitmap;
        Act: TAction;
      begin
        Act := TAction.Create(nil);
        Act.ActionList := Services.ActionList;
        vReciver.FActionList.Add(Act);

        Bmp := TBitmap.Create;
        Bmp.LoadFromResourceName(HInstance, AResName);
        Act.ImageIndex := Services.AddMasked(Bmp, Bmp.TransparentColor, 'GLScene.' + AResName);
        Bmp.Destroy;

        Act.Hint := AHint;
        Act.Tag := T;
        Act.OnExecute := vReciver.OnClick;

        with Services.AddToolButton(cVXSceneViewerToolbar, 'GLSButton' + IntToStr(T), Act) do
          Action := Act;
        Act.Enabled := True;

        Inc(T);
      end;

  begin

    if not Supports(BorlandIDEServices, INTAServices, Services) then
      exit;

    GLToolbar := Services.ToolBar[cVXSceneViewerToolbar];
    vReciver := TVXSToolButtonReceiver.Create;
    T := 0;

    if not Assigned(GLToolbar) then
    begin
      GLToolbar := Services.NewToolbar(cVXSceneViewerToolbar, 'VXScene Viewer Control');
      if Assigned(GLToolbar) then
        with GLToolbar do
        begin
          AddButton('VXSceneViewer default control mode', 'VXSceneViewerControlToolbarDefault');
          AddButton('VXSceneViewer navigation mode', 'VXSceneViewerControlToolbarNavigation');
          AddButton('VXSceneViewer gizmo mode', 'VXSceneViewerControlToolbarGizmo');
          AddButton('Reset view to VXSceneViewer camera', 'VXSceneViewerControlToolbarCameraReset');
          Visible := True;
        end;
      MsgServices.AddTitleMessage('VXScene Toolbar created');
    end
    else
    begin
      for T := 0 to GLToolbar.ButtonCount - 1 do
      begin
        GLToolbar.Buttons[T].Action.OnExecute := vReciver.OnClick;
        vReciver.FActionList.Add(GLToolbar.Buttons[T].Action);
      end;
      MsgServices.AddTitleMessage('VXScene Toolbar activated');
    end;
    Services.ToolbarModified(GLToolbar);
  end;

constructor TVXSToolButtonReceiver.Create;
begin
  FActionList := GList<TBasicAction>.Create;
  vVXSceneViewerMode := svmDefault;
end;

destructor TVXSToolButtonReceiver.Destroy;
var
  I: Integer;
begin
  for I := 0 to FActionList.Count - 1 do
    FActionList[I].OnExecute := nil;
  FActionList.Destroy;
  vVXSceneViewerMode := svmDisabled;
end;

procedure TVXSToolButtonReceiver.OnClick(Sender: TObject);
  const
    cMode: array [TVXSceneViewerMode] of string = ('', 'default', 'navigation', 'gizmo');
  var
    T: Integer;
  begin
    inherited;
    T := TComponent(Sender).Tag;
    if T < 3 then
    begin
      vVXSceneViewerMode := TVXSceneViewerMode(T+1);
      MsgServices.AddTitleMessage(Format('VXSceneViewer %s mode', [cMode[vVXSceneViewerMode]]));
    end
    else
      vResetDesignView := True;
  end;

initialization

  AddGLSceneToolbar;

finalization

  vReciver.Free;

end.
