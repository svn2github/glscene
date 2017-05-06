//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Added a toolbar to Delphi IDE. 
  
}

unit VKS.SceneToolbar;

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

  VKS.Scene,
  VKS.Generics;

const
  cVKSceneViewerToolbar = 'VKSceneViewerToolbar';

type

  TVKSToolButtonReceiver = class
  protected
    FActionList: GList<TBasicAction>;
    procedure OnClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  vReciver: TVKSToolButtonReceiver;

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

        with Services.AddToolButton(cVKSceneViewerToolbar, 'GLSButton' + IntToStr(T), Act) do
          Action := Act;
        Act.Enabled := True;

        Inc(T);
      end;

  begin

    if not Supports(BorlandIDEServices, INTAServices, Services) then
      exit;

    GLToolbar := Services.ToolBar[cVKSceneViewerToolbar];
    vReciver := TVKSToolButtonReceiver.Create;
    T := 0;

    if not Assigned(GLToolbar) then
    begin
      GLToolbar := Services.NewToolbar(cVKSceneViewerToolbar, 'VKScene Viewer Control');
      if Assigned(GLToolbar) then
        with GLToolbar do
        begin
          AddButton('VKSceneViewer default control mode', 'VKSceneViewerControlToolbarDefault');
          AddButton('VKSceneViewer navigation mode', 'VKSceneViewerControlToolbarNavigation');
          AddButton('VKSceneViewer gizmo mode', 'VKSceneViewerControlToolbarGizmo');
          AddButton('Reset view to VKSceneViewer camera', 'VKSceneViewerControlToolbarCameraReset');
          Visible := True;
        end;
      MsgServices.AddTitleMessage('VKScene Toolbar created');
    end
    else
    begin
      for T := 0 to GLToolbar.ButtonCount - 1 do
      begin
        GLToolbar.Buttons[T].Action.OnExecute := vReciver.OnClick;
        vReciver.FActionList.Add(GLToolbar.Buttons[T].Action);
      end;
      MsgServices.AddTitleMessage('VKScene Toolbar activated');
    end;
    Services.ToolbarModified(GLToolbar);
  end;

constructor TVKSToolButtonReceiver.Create;
begin
  FActionList := GList<TBasicAction>.Create;
  vVKSceneViewerMode := svmDefault;
end;

destructor TVKSToolButtonReceiver.Destroy;
var
  I: Integer;
begin
  for I := 0 to FActionList.Count - 1 do
    FActionList[I].OnExecute := nil;
  FActionList.Destroy;
  vVKSceneViewerMode := svmDisabled;
end;

procedure TVKSToolButtonReceiver.OnClick(Sender: TObject);
  const
    cMode: array [TVKSceneViewerMode] of string = ('', 'default', 'navigation', 'gizmo');
  var
    T: Integer;
  begin
    inherited;
    T := TComponent(Sender).Tag;
    if T < 3 then
    begin
      vVKSceneViewerMode := TVKSceneViewerMode(T+1);
      MsgServices.AddTitleMessage(Format('VKSceneViewer %s mode', [cMode[vVKSceneViewerMode]]));
    end
    else
      vResetDesignView := True;
  end;

initialization

  AddGLSceneToolbar;

finalization

  vReciver.Free;

end.
