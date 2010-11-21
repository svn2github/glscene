//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSceneViewerToolbar<p>

   Added GLScene's toolbar to Delphi IDE.<p>

 <b>History : </b><font size=-1><ul>
      <li>21/11/10 - Yar - Creation
 </ul></font>
}

unit GLSceneToolbar;

interface

procedure Register;

implementation

uses
  Classes,
  SysUtils,
  Graphics,
  ImgList,
  Controls,
  ComCtrls,
  ExtCtrls,
  ToolsAPI,
  GLScene;

type
  TToolBarClass = class of TToolBar;

  TGLSToolButtonReciver = class
  protected
    procedure OnClick(Sender: TObject);
  end;

var
  vReciver: TGLSToolButtonReciver;

function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;

procedure Register;

  var
    IDEImageList: TCustomImageList;
    Services: INTAServices;
    StdToolbar: TToolBar;
    DockToolbar: TToolBarClass;
    IDEBarControl: TControlBar;
    GLSceneViewerToolbar: TToolBar;
    I, T: Integer;

  procedure AddButton(const AHint: string);
  begin
    with TToolButton.Create(GLSceneViewerToolbar) do
    try
      AutoSize := True;
      Grouped := True;
      Parent := GLSceneViewerToolbar;
      ImageIndex := I;
      Hint := AHint;
      Tag := T;
      OnClick := vReciver.OnClick;
      Dec(T);
    except
      Free;
    end;
  end;

  var
    Bmp: TBitmap;

begin
  if not Supports(BorlandIDEServices, INTAServices, Services) then
    exit;
  if Assigned(Services.ToolBar['GLSceneViewerToolbar']) then
    exit;
  IDEImageList := Services.ImageList;
  StdToolbar := Services.ToolBar['StandardToolBar'];
  DockToolbar := TToolBarClass(StdToolBar.ClassType);
  IDEBarControl := StdToolBar.Parent as TControlBar;
  GLSceneViewerToolbar := DockToolBar.Create(IDEBarControl);
  with GLSceneViewerToolbar do
  begin
    Visible := False;
    Parent := IDEBarControl;
    Name := 'GLSceneViewerToolbar';
    Caption := 'GLScene Viewer Control';
    EdgeInner := StdToolbar.EdgeInner;
    EdgeOuter := StdToolbar.EdgeOuter;
    Flat := StdToolbar.Flat;
    Images := IDEImageList;
    OnGetSiteInfo := StdToolBar.OnGetSiteInfo;
    Constraints.MinHeight := StdToolBar.Constraints.MinHeight;
    Constraints.MinWidth := StdToolBar.Constraints.MinWidth;
    DockSite := StdToolBar.DockSite;
    DragKind := StdToolBar.DragKind;
    DragMode := StdToolBar.DragMode;
    PopupMenu := StdToolBar.PopupMenu;
    OnStartDock := StdToolBar.OnStartDock;
    OnEndDock := StdToolBar.OnEndDock;
    ShowHint := StdToolBar.ShowHint;
    ShowCaptions := StdToolBar.ShowCaptions;

    vReciver := TGLSToolButtonReciver.Create;
    T := 3;

    Bmp := TBitmap.Create;
    Bmp.LoadFromResourceName(HInstance, 'GLSceneViewerControlToolbarCameraReset');
    I := Services.AddMasked(Bmp, Bmp.TransparentColor, 'GLScene.Viewer control view reset image');
    Bmp.Destroy;
    AddButton('Reset view to GLSceneViewer camera');

    Bmp := TBitmap.Create;
    Bmp.LoadFromResourceName(HInstance, 'GLSceneViewerControlToolbarGizmo');
    I := Services.AddMasked(Bmp, Bmp.TransparentColor, 'GLScene.Viewer control gizmo image');
    Bmp.Destroy;
    AddButton('GLSceneViewer gizmo mode');

    Bmp := TBitmap.Create;
    Bmp.LoadFromResourceName(HInstance, 'GLSceneViewerControlToolbarNavigation');
    I := Services.AddMasked(Bmp, Bmp.TransparentColor, 'GLScene.Viewer control navigation image');
    Bmp.Destroy;
    AddButton('GLSceneViewer navigation mode');

    Bmp := TBitmap.Create;
    Bmp.LoadFromResourceName(HInstance, 'GLSceneViewerControlToolbarDefault');
    I := Services.AddMasked(Bmp, Bmp.TransparentColor, 'GLScene.Viewer control default image');
    Bmp.Destroy;
    AddButton('GLSceneViewer default control mode');

    Left := StdToolBar.Left + StdToolBar.Width;
    Top := StdToolBar.Top;
    Height := StdToolBar.Height;

    Visible := True;
  end;

  IDEBarControl.StickControls;
end;

procedure TGLSToolButtonReciver.OnClick(Sender: TObject);
const
  cMode: array[TGLSceneViewerMode] of string =
    ('default', 'navigation', 'gizmo');
var
  t: Integer;
begin
  inherited;
  t := TToolButton(Sender).Tag;
  if t<3 then
  begin
    vGLSceneViewerMode := TGLSceneViewerMode(t);
    MsgServices.AddTitleMessage(Format('GLSceneViewer %s mode', [cMode[vGLSceneViewerMode]]));
  end
  else
    vResetDesignView := True;
end;

end.

