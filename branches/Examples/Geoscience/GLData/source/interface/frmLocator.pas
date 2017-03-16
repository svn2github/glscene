unit frmLocator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls,
  StdCtrls, geFloatEdit, ExtCtrls, System.Actions;

type
  TformLocator = class(TForm)
    ActionToolBar_Locator: TActionToolBar;
    ActionManager: TActionManager;
    ImageList: TImageList;
    acVisible: TAction;
    acGetFocus: TAction;
    pnlLocator: TPanel;
    lblLocatorX: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    acGetCamera: TAction;
    acToFocus: TAction;
    acClose: TAction;
    procedure acVisibleExecute(Sender: TObject);
    procedure acGetFocusExecute(Sender: TObject);
    procedure geLocatorXExit(Sender: TObject);
    procedure geLocatorYExit(Sender: TObject);
    procedure geLocatorZExit(Sender: TObject);
    procedure acGetCameraExecute(Sender: TObject);
    procedure acToFocusExecute(Sender: TObject);
  private
     
  public
     
  end;

var
  formLocator: TformLocator;

implementation

uses
  frmMain;

{$R *.dfm}
// ----- TformLocator.acVisibleExecute -----------------------------------------
procedure TformLocator.acVisibleExecute(Sender: TObject);

begin
  formMain.glsLocator.Visible := acVisible.Checked;
end;
// ----- TformLocator.acGetFocusExecute ----------------------------------------
procedure TformLocator.acGetFocusExecute(Sender: TObject);
begin
  geLocatorX.Value := formMain.GLDummyCube.Position.X/
    formMain.usersettings.ScaleX;
  geLocatorY.Value := formMain.GLDummyCube.Position.Y/
    formMain.usersettings.ScaleY;
  geLocatorZ.Value := formMain.GLDummyCube.Position.Z/
    formMain.usersettings.ScaleZ;

  with formMain do
  begin
    glsLocator.Position.X := GLDummyCube.Position.X;
    glsLocator.Position.Y := GLDummyCube.Position.Y;
    glsLocator.Position.Z := GLDummyCube.Position.Z;
  end;
end;
// ----- TformLocator.geLocatorXExit -------------------------------------------
procedure TformLocator.geLocatorXExit(Sender: TObject);

begin
  with formMain do
  begin
    glsLocator.Position.X := geLocatorX.Value*usersettings.ScaleX;
    TransformCamera;
  end;
end;
// ----- TformLocator.geLocatorYExit -------------------------------------------
procedure TformLocator.geLocatorYExit(Sender: TObject);

begin
  with formMain do
  begin
    glsLocator.Position.Y := geLocatorY.Value*usersettings.ScaleY;
    TransformCamera;
  end;
end;
// ----- TformLocator.geLocatorZExit -------------------------------------------
procedure TformLocator.geLocatorZExit(Sender: TObject);

begin
  with formMain do
  begin
    glsLocator.Position.Z := geLocatorZ.Value*usersettings.ScaleZ;
    TransformCamera;
  end;
end;
// ----- TformLocator.acGetCameraExecute ---------------------------------------
procedure TformLocator.acGetCameraExecute(Sender: TObject);
begin
  with formMain do
  begin
    geLocatorX.Value := GLCamera.AbsolutePosition.V[0]/usersettings.ScaleX;
    geLocatorY.Value := GLCamera.AbsolutePosition.V[1]/usersettings.ScaleY;
    geLocatorZ.Value := GLCamera.AbsolutePosition.V[2]/usersettings.ScaleZ;
    glsLocator.Position.X := GLCamera.AbsolutePosition.V[0];
    glsLocator.Position.Y := GLCamera.AbsolutePosition.V[1];
    glsLocator.Position.Z := GLCamera.AbsolutePosition.V[2];
    TransformCamera;
  end;
end;
// ----- TformLocator.acToFocusExecute -----------------------------------------
procedure TformLocator.acToFocusExecute(Sender: TObject);

begin
  with formMain do
  begin
    GLDummyCube.Position.X := geLocatorX.Value*userSettings.ScaleX;
    GLDummyCube.Position.Y := geLocatorY.Value*userSettings.ScaleY;
    GLDummyCube.Position.Z := geLocatorZ.Value*userSettings.ScaleZ;
    TransformCamera;
  end;
end;
// =============================================================================
end.




