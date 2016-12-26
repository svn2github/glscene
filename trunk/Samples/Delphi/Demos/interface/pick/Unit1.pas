unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  // GLS
  GLScene,
  GLObjects,
  GLTexture,
  GLWin32Viewer,
  GLGeomObjects,
  GLColor,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    oldPick: TGLCustomSceneObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  pick: TGLCustomSceneObject;
begin
  // find what's under the mouse
  pick := (GLSceneViewer1.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  // if it has changed since last MouseMove...
  if (pick <> oldPick) then
  begin
    // ...turn to black previous "hot" object...
    if Assigned(oldPick) then
      oldPick.Material.FrontProperties.Emission.Color := clrBlack;
    // ...and heat up the new selection...
    if Assigned(pick) then
      pick.Material.FrontProperties.Emission.Color := clrRed;
    // ...and don't forget it !
    oldPick := pick;
  end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pick: TGLCustomSceneObject;
begin
  // if an object is picked...
  pick := (GLSceneViewer1.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  if Assigned(pick) then
  begin
    // ...turn it to yellow and show its name
    pick.Material.FrontProperties.Emission.Color := clrYellow;
    ShowMessage('You clicked the ' + pick.Name);
  end;
end;

end.
