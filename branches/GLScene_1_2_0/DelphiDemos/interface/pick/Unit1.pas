{ : Basic interactive object picking<p>

  This is a bare bones sample on the use of the GetPickedObject function.
  Two events are handled : OnMouseMove triggers a color change (grey/red) when
  the mouse is moved over an object, and a message popups when an object is
  clicked in OnMouseDown.<p>

  In a real world proggie, both events should make use of the oldPick variable
  (since you can't click what is not under the mouse, the GetPickedObject in
  OnMouseDown returns what we already have in oldPick, set during the last
  OnMouseMove).
}
unit Unit1;

interface

uses
  Forms,
  Classes,
  Controls,
  Dialogs,

  GLScene_Cadencer,
  GLScene_Material,
  GLScene_MaterialEx,
  GLScene_Core,
  GLScene_ObjectsEx,
  GLScene_Objects,
  GLScene_Base_Coordinates,
  GLScene_Platform,
  GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
    Line: TGLLines;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    GLCadencer1: TGLCadencer;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
    { Déclarations privées }
    oldPick: TGLBaseSceneObject;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin
  // find what's under the mouse
  pick := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
  if pick <> oldPick then
  begin
    // if it has changed since last MouseMove...
    // ...turn to black previous "hot" object...
    if Assigned(oldPick) then
      TGLSceneObjectEx(oldPick).LibMaterialName := 'DefaultMaterial';
    // ...and heat up the new selection...
    if Assigned(pick) then
      TGLSceneObjectEx(pick).LibMaterialName := 'HighlightMaterial';
    // ...and don't forget it !
    oldPick := pick;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin
  // if an object is picked...
  pick := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
  if Assigned(pick) then
  begin
    // ...turn it to yellow and show its name
    TGLSceneObjectEx(pick).LibMaterialName := 'HighlightMaterial2';
    ShowMessage('You clicked the ' + pick.Name);
  end;
end;

end.
