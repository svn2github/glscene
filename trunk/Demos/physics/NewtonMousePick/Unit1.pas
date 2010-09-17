unit Unit1;

{ : Newton Game Dynamics Physics Engine demo.<p>

  This exemple show how to move a body by calling the pick function of dynamic
  behavior. Most of the code must be written by developer in mouse events.

  <b>History : </b><font size=-1><ul>
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLNGDManager,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  BaseClasses,
  VectorGeometry;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    GLCube1: TGLCube;
    GLNGDManager1: TGLNGDManager;
    GLRenderPoint1: TGLRenderPoint;
    GLSphere1: TGLSphere;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    FPickedSceneObject: TGLBaseSceneObject;
    NGDDynamicBehav: TGLNGDDynamic;
    point3d, FPaneNormal: TVector;

  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$r *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  point3d := VectorMake(GLSceneViewer1.Buffer.PixelRayToWorld(X, Y));

  FPickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(X, Y);

  if Assigned(FPickedSceneObject) then // If the user click on a glSceneObject
  begin
    NGDDynamicBehav := FPickedSceneObject.Behaviours.GetByClass(TGLNGDDynamic)
      as TGLNGDDynamic;
    if Assigned(NGDDynamicBehav) then
      // point3d is the global space point of the body to attach
      NGDDynamicBehav.Pick(point3d, pmAttach)
    else
      FPickedSceneObject := nil;
    // We save the normal to create a plane parallel to camera in mouseMove Event.
    FPaneNormal := GLCamera1.AbsoluteDirection;
  end;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  point2d, GotoPoint3d: TVector;
begin
  if Assigned(NGDDynamicBehav) then
  begin

    // Get the screenPoint with opengl correction [Height - Y] for the next function
    point2d := VectorMake(X, GLSceneViewer1.Height - Y, 0, 0);

    // Get the intersect point between the plane [parallel to camera] and mouse position
    if GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlane(point2d, point3d,
      FPaneNormal, GotoPoint3d) then
      NGDDynamicBehav.Pick(GotoPoint3d, pmMove);
    // Move the body to the new position
  end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Detach the body
  if Assigned(NGDDynamicBehav) then
  begin
    NGDDynamicBehav.Pick(point3d, pmDetach);
    NGDDynamicBehav := nil;
    FPickedSceneObject := nil;
  end;
end;

end.
