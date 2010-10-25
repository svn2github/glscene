unit Unit1;

{ : Newton Game Dynamics Physics Engine demo.<p>

  This exemple show Joints.
  Mouse1 to pick, Mouse2 to move camera.

  When you create Joints with NGD, it's better if one of the two bodies is
  static.
  In debug view (If ShowJoint is true in manager), the blues lines represent
  pins direction and connections between BaseSceneObjects. However if you create
  multiples connected joints (ex: FLOOR<--HINGE-->CUBE<--HINGE-->SPHERE), the
  debug view won't match to bodies positions because Joints are represented in
  global space. Debug view was made for design time.

  Linear and angular damping is added to universal and ball to avoid instability.

  <b>History : </b><font size=-1><ul>
  <li>20/09/10 - FP - Created by Franck Papouin
  </ul>
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLNGDManager, GLScene, GLObjects, GLCoordinates, GLCadencer,
  GLWin32Viewer, GLCrossPlatform, BaseClasses, VectorGeometry,
  GLSimpleNavigation, GLKeyboard, GLGeomObjects, GLHUDObjects, GLBitmapFont,
  GLWindowsFont;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    Hinge: TGLCube;
    GLNGDManager1: TGLNGDManager;
    GLRenderPoint1: TGLRenderPoint;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLNGDJointList1: TGLNGDJointList;
    Slider: TGLCube;
    Corkscrew: TGLCube;
    CustomHinge: TGLCube;
    CustomSlider: TGLCube;
    Universal: TGLCone;
    CustomBall: TGLSphere;
    Ball: TGLSphere;
    GLStoredBitmapFont1: TGLStoredBitmapFont;
    GLAbsoluteHUDText1: TGLAbsoluteHUDText;
    GLAbsoluteHUDText2: TGLAbsoluteHUDText;
    GLAbsoluteHUDText3: TGLAbsoluteHUDText;
    GLAbsoluteHUDText4: TGLAbsoluteHUDText;
    GLAbsoluteHUDText5: TGLAbsoluteHUDText;
    GLAbsoluteHUDText6: TGLAbsoluteHUDText;
    GLAbsoluteHUDText8: TGLAbsoluteHUDText;
    GLAbsoluteHUDText7: TGLAbsoluteHUDText;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSimpleNavigation1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Déclarations privées }
    FPickedSceneObject: TGLBaseSceneObject;
    NGDDynamicBehav: TGLNGDDynamic;
    point3d, FPaneNormal: TVector;
    FAlreadyInSimpleMove: Boolean;

  public
    { Déclarations publiques }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbLeft) then
    if IsKeyDown(VK_LBUTTON) then
    begin

      point3d := VectorMake(GLSceneViewer1.Buffer.PixelRayToWorld(X, Y));

      FPickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(X, Y);

      if Assigned(FPickedSceneObject) then // If the user click on a glSceneObject
      begin
        NGDDynamicBehav := FPickedSceneObject.Behaviours.GetByClass
          (TGLNGDDynamic) as TGLNGDDynamic;
        if Assigned(NGDDynamicBehav) then
          // point3d is the global space point of the body to attach
          NGDDynamicBehav.Pick(point3d, pmAttach)
        else
          FPickedSceneObject := nil;
        // We save the normal to create a plane parallel to camera in mouseMove Event.
        FPaneNormal := GLCamera1.AbsoluteVectorToTarget;
      end;
    end;

end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  point2d, GotoPoint3d: TVector;
begin
  if IsKeyDown(VK_LBUTTON) then
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
  if not IsKeyDown(VK_LBUTTON) then
    if Assigned(NGDDynamicBehav) then
    begin
      NGDDynamicBehav.Pick(point3d, pmDetach);
      NGDDynamicBehav := nil;
      FPickedSceneObject := nil;
    end;
end;

procedure TForm1.GLSimpleNavigation1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FAlreadyInSimpleMove then
    exit;
  FAlreadyInSimpleMove := True;
  GLSceneViewer1MouseMove(Sender, Shift, X, Y);
  FAlreadyInSimpleMove := False;
end;

end.
