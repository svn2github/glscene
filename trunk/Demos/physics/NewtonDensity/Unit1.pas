unit Unit1;

{ : Newton Game Dynamics Physics Engine demo.<p>

  Density unit is the number of mass unit per volume unit: D=M/V
  Body volume is calculated by Newton, and Mass is the product result of
  Density*Volume.

  The Manager can add water, by setting a positive waterdensity.

  If waterdensity=body density, the body will be immerged like a submarine.

  Density is also an important parameter when two bodies collide.
  The shoot button launche a cube with constant force. You can see the result
  when the cube hit the paper ball or the lead ball.

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
  GLObjects,
  GLScene,
  GLCoordinates,
  GLSimpleNavigation,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  BaseClasses,
  VectorGeometry,
  StdCtrls,
  GLHUDObjects,
  GLBitmapFont,
  GLWindowsFont,
  Spin;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLNGDManager1: TGLNGDManager;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    GLDummyCube1: TGLDummyCube;
    SubMarine: TGLCube;
    GLPaperSphere: TGLSphere;
    GLLeadSphere: TGLSphere;
    Button1: TButton;
    GLCube1: TGLCube;
    SpinEdit1: TSpinEdit;
    GLStoredBitmapFont1: TGLStoredBitmapFont;
    GLHUDText1: TGLHUDText;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$r *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Ball: TGLCube;
  NGDDyn: TGLNGDDynamic;
begin
  Ball := TGLCube.CreateAsChild(GLCamera1);
  Ball.CubeWidth := 0.5;
  Ball.CubeHeight := 0.5;
  Ball.CubeDepth := 0.5;
  NGDDyn := Ball.GetOrCreateBehaviour(TGLNGDDynamic) as TGLNGDDynamic;
  NGDDyn.Manager := GLNGDManager1;
  NGDDyn.Density := 10;

  // Add force in the camera direction
  NGDDyn.Force.AsVector := VectorScale(GLCamera1.AbsoluteVectorToTarget, 100);

  // Remove gravity
  NGDDyn.Force.Y := NGDDyn.Force.Y - GLNGDManager1.Gravity.Y * NGDDyn.Mass;

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  GLNGDManager1.WaterDensity := StrToFloat(SpinEdit1.Text);
end;

end.
