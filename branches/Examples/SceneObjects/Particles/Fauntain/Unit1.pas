unit unit1;
{
  This is a little example about particles system.

  Here we use the Direction property of the TGLSphere like
  a motion vector for the movement.

  *****************************************************************
  *                                                               *
  *       .---.        .-----------   L.I. CARLOS GARCÍA TRUJILLO *
  *      /     \  __  /    ------                                 *
  *     / /     \(  )/    -----                                   *
  *    //////   ' \/ `   ---    Email:                            *
  *   //// / // :    : ---             cgar1136@yahoo.com         *
  *  // /   /  /`    '--        ICQ:                              *
  * //          //..\\                 26256096                   *
  *         ===UU====UU====                                       *
  *            '//||\\`     WEB:                                  *
  *              ''``          http://www.geocities.com/cgar1136/ *
  *                                                               *
  *****************************************************************
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  //GLS
  GLScene, GLObjects, GLParticles, GLWin32Viewer,
  GLCadencer, GLBehaviours, GLVectorTypes, GLVectorGeometry,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Fountain: TGLParticles;
    Sphere1: TGLSphere;
    GLCadencer1: TGLCadencer;
    DummyCube1: TGLDummyCube;
    Button1: TButton;
    GLLightSource1: TGLLightSource;
    Plane1: TGLPlane;
    procedure FormCreate(Sender: TObject);
    procedure Sphere1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
     
  public
     
  end;

const
  DROP_COUNT = 800; // How many drops we wish...

var
  motion: array [0 .. DROP_COUNT] of TVector4f;
  Form1: TForm1;
  mdx, mdy: Integer;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: word;
begin
  Randomize;
  for i := 0 to DROP_COUNT - 1 do
    with TGLSphere(Fountain.CreateParticle) do
    begin
      Position.SetPoint(0, 0, 0); // a initial position at origin
      MakeVector(motion[i], (Random - 0.5) / 20, (Random / 5) + 0.01,
        (Random - 0.5) / 20);
      Tag := i;
    end;
end;

procedure TForm1.Sphere1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  with TGLSphere(Sender) do
  begin
    Translate(motion[TGLSphere(Sender).Tag].X, motion[TGLSphere(Sender).Tag].Y,
      motion[TGLSphere(Sender).Tag].Z);
    // translate(Direction.X,Direction.Y,Direction.Z);  //Translating the particle, by the motion vector.
    If (Position.Y < 0) then
    begin
      Position.setpoint(0, 0, 0); // Reset the particle's position
      makevector(motion[TGLSphere(Sender).Tag], (Random - 0.5) / 20,
        (Random / 5) + 0.01, (Random - 0.5) / 20);
    end
    else
      motion[TGLSphere(Sender).Tag].Y := motion[TGLSphere(Sender).Tag].Y - 0.01;
  end;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mdx := X;
  mdy := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  If Shift <> [] then
    GLCamera1.MoveAroundTarget(mdy - Y, mdx - X);
  mdx := X;
  mdy := Y;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  For i := 0 to Fountain.count - 1 do // Reseting the
    with TGLSphere(Fountain.Children[i]) do // particle system
    begin
      Position.SetPoint(0, -0.5, 0);
      makevector(motion[i], (Random - 0.5) / 20, (Random / 7) + 0.01,
        (Random - 0.5) / 20);
    end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := 50 * Width / 350;
end;

end.
