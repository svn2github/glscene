{: Ultra-basic collision detection sample.<p>

   Two sphere have been placed in the scene, both have a TGLBCollision behaviour
   linked to the collision manager component.<p>

   Move them and click the button, if they collide, you will get a message, if
   not, nothing will happen. }
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLCollision, GLMisc, ComCtrls, StdCtrls,
  GLWin32Viewer, GLCrossPlatform;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    CollisionManager1: TCollisionManager;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Sphere1: TGLSphere;
    Sphere2: TGLSphere;
    TrackBar1: TTrackBar;
    Button1: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CollisionManager1Collision(Sender: TObject; object1,
      object2: TGLBaseSceneObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   Sphere1.Position.Z:=TrackBar1.Position/10;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   CollisionManager1.CheckCollisions;
end;

procedure TForm1.CollisionManager1Collision(Sender: TObject; object1,
  object2: TGLBaseSceneObject);
begin
   ShowMessage('Collision between '+object1.Name+' and '+object2.Name);
end;

end.
