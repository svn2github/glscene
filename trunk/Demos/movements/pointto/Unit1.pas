{: Demo/test case for the PointTo method of objects.<p>

   The PointTo method allows to easily orient an object to point toward another
   object, whatever their relative positions in the scene hierarchy.<br>
   In this sample, we have a green sphere turning in circle and riding a sin,
   while a blue arrow, turning in a smaller circle, is maintained pointed
   toward the sphere. The other items (lines...) are just here to help visualize
   the 3D nature of the thing.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLObjects, GLScene, GLMisc, StdCtrls, Geometry, GLCadencer;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DCSphere: TDummyCube;
    ArrowLine: TArrowLine;
    GLLightSource1: TGLLightSource;
    Sphere: TSphere;
    DCArrow: TDummyCube;
    GLCadencer1: TGLCadencer;
    Disk1: TDisk;
    Disk2: TDisk;
    Lines1: TLines;
    Plane1: TPlane;
    Lines2: TLines;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // Make the blue sphere turn and ride a sin
   DCSphere.Turn(deltaTime*30);
   Sphere.Position.Y:=Sin(DegToRad(newTime*50))*3;

   // Make the arrow turn
   DCArrow.Turn(-deltaTime*15);

   // Make the arrow point toward the sphere, using Y as up reference
   ArrowLine.PointTo(Sphere, YHmgVector);
end;

end.
