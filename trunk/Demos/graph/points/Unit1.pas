{: Demo of the TGLPoints component.<p>

   The component is specialized in rendering large numbers of points,
   with ability to adjust point style (from fast square point to smooth
   round points) and point parameters.<p>
   The point parameters define how point size is adjusted with regard
   to eye-point distance (to make farther points smaller, see ARB_point_parameters
   for more details).<p>
   The component is also suitable for particle systems, but offers less
   flexibility than the TGLParticleFX.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLWin32Viewer, GLMisc, StdCtrls, Geometry, VectorLists,
  GLCadencer, GLTexture, ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLPoints1: TGLPoints;
    GLCadencer1: TGLCadencer;
    GLPoints2: TGLPoints;
    Panel1: TPanel;
    CBPointParams: TCheckBox;
    CBAnimate: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBAnimateClick(Sender: TObject);
    procedure CBPointParamsClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   // allocate points in the 1st point set
   GLPoints1.Positions.Count:=180;
   // specify white color for the 1st point set
   // (if a single color is defined, all points will use it,
   // otherwise, it's a per-point coloring)
   GLPoints1.Colors.Add(clrWhite);
   // specify blue color for the 2nd point set
   GLPoints2.Colors.Add(clrBlue);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   i : Integer;
   f, a : Single;
   p : TAffineVectorList;
   v : TAffineVector;
begin
   // update the 1st point set with values from a math func
   f:=1+Cos(newTime);
   p:=GLPoints1.Positions;
   for i:=0 to 179 do begin
      a:=DegToRad(4*i)+newTime*0.1;
      v[0]:=2*Cos(a);
      v[1]:=2*Cos(f*a);
      v[2]:=2*Sin(a);
      p[i]:=v;
   end;
   // notify the point set of the change (so that the scene gets updated)
   GLPoints1.StructureChanged;
   // replicate points in second set
   GLPoints2.Positions:=GLPoints1.Positions;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.CBAnimateClick(Sender: TObject);
begin
   GLCadencer1.Enabled:=CBAnimate.Checked;
end;

procedure TForm1.CBPointParamsClick(Sender: TObject);
begin
   GLPoints1.PointParameters.Enabled:=CBPointParams.Checked;
   GLPoints2.PointParameters.Enabled:=CBPointParams.Checked;
end;

end.
