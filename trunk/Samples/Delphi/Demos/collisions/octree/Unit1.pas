{: Octree raycast/mesh sample.<p>

   Demonstrating how to find the intersection point between eye-screen ray
   and a high poly mesh with an Octree property.

   To see the performance difference:

   -move mouse around on the scene with octree disabled (default)
   -check the "octree enabled" box.  Note the frame-rate difference.

   .<p>
}
unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms,
  GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer,
  GLCadencer, ExtCtrls, StdCtrls, GLGeomObjects, GLCrossPlatform, GLCoordinates,
  BaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    Sphere1: TGLSphere;
    ArrowLine1: TGLArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    LABuild: TLabel;
    CheckBox1: TCheckBox;
    CBOctree: TCheckBox;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mousex, mousey: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses VectorGeometry, VectorLists, GLFile3DS, GLUtils;

procedure TForm1.FormCreate(Sender: TObject);
var
   t : Int64;
begin
   SetGLSceneMediaDir();
   // Load high poly mesh (10,000 triangles).
   FreeForm1.LoadFromFile('HighPolyObject.3ds');

   t:=StartPrecisionTimer;

   FreeForm1.BuildOctree;

   LABuild.Caption:=Format('Build time: %.3f ms', [StopPrecisionTimer(t)*1000]);

   with FreeForm1.Octree do begin
      Label1.Caption:='Octree Nodes: '+inttostr(NodeCount);
      Label2.Caption:='Tri Count Octree: '+inttostr(TriCountOctree);
      Label3.Caption:='Tri Count Mesh: '+inttostr(TriCountMesh);
   end;

   mousex:= -1;
   mousey:= -1;
end;

procedure TForm1.GLSceneViewer2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   rayStart, rayVector, iPoint, iNormal : TVector;
   t : Int64;
begin
   SetVector(rayStart, GLCamera2.AbsolutePosition);
   SetVector(rayVector, GLSceneViewer2.Buffer.ScreenToVector(AffineVectorMake(x, GLSceneViewer2.Height-y, 0)));
   NormalizeVector(rayVector);

   t:=StartPrecisionTimer;
   if CBOctree.Checked then begin
      // Octree method (fast)
      if FreeForm1.OctreeRayCastIntersect(raystart, rayvector, @iPoint, @iNormal) then begin
         Sphere1.Visible:=True;
         Sphere1.Position.AsVector:=iPoint;
         Sphere1.Direction.AsVector:=VectorNormalize(iNormal);
      end else Sphere1.Visible:=False;
      Label4.Hint:='# Nodes hit with raycast: '+inttostr(High(FreeForm1.Octree.ResultArray)+1);
   end else begin
      // Brute-Force method (slow)
      if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then begin
         Sphere1.Visible:=True;
         Sphere1.Position.AsVector:=iPoint;
         Sphere1.Direction.AsVector:=VectorNormalize(iNormal);
      end else Sphere1.Visible:=False;
   end;
   Label5.Hint:=Format('Intersect Time: %.3f ms', [StopPrecisionTimer(t)*1000]);
end;

procedure TForm1.GLSceneViewer2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   mousex:=x;
   mousey:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if CheckBox1.Checked then
      GLSceneViewer2MouseDown(Sender, TMouseButton(mbLeft), [ssShift], mousex, mousey);

   FreeForm1.RollAngle:=5*newTime; // 45� per second
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // Show FPS Rating
	Caption:=Format('%.2f FPS', [GLSceneViewer2.FramesPerSecond]);
	GLSceneViewer2.ResetPerformanceMonitor;
   // Not doing so causes ugly flickering and a significant decrease in FPS...
   with Label4 do Caption:=Hint;
   with Label5 do Caption:=Hint;
end;

end.