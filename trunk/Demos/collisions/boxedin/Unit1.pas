{: Basic Octree/Collision demo.<p>

Robert Hayes, March 2002.

This demo uses the new Octree class to quickly detect triangle-level collisions with a sphere.

 **** This function is the result of a LOT of work and experimentation with both Paul Nettle's method
 (www.fluidstudios.com) and Telemachos' method (www.peroxide.dk) over a period of about 2 months. If
 you find ways to optimize the general structure (or bugs), please let me know at rmch@cadvision.com. ****

 TO DO:
 ======

- Response based on tangent plane
- R4 conversion (routine already exists for this in Octree) for ellipsoid space.
- Various optimizations.

  My method for caclulating sphere CD vs polygon:
 ...for each triangle:
 1. cast a ray from sphere origin to triangle's plane (normal scaled to sphere radius).
    If distance < 0 then skip checking this triangle.
 2. if the distance is =< the sphere radius, the plane is embedded in the sphere.
    Calculate poly intersection point and go to step 8.
 3. if the distance > sphere radius, calculate the sphere intersection point to this plane by
    subtracting the plane's normal from the sphere's origin
 4. cast a new ray from the sphere intersection point to the plane; this is the new plane intersection point
 6. determine if the plane intersection point lies within the triangle itself; if yes then the polygon
    intersection point = the plane intersection point
 7. else, find the point on the triangle that is closest to the plane intersection point; this becomes
    the polygon intersection point (ie: edge detection)
 8. cast a ray from the polygon intersection point back along the negative velocity vector of the sphere
 9. if there is no intersection, the sphere cannot possibly collide with this triangle
 10. else, save the distance from step 8 if, and only if, it is the shortest collision distance so far

}
unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Keyboard, Geometry, 
  GLMisc, GLScene, GLVectorFileObjects, GLObjects, GLWin32Viewer,
  GLCadencer, ExtCtrls, StdCtrls, GLNavigator, ComCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    DummyCube1: TDummyCube;
    FreeForm1: TFreeForm;
    Sphere1: TSphere;
    ArrowLine1: TArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    DummyCube2: TDummyCube;
    Sphere2: TSphere;
    GLLightSource2: TGLLightSource;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TrackBar1: TTrackBar;
    Button1: TButton;
    Lines1: TLines;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    colTotalTime : Single; // for timing collision detection
    colCount : Integer;
    procedure AddToTrail(const p : TVector);
  public
    { Public declarations }
    mousex, mousey: integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses GLCrossPlatform;

procedure TForm1.FormCreate(Sender: TObject);
begin
   FreeForm1.LoadFromFile('..\..\media\BoxedIn.3ds');

   FreeForm1.BuildOctree;
   Label1.Caption:='Octree Nodes    : '+inttostr(FreeForm1.Octree.NodeCount);
   Label2.Caption:='Tri Count Octree: '+inttostr(FreeForm1.Octree.TriCountOctree);
   Label3.Caption:='Tri Count Mesh  : '+inttostr(FreeForm1.Octree.TriCountMesh);

   Lines1.AddNode(0, 0, 0);
   Lines1.ObjectStyle:=Lines1.ObjectStyle+[osDirectDraw];
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   rayStart, rayVector : TVector;
   velocity : Single;
   pPoint : TVector;
   pNormal : TVector;
   t : Int64;
begin
   if IsKeyDown(VK_ESCAPE) then close;

   Velocity:=Trackbar1.Position*deltaTime;

   t:=StartPrecisionTimer;

   with FreeForm1 do begin
      SetVector(rayStart, Sphere2.AbsolutePosition);
      SetVector(rayVector, Sphere2.AbsoluteDirection);
      NormalizeVector(rayVector);
      //Note: since collision may be performed on multiple meshes, we need to know which hit
      //      is closest (ie: d:=raystart - pPoint).
      if OctreeSphereIntersect(raystart, rayvector, velocity, Sphere2.Radius*10,  //why 10? I don't know.
                               @pPoint, @pNormal) then begin
         // Show the polygon intersection point
         Sphere1.Position.AsVector:=pPoint;
         Sphere1.Direction.AsVector:=VectorNormalize(pNormal);

         // Make it rebound...
         with Sphere2.Direction do
            AsAffineVector:=VectorReflect(AsAffineVector, AffineVectorMake(pNormal));

         // Add intersect point to trail
         AddToTrail(pPoint);
      end else begin
         Sphere2.Move(velocity); //No collision, so just move the ball.
      end;
   end;
   // Last trail point is always the sphere's current position
   Lines1.Nodes.Last.AsVector:=Sphere2.Position.AsVector;

   colTotalTime:=colTotalTime+StopPrecisionTimer(t);
   Inc(colCount);
end;

procedure TForm1.AddToTrail(const p : TVector);
var
   i, k : Integer;
begin
   Lines1.Nodes.Last.AsVector:=p;
   Lines1.AddNode(0, 0, 0);
   if Lines1.Nodes.Count>20 then // limit trail to 20 points
      Lines1.Nodes.Delete(0);

   for i:=0 to 19 do begin
      k:=Lines1.Nodes.Count-i-1;
      if k>=0 then
         TGLLinesNode(Lines1.Nodes[k]).Color.Alpha:=0.95-i*0.05;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
   t : Single;
begin
   if colCount>0 then
      t:=colTotalTime*1000/colCount
   else t:=0;
	Caption:=Format('%.2f FPS - %.3f ms for collisions/frame',
                   [GLSceneViewer2.FramesPerSecond, t]);
	GLSceneViewer2.ResetPerformanceMonitor;
   colTotalTime:=0;
   colCount:=0;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   //If the ball gets stuck in a pattern, hit the reset button.
   Sphere2.Position.X:=0+random;
   Sphere2.Position.Y:=0+random;
   Sphere2.Position.Z:=0+random;

   Sphere2.Direction.X:=0+random;
   Sphere2.Direction.Y:=0+random;
   Sphere2.Direction.Z:=0+random;
end;

end.
