{: Demo/sample/testbed for RayCastIntersect.<p>

   The RayCastIntersect aims at determining an as precise as possible collision
   detection between a ray and and object. With the intersection point is also
   returned the normal (which can be used for things like bouncing).<p>

   In this sample, this mechanism is used to implement a two-cents worth
   raytracer, simply by throwing rays for each point in a raster image (that
   is what raytracers do, but they go beyond throwing simple rays ;)),
   the intersection's normal and intersected  object's material is then used
   to calculate a basic lighting.<p>

   To calculate the raytraced/raycasted image, just it the "cast" button.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLMisc, StdCtrls, ExtCtrls, GLTexture, GLCadencer,
  GLWin32Viewer, ComCtrls;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Sphere1: TSphere;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    Image1: TImage;
    BUCast: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    GLCadencer1: TGLCadencer;
    Sphere2: TSphere;
    DummyCube1: TDummyCube;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Torus1: TTorus;
    Button1: TButton;
    TrackBar1: TTrackBar;
    procedure BUCastClick(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Geometry, Polynomials;

procedure TForm1.BUCastClick(Sender: TObject);
var
   o, v, vLight, light, iPoint, iNormal : TAffineVector;
   up, right, dir : TAffineVector;
   x, y, dx, dy : Integer;
   f, d : Single;
   color : TColor;
   iObj : TGLBaseSceneObject;
begin
   Screen.Cursor:=crHourGlass;
   // First we extract/prepare the vector we will use during our raycasting
   // the origin is the camera position, and factor was grossly adjusted so
   // that both view look grossly similar
   SetVector(o,   GLCamera1.AbsolutePosition);
   SetVector(dir, GLCamera1.AbsoluteDirection);
   SetVector(up,  GLCamera1.AbsoluteUp);
   SetVector(light, GLLightSource1.AbsolutePosition);
   right:=VectorCrossProduct(dir, up);
   f:=1/300;
   dx:=(Image1.Width div 2);
   dy:=(Image1.Height div 2);
   // Cover a square area
   for y:=0 to Image1.Height-1 do begin
      for x:=0 to Image1.Width-1 do begin
         // Calculate our ray vector
         v:=VectorCombine3(dir, right, up, 1, (x-dx)*f, (dy-y)*f);
         // ray vectors must be of unit length!
         NormalizeVector(v);
         // ray cast
         iObj:=GLScene1.RayCastIntersect(o, v, @iPoint, @iNormal);
         if Assigned(iObj) then begin
            // if something found, calculate vector to light source
            vLight:=VectorSubtract(light, iPoint);
            NormalizeVector(vLight);
            // color is given by the normal/lightsource vectors dot-product
            // and this intensity is composited with the object's diffuse color
            d:=VectorDotProduct(iNormal, vLight);
            if d<0 then d:=0;
            with (iObj as TGLCustomSceneObject).Material.FrontProperties do
               color:=ConvertColorVector(Diffuse.Color, d);
         end else color:=clGray;
         // plot our point
         Image1.Canvas.Pixels[x, y]:=color;
      end;
   end;
   Screen.Cursor:=crDefault;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   o, v, iPoint, iNormal : TAffineVector;
   up, right, dir : TAffineVector;
   dx, dy : Integer;
   f, d : Single;
   color : TColor;
   iObj : TGLBaseSceneObject;
begin
   SetVector(o,   GLCamera1.AbsolutePosition);
   SetVector(dir, GLCamera1.AbsoluteDirection);
   SetVector(up,  GLCamera1.AbsoluteUp);
   right:=VectorCrossProduct(dir, up);
   f:=1/300;
   dx:=(Image1.Width div 2);
   dy:=(Image1.Height div 2);
   // Calculate our ray vector
   v:=VectorCombine3(dir, right, up, 1, (x-dx)*f, (dy-y)*f);
   // ray vectors must be of unit length!
   NormalizeVector(v);
   iObj:=GLScene1.RayCastIntersect(o, v, @iPoint, @iNormal);
   ShowMessage(Format('%f %f %f', [iPoint[0], iPoint[1], iPoint[2]]));
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DummyCube1.TurnAngle:=newTime*50;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
   poly : array of Extended;
   roots : TComplexArray;
   trueSolutions, calcSolutions : array of Extended;
   mark : array of Boolean;
   a, b, c, d, delta, worstDelta : Extended;
   i, j, n : Integer;
begin
   SetLength(poly, 5);
   SetLength(trueSolutions, 4);
   SetLength(calcSolutions, 4);
   worstDelta:=0;
   for i:=0 to 50000 do begin
      a:=(Random*10-5); trueSolutions[0]:=a;
      b:=(Random*10-5); trueSolutions[1]:=b;
      c:=(Random*10-5); trueSolutions[2]:=c;
      d:=(Random*10-5); trueSolutions[3]:=d;
      // (x-a)*(x-b)*(x-c) = (x2 - (a+b).x + ab)*(x-c)
      //                   = x3 - (a+b).x2 + ab.x - c.x2 + c(a+b).x - abc
      //                   = x3 - (a+b+c).x2 +(ab+c(a+b)).x - abc          *(x-d)
      // x4 - (a+b+c).x3 + (ab+c(a+b)).x2 - abc.x - d.x3 + d(a+b+c).x2 - d(ab+c(a+b)).x + abcd
      // x4 - (a+b+c+d).x3 + (ab+c(a+b)+d(a+b+c)).x2 - (abc+d(ab+c(a+b))).x +abcd
      poly[0]:=a*b*c*d;
      poly[1]:=-(a*b*c+d*(a*b+c*(a+b)));
      poly[2]:=a*b+c*(a+b)+d*(a+b+c);
      poly[3]:=-(a+b+c+d);
      poly[4]:=1;
      roots:=qtcrt(@poly[0]);
      Assert(Length(roots)=4, 'Found '+IntToStr(Length(roots))+' roots');
      SetLength(mark, Length(roots));
      for j:=0 to High(roots) do begin
         Assert(Abs(roots[j].Imag)=0);
         calcSolutions[j]:=roots[j].Real;
         mark[j]:=False;
      end;
      SortArrayAscending(trueSolutions);
      SortArrayAscending(calcSolutions);
      for j:=0 to High(roots) do begin
         delta:=Abs(calcSolutions[j]-trueSolutions[j]);
         if delta>worstDelta then
            worstDelta:=delta;
      end;
   end;
   ShowMessage('Worst Delta : '+FloatToStr(worstDelta));
end;

end.
