unit fzBSim;

interface

uses SysUtils, Classes, Windows, Graphics, BenTools;

const
   BALL = 3;   // Radius

type
   TBPoint = class
      Num         : integer;
      Selected    : boolean;
      Locked      : boolean;
      PosX        : double;
      PosY        : double;
      VelX        : double;
      VelY        : double;
      function    PointDis(Pt2: TBPoint): double;
   end;

   TBLink = class
      Pt1         : integer;  // Index in the PointList
      Pt2         : integer;
      LinkDis     : double;
      constructor Create;
   end;

   TPointList = class(TOwnList)
   end;

   TLinkList = class(TOwnList)
   end;

   TBounceSim = class
   protected
      procedure   UpdateObjects;
      procedure   UpdatePos(var Pos, Vel, Vel2: double; Min, Max: integer);
      procedure   Dampen(var VelX, VelY: double; vx, vy, Sgn: double);
   public
      Iter        : integer;
      GravX       : double;
      GravY       : double;
      BounceFric  : double;
      mx, my      : integer;     // Max X and Max Y boundary
      PointList   : TPointList;
      LinkList    : TLinkList;
      constructor Create;
      destructor  Destroy; override;
      procedure   Draw(c: TCanvas);
      procedure   RunIteration(c: TCanvas);
      function    NearestPoint(x, y: integer): TBPoint;
   end;

var
   AirFric : double;
   LinkStr : double;

implementation

// ******************************************************************
// TBPoint
// ******************************************************************

function TBPoint.PointDis(Pt2: TBPoint): double;
var
   xd, yd   : double;
begin
   xd := PosX - Pt2.PosX;
   yd := PosY - Pt2.PosY;
   Result := Sqrt(xd*xd + yd*yd);
end;


// ******************************************************************
// TBLink
// ******************************************************************

constructor TBLink.Create;
begin
   inherited;
end;

// ******************************************************************
// TBounceSim
// ******************************************************************

constructor TBounceSim.Create;
begin
   GravX       := 0;
   GravY       := 1;
   Iter        := 0;
   BounceFric  := 0.96;
   PointList   := TPointList.Create;
   LinkList    := TLinkList.Create;
end;

destructor TBounceSim.Destroy;
begin
   PointList.Free;
   LinkList.Free;
   inherited;
end;

procedure TBounceSim.RunIteration(c: TCanvas);
begin
   Iter := Iter + 1;
   UpdateObjects;
   Draw(c);
end;

procedure TBounceSim.UpdatePos(var Pos, Vel, Vel2: double; Min, Max: integer);
begin
   Vel := Vel * AirFric;

   if ((Pos + Vel) < Min) or ((Pos + Vel) > Max) then begin
      Vel := -Vel * BounceFric;
      Vel2 := Vel2 * BounceFric;
      if Pos < Min then Pos := Min;
      if Pos > Max then Pos := Max;
   end else
      Pos := Pos + Vel;
end;

procedure TBounceSim.Dampen(var VelX, VelY: double; vx, vy, Sgn: double);
begin
end;

procedure TBounceSim.UpdateObjects;
var
   i        : integer;
   l        : TBLink;
   p1, p2   : TBPoint;
   xd, yd   : double;
   Dis, Inv : double;
   f        : double;
   sgn      : double;
begin
   for i := 0 to LinkList.Count-1 do begin
      l  := LinkList[i];
      p1 := PointList[l.Pt1];
      p2 := PointList[l.Pt2];
      
      // Update Velocity from Links
      xd := p1.PosX - p2.PosX;
      yd := p1.PosY - p2.PosY;
      Dis := Sqrt(xd * xd + yd * yd);
      Inv := 1 / Dis;
      xd := xd * Inv;
      yd := yd * Inv;

      f := (Dis - l.LinkDis) * LinkStr;
      if f > 0 then sgn := 1 else sgn := -1;

      // Point 1 moves toward Point 2 with force f in the direction of V(xd,yd)
      // Point 2 moves toward Point 1 with force f in the direction of V(-xd,-yd)
      p1.VelX := p1.VelX - xd * f;
      p1.VelY := p1.VelY - yd * f;
      p2.VelX := p2.VelX + xd * f;
      p2.VelY := p2.VelY + yd * f;

      Dampen(p1.VelX, p1.VelY, -xd, -yd, sgn);
      Dampen(p2.VelX, p2.VelY, xd, yd, sgn);
   end;

   for i := 0 to PointList.Count-1 do begin
      p1 := PointList[i];

      // Update Velocity from Gravity
      p1.VelX := p1.VelX + GravX / 5;
      p1.VelY := p1.VelY + GravY / 5;

      if p1.Locked then begin
         p1.VelX := 0;
         p1.VelY := 0;
      end;

      UpdatePos(p1.PosX, p1.VelX, p1.VelY, 0 + BALL, mx-1-BALL);
      UpdatePos(p1.PosY, p1.VelY, p1.VelX, 0 + BALL, my-1-BALL);
   end;
end;


procedure TBounceSim.Draw(c: TCanvas);
var
   R        : TRect;
   i        : integer;
   l        : TBLink;
   p1, p2   : TBPoint;
   x, y     : integer;
begin
   c.Brush.Style := bsSolid;
   c.Brush.Color := clBtnFace;
   R := Rect(0, 0, mx, my);
   c.FillRect(R);

   for i := 0 to LinkList.Count-1 do begin
      l  := LinkList[i];
      p1 := PointList[l.Pt1];
      p2 := PointList[l.Pt2];
      c.MoveTo(Round(p1.PosX), Round(p1.PosY));
      c.LineTo(Round(p2.PosX), Round(p2.PosY));
   end;

   for i := 0 to PointList.Count-1 do begin
      p1 := PointList[i];
      x := Round(p1.PosX);
      y := Round(p1.PosY);
      if p1.Selected then
         c.Brush.Color := clRed
      else
         c.Brush.Color := clGreen;
      c.Ellipse(x-BALL, y-BALL, x+BALL, y+BALL);
   end;
end;

function TBounceSim.NearestPoint(x, y: integer): TBPoint;
var
   i        : integer;
   MinDis   : double;
   Dis      : double;
   p1       : TBPoint;
   xd,yd    : double;
begin
   Result := nil;
   MinDis := 1e30;

   for i := 0 to PointList.Count-1 do begin
      p1 := PointList[i];
      xd := x - p1.PosX;
      yd := y - p1.PosY;
      Dis := Sqrt(xd*xd + yd*yd);
      if Dis < MinDis then begin
         MinDis := Dis;
         Result := p1;
      end;
   end;
end;


initialization
   AirFric := 0.985;
   LinkStr := 0.1;
end.
