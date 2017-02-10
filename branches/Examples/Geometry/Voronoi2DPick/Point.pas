
constructor TGPoint.Create(x_,y_:extended;L:TList;C:TCanvas);
begin
  x:=x_;
  y:=y_;
  inherited Create(L,C);
  Color.Color:=$00FFFFFF;
  closeDist:=2;
end;

//------------------------------------------------------------------

function   TGPoint.Clone:TGraphObject;
var a:TGPoint;
begin
  a:=TGPoint.Create(x,y,nil,getCanvas);
  a.orig_index:=getOrigIndex;
  result:=a;
end;


//------------------------------------------------------------------

procedure   TGPoint.draw;
begin
  getcanvas.pixels[round(x),round(y)]:=color.Color;
end;

//------------------------------------------------------------------

procedure   TGPoint.clear;
begin
end;

//------------------------------------------------------------------

function    TGPoint.getX:extended;
begin
  result:=x;
end;

//------------------------------------------------------------------

function    TGPoint.getY:extended;
begin
  result:=y;
end;

//------------------------------------------------------------------

function    TGPoint.DistanceTo(p:TGPoint):extended;
begin
  result:=sqrt((p.x-x)*(p.x-x)+(p.y-y)*(p.y-y));
end;

//------------------------------------------------------------------

function    TGPoint.DistanceTo(x_,y_:extended):extended;
begin
  result:=sqrt((x_-x)*(x_-x)+(y_-y)*(y_-y));
end;

//------------------------------------------------------------------

procedure   TGPoint.MoveTo(x_,y_:extended);
begin
  x:=x_;
  y:=y_;
end;

//------------------------------------------------------------------

function    TGPoint.Match(p:TGPoint):boolean;
begin
  result:= (DistanceTo(p) <= CloseDist);
end;

//------------------------------------------------------------------

function    TGPoint.Match(x_,y_:extended):boolean;
begin
  result:= (DistanceTo(x_,y_) <= CloseDist);
end;

//------------------------------------------------------------------

function    TGPoint.Angle(p:TGPoint):extended;   // required for building the convex hull
begin
  result:=arcsin((p.x-x)/distanceto(p));
  if (p.x>=x) and (p.y>=y) then
      else
  if (p.x>=x) and (p.y<y) then
     result:=pi-result else
  if (p.x<x) and (p.y>=y) then
     result:=(pi+pi)+result else
  if (p.x<x) and (p.y<y) then
     result:=pi-result;

end;

//------------------------------------------------------------------

function    TGPoint.IsRightTurn(p1,p2:TGPoint):boolean;  // required for Graham scan
var a1,a2:extended;
begin

  a1:=angle(p1);
  a2:=angle(p2);
  a1:=a1-a2;
  if a1<0 then a1:=2*pi+a1;
  if a1>pi then result:=true else result:=false;
end;

//------------------------------------------------------------------

function    TGPoint.areCollinear(a,b:TGPoint):boolean;
begin
 result:= ((b.y-a.y)*(x-a.x)-(b.x-a.x)*(y-a.y))=0;
end;


//------------------------------------------------------------------

function    TGPoint.Bisector(p:TGPoint):TGLine;
var a:TGLine;
    sx,sy,dx,dy:extended;
begin
  sx:= (x+p.x)/2;
  sy:= (y+p.y)/2;
  dx:= p.x-x;
  dy:= p.y-y;
  a:=TGLine.Create(TGPoint.Create(sx-dy,sy+dx,nil,nil),TGPoint.Create(sx+dy,sy-dx,nil,nil),Vector,nil,getCanvas);
  a.bisectorOf[1]:=getOrigIndex;
  a.bisectorOf[2]:=p.getOrigIndex;
  result:=a;

end;

//------------------------------------------------------------------
// Got this one from the internet

function    TGPoint.CircleCenter(a,b:TGPoint):TGPoint;
var u,v,den:extended;
begin
    u := ((a.x-b.x)*(a.x+b.x) + (a.y-b.y)*(a.y+b.y)) / 2.0;
    v := ((b.x-x)*(b.x+x) + (b.y-y)*(b.y+y)) / 2.0;
    den := (a.x-b.x)*(b.y-y) - (b.x-x)*(a.y-b.y);
    result:=TGPoint.Create((u*(b.y-y)   - v*(a.y-b.y)) / den,
                           (v*(a.x-b.x) - u*(b.x-x)) / den,nil,getcanvas);

end;






