procedure TGLine.initialize;
begin
  dx:=p2.getX-p1.getX;
  dy:=p2.getY-p1.getY;
  d:=sqrt(dx*dx+dy*dy);
//  m:=dy/dx;
  ex:=dx/d;
  ey:=dy/d;
end;

//------------------------------------------------------------------

constructor TGLine.Create(p1_,p2_:TGPoint;s:TGLineState;L:TList;C:TCanvas);
begin
  p1:=p1_;
  p2:=p2_;
  bisectorOf[1]:=-1;
  bisectorOf[2]:=-1;

  state:=s;
  inherited Create(L,C);
  Color.Color:=$00FFFFFF;
  initialize;
end;

//------------------------------------------------------------------

function   TGLine.Clone:TGraphObject;
begin
  result:=TGLine.Create(p1,p2,state,nil,getCanvas);
  result.orig_index:=getOrigIndex;
  TGLine(result).BisectorOf[1]:=BisectorOf[1];
  TGLine(result).BisectorOf[2]:=BisectorOf[2];

end;

//------------------------------------------------------------------

procedure   TGLine.draw;
var maxl:extended; //maximal line length possible in canvas, let canvas clip.
    p:TPoint;      //needed to set locations in canvas
begin

  maxl:=sqrt(sqr(getcanvas.ClipRect.Right)+sqr(getcanvas.ClipRect.Bottom));
  getcanvas.Pen.Color:=Color.Color;
  case state of
     TwoPoint:begin
                p.x:=round(p1.getX);
                p.y:=round(p1.getY);
                getcanvas.PenPos:=p;
                getcanvas.LineTo(round(p2.getX),round(p2.getY));
              end;
     OnePoint:begin
                p.x:=round(p1.getX);
                p.y:=round(p1.getY);
                getcanvas.PenPos:=p;
                getcanvas.LineTo(round(p1.getx+maxl*2*ex),round(p1.gety+maxl*2*ey));
              end;
     Vector  :begin
                p.x:=round(p1.getX-maxl*ex);
                p.y:=round(p1.getY-maxl*ey);
                getcanvas.PenPos:=p;
                getcanvas.LineTo(round(p1.getx+maxl*2*ex),round(p1.gety+maxl*2*ey));
              end;
  end;
end;

//------------------------------------------------------------------

procedure   TGLine.clear;
begin
end;

//------------------------------------------------------------------

function    TGLine.GetState:TGLineState;
begin
  result:=state;
end;

//------------------------------------------------------------------

function    TGLine.Intersect(ln:TGLine):boolean;
var ax,ay,divider:extended;
begin

  result:=false;

  divider:=ex*ln.ey-ey*ln.ex;

  if divider=0 then exit; //paralell

  r:=-(p1.getX*ey-p1.getY*ex+ex*ln.p1.getY-ey*ln.p1.getX)/divider;
  t:=-(p1.getX*ln.ey-p1.getY*ln.ex-ln.p1.getX*ln.ey+ln.p1.getY*ln.ex)/divider;

  ax:=p1.getX+t*ex;
  ay:=p1.getY+t*ey;


  if (state=Vector) and (ln.state=vector) then
     result:=true;

  if (state=Vector) and (ln.state=OnePoint) then
     if r>=0 then result:=true;

  if (state=Vector) and (ln.state=TwoPoint) then
     if (r>=0) and (r<=ln.d) then result:=true;


  if (state=OnePoint) and (ln.state=vector) then
     if (t>=0) then result:=true;

  if (state=OnePoint) and (ln.state=OnePoint) then
     if (t>=0) and (r>=0) then result:=true;

  if (state=OnePoint) and (ln.state=TwoPoint) then
     if (t>=0) and (r>=0) and (r<=ln.d) then result:=true;


  if (state=TwoPoint) and (ln.state=vector) then
     if (t>=0) and (t<=d) then result:=true;

  if (state=TwoPoint) and (ln.state=OnePoint) then
     if (t>=0) and (t<=d) and (r>=0) then result:=true;

  if (state=TwoPoint) and (ln.state=TwoPoint) then
     if (t>=0) and (t<=d) and (r>=0) and (r<=ln.d) then result:=true;


  if result then begin
     ix:=ax;
     iy:=ay;
  end;

end;

//------------------------------------------------------------------


procedure   TGLine.GetCurrentIPoint(var x,y:extended);  // copies ix and iy. only valid after intersect() call !
begin
  x:=ix;
  y:=iy;
end;

//------------------------------------------------------------------

procedure   TGLine.GetCurrentIPoint(var P:TGPoint);  // copies ix and iy. only valid after intersect() call !
begin
  if assigned(p) then p.free;
  p:=TGPoint.create(ix,iy,nil,getcanvas);
end;

//------------------------------------------------------------------

procedure   TGLine.CutRight(ln:TGLine);
begin
  if intersect(ln) then begin

     if state=vector then begin
        state:=OnePoint;
        p1.MoveTo(ix,iy);
        if ex>0 then begin
          ex:=-ex;
          ey:=-ey;
        end;
        p2.MoveTo(ix+ex,iy+ey);
     end else

     if state=OnePoint then begin
        if ex>0 then begin
          state:=TwoPoint;
          p2.MoveTo(ix,iy);
        end else begin
          //state unchanged ! stais OnePoint !!!
          p1.MoveTo(ix,iy);
          p2.MoveTo(ix+ex,iy+ey);
        end;
     end else

     if state=TwoPoint then begin
        state:=TwoPoint;
        if ex>0 then begin
          p2.MoveTo(ix,iy);
        end else begin
          p1.MoveTo(ix,iy);
        end;
     end;

     initialize;
  end;
end;

//------------------------------------------------------------------

procedure   TGLine.CutLeft(ln:TGLine);
begin
  if intersect(ln) then begin

     if state=vector then begin
        state:=OnePoint;
        p1.MoveTo(ix,iy);
        if ex<=0 then begin
          ex:=-ex;
          ey:=-ey;
        end;
        p2.MoveTo(ix+ex,iy+ey);

     end else



     if state=OnePoint then begin
        if ex<=0 then begin
          state:=TwoPoint;
          p2.MoveTo(ix,iy);
        end else begin
          //state unchanged ! stais OnePoint !!!
          p1.MoveTo(ix,iy);
          p2.MoveTo(ix+ex,iy+ey);
        end;
     end else



     if state=TwoPoint then begin
        if ex<=0 then begin
          p2.MoveTo(ix,iy);
        end else begin
          p1.MoveTo(ix,iy);
        end;
     end;

     initialize;

  end;
end;

//------------------------------------------------------------------

procedure   TGLine.CutBoth(ln:TGLine); // caller line must be the one on top
var a,b:TGline;
begin
 a:=(clone as TGLine);
 b:=(ln.clone as TGLine);
 if a.ey>0 then begin
    a.ex:=-a.ex;
    a.ey:=-a.ey;
 end;

 if b.ey>0 then begin
    b.ex:=-b.ex;
    b.ey:=-b.ey;
 end;

 if a.ex>=b.ex then begin
    if a.ex>0 then CutRight(ln) else CutLeft(ln);
    if b.ex<0 then ln.CutRight(a) else ln.CutLeft(a);
 end else begin
    if a.ex>0 then CutRight(ln) else CutLeft(ln);
    if b.ex>0 then ln.CutLeft(a) else ln.CutRight(a);
 end;

 a.free;
 b.free;

end;




