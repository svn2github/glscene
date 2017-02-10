function    getAvgX(L:TList):extended;
var minx,maxx,x:extended;
    z:integer;
begin
  minx:=66666;
  maxx:=-66666;
  for z:=0 to L.Count-1 do begin
    if assigned(L.Items[z]) then
       if TObject(L.Items[z]) is TGPoint then begin
          x:=TGPoint(L.Items[z]).getX;
          if x>maxx then maxx:=x;
          if x<minx then minx:=x;
       end;
  end;

  result:=(maxx+minx)/2;
end;

//---------------------------------------------------------------------


function next(i:integer;l:TList):integer;
begin
  inc(i);
  if i=l.Count then i:=0;
  result:=i;
end;

function prev(i:integer;l:TList):integer;
begin
  dec(i);
  if i<0 then i:=l.Count-1;
  result:=i;
end;




//---------------------------------------------------------------------


procedure GetVPoly(i:integer;var l:TList);
var z:integer;
begin
  if assigned(l) then begin
     l.Clear;
  end else
     l:=TList.Create;

  for z:=0 to LGlobal.Count-1 do
     if assigned(LGlobal.items[z]) then
        if TObject(LGlobal.items[z]) is TGLine then
           if (TGLine(LGlobal.items[z]).BisectorOf[1]=i) or (TGLine(LGlobal.items[z]).BisectorOf[2]=i) then
              TGLine(LGlobal.items[z]).CopyToList(l);
end;


//---------------------------------------------------------------------


function FindMinIPDistance(var slist:TList;var sindex:integer;var ip:TGPoint):extended;
var z:integer;
    p:TGPoint;
    min:extended;
begin
  p:=nil;
  ip:=nil;
  min:=66666;
  for z:=0 to slist.Count-1 do
    if TGLine(slist.items[z]).Intersect(Edge) then begin
       TGLine(slist.items[z]).GetCurrentIPoint(p);
       if (v.DistanceTo(p)<min) and (v.DistanceTo(p) > 1e-5) then begin  //must be with security distance
         min:=v.DistanceTo(p);
         ip:=p.clone as TGPoint;
         sindex:=z;
       end;
    end;
  p.Free;
  result:=min;
end;


//---------------------------------------------------------------------

function SortY(i1,i2:pointer):integer;begin if TGPoint(i1).getY > TGPoint(i2).getY then result:=1 else result:=-1;end;
function SortX(i1,i2:pointer):integer;begin if TGPoint(i1).getX > TGPoint(i2).getX then result:=1 else result:=-1;end;

procedure sort(L:TList;cf:TListSortCompare); // performs a bubble sort. TList.sort doesn't work for some reason.
var z,i:integer;
    p:pointer;
begin
 for z:=l.Count-2 downto 0 do
  for i:=0 to z do begin
    if cf(l.items[i],l.items[i+1]) < 0 then begin
      p:=l.items[i];l.items[i]:=l.items[i+1];l.Items[i+1]:=p; //exchange items
      TGraphObject(l.Items[i]).ReIndex(false,i);
      TGraphObject(l.Items[i+1]).ReIndex(false,i+1);
    end;
  end;

end;

//---------------------------------------------------------------------

procedure DiscardLines;
var a,b,c:integer;
    ip:TGPoint;
    l1,l2:TGLine;
label next;
begin
  l2:=nil;
  a:=0;
  while a<lglobal.count do begin
    if TObject(LGlobal.items[a]) is TGLine then begin
       l1:=LGlobal.items[a];

       for b:=0 to ConvexHull1.count-1 do
           if (l1.bisectorof[1]=TGPoint(ConvexHull1.items[b]).getOrigIndex) or (l1.bisectorof[2]=TGPoint(ConvexHull1.items[b]).getOrigIndex) then begin //l1 should be left from chain
              if l2<>nil then begin l2.p2.free;l2.free;end;
              l2:=TGLine.create(l1.p1,TGPoint.Create(l1.p1.getx-1,l1.p1.gety,nil,nil),OnePoint,nil,nil);
              for c:=0 to chain.count-1 do begin
                  if l2.intersect(chain.items[c]) then begin
                     ip:=nil;
                     l2.getCurrentIPoint(ip);
                     if l1.p1.distanceto(ip) > 1e-5 then begin
                          ip.free;
                          TGLine(LGlobal.items[a]).Delete(true);
                          dec(a);
                          goto next;
                     end;
                  end;
              end;
           end;

       for b:=0 to ConvexHull2.count-1 do
           if (l1.bisectorof[1]=TGPoint(ConvexHull2.items[b]).getOrigIndex) or (l1.bisectorof[2]=TGPoint(ConvexHull2.items[b]).getOrigIndex) then begin //l1 should be right from chain
              if l2<>nil then begin l2.p2.free;l2.free;end;
              l2:=TGLine.create(l1.p1,TGPoint.Create(l1.p1.getx+1,l1.p1.gety,nil,nil),OnePoint,nil,nil);
              for c:=0 to chain.count-1 do begin
                  if l2.intersect(chain.items[c]) then begin
                     ip:=nil;
                     l2.getCurrentIPoint(ip);
                     if l1.p1.distanceto(ip) > 1e-5 then begin
                          ip.free;
                          TGLine(LGlobal.items[a]).Delete(true);
                          dec(a);
                          goto next;
                     end;
                  end;
              end;
           end;


       next:
    end;

    inc(a);
  end; //while
end;

