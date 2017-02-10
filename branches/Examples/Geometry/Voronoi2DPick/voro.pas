unit voro;
{
  Algorithm according to Shamos-Hoey and Lee that creates the voronoi diagram of a 2D point field in N log(N).
  Copyright (C) 2002 Christian Huettig
  Released: 08/08/2002
  Restrictions: The line-cut algorithm isn't perfect (5% failure, but indicated!)
  Huge problems with collinearity
  Inefficient line discard algorithm
  How to use: Just throw the main unit and all whats related to out.
  Please let me know if you were able to fix something.
  Contact: snakers@gmx.net
  First published: www.torry.net

  This source is free; you can redistribute it and/or modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
  This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along with this program;
  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

}

interface

uses
  System.Classes,
  Vcl.Graphics,
  System.SysUtils,
  Vcl.Forms,
  // Voro
  GraphObjects;

type
  TVoronoi = class(TObject)
  private
    Canvas: TCanvas;
    function Iterate(L: TList): TList; // The result is the convex hull !
  public
    LGlobal: TList; // Global List of Points (and later Lines)
    constructor Create(C: TCanvas; L: TList);
    procedure ClearLines; // Deletes all Lines from LGlobal
    procedure CalcVoronoi(ShowCHull, ShowTriangulation: boolean);
    // Builds the Voronoi Diagram into LGlobal
  end;

implementation

uses
  main;

constructor TVoronoi.Create(C: TCanvas; L: TList);
begin
  LGlobal := L;
  Canvas := C;
end;

// ---------------------------------------------------------------------

function getPointCount(L: TList): integer;
var
  z: integer;
begin
  result := 0;
  if L.Count = 0 then
    exit;
  for z := 0 to L.Count - 1 do
    if assigned(L.Items[z]) then
      if TObject(L.Items[z]) is TGPoint then
        inc(result);
end;



// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

function TVoronoi.Iterate(L: TList): TList;
// integrates the voronoi diagram from a given point list L into LGlobal, and returns the convex hull
var
  iterateList1, iterateList2, temp: TList;
  z, i, i1, i2, points: integer;
  avgX, aex, aey, bex, bey: extended;
  ConvexHull1, ConvexHull2: TList;
  chain, el, er: TList; // el,er represents the voronoi polygon of pl/pr
  Edge: TGLine;
  v, pl, pr, ip1, ip2: TGPoint;
  mina, minb, maxa, a: extended;
  i1maxx, i2minx, i1max, i1min, i2max, i2min: integer;
  // indexes for the new convex hull

label done, abort;

{$I service.pas}   // modularized the algorithm to keep it clear


// ---------------------------------------------------------------------------------------------

begin
  points := getPointCount(L);

  if points = 0 then
  begin
    raise ERangeError.Create('Received 0 point list in iteration!');
    exit;
  end;

  result := TList.Create;

  if points = 1 then
  begin
    for z := 0 to L.Count - 1 do
      if assigned(L.Items[z]) then
        if TObject(L.Items[z]) is TGPoint then
        begin
          TGPoint(L.Items[z]).MoveToList(result);
          exit;
        end;

    raise ERangeError.Create('Found 0 points instead of 1!');
    exit;

  end;

  if points = 3 then
  begin // collinear check, VERY incomplete.
    if TGPoint(L.Items[0]).areCollinear(L.Items[1], L.Items[2]) then
    begin
      if TGPoint(L.Items[0]).getx = TGPoint(L.Items[1]).getx then
      begin
        Sort(L, @SortY);
      end
      else if TGPoint(L.Items[0]).gety = TGPoint(L.Items[1]).gety then
      begin
        Sort(L, @SortX);
      end
      else
        Sort(L, @SortY);
      // integrate bisectors
      TGPoint(L.Items[0]).bisector(L.Items[1]).MoveToList(LGlobal);
      TGPoint(L.Items[1]).bisector(L.Items[2]).MoveToList(LGlobal);
      // build chull
      TGPoint(L.Items[0]).MoveToList(result);
      TGPoint(L.Items[2]).MoveToList(result);
      exit;
    end;
  end;

  iterateList1 := TList.Create;
  iterateList2 := TList.Create;
  avgX := getavgX(L);


  // divide all points into 2 seperate lists according to avgX

  for z := 0 to L.Count - 1 do
    if assigned(L.Items[z]) then
      if TObject(L.Items[z]) is TGPoint then
        if TGPoint(L.Items[z]).getx < avgX then
          TGPoint(L.Items[z]).MoveToList(iterateList1)
        else
          TGPoint(L.Items[z]).MoveToList(iterateList2);

  // for the case that all points have the same x value. we can't iterate an empty list

  if iterateList1.Count = 0 then
    TGPoint(iterateList2.Items[iterateList2.Count - 1])
      .MoveToList(iterateList1);
  if iterateList2.Count = 0 then
    TGPoint(iterateList1.Items[iterateList1.Count - 1])
      .MoveToList(iterateList2);

  // now the iteration

  ConvexHull1 := Iterate(iterateList1);
  ConvexHull2 := Iterate(iterateList2);


  // the hard part...

  chain := TList.Create;

  //
  // constructing new convex hull according to the Preparata-Hong algorithm
  //

  maxa := -66666;
  i1maxx := -1;
  for z := 0 to ConvexHull1.Count - 1 do
    if TGPoint(ConvexHull1.Items[z]).getx >= maxa then
    begin
      i1maxx := z;
      maxa := TGPoint(ConvexHull1.Items[z]).getx;
    end;

  mina := 66666;
  i2minx := -1;
  for z := 0 to ConvexHull2.Count - 1 do
    if TGPoint(ConvexHull2.Items[z]).getx <= mina then
    begin
      i2minx := z;
      mina := TGPoint(ConvexHull2.Items[z]).getx;
    end;

  // find upper supporting line

  i1 := i1maxx;
  i2 := i2minx;

  while (TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[i2]) >
    TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[next(i2, ConvexHull2)
    ])) or (TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[i1]) <
    TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[prev(i1,
    ConvexHull1)])) do
  begin

    while TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[i2]) >
      TGPoint(ConvexHull1.Items[i1])
      .Angle(ConvexHull2.Items[next(i2, ConvexHull2)]) do
      i2 := next(i2, ConvexHull2);

    while TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[i1]) <
      TGPoint(ConvexHull2.Items[i2])
      .Angle(ConvexHull1.Items[prev(i1, ConvexHull1)]) do
      i1 := prev(i1, ConvexHull1);

  end;

  i1max := i1;
  i2max := i2;


  // find lower supporting line

  i1 := i1maxx;
  i2 := i2minx;

  while (TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[i2]) <
    TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[prev(i2, ConvexHull2)
    ])) or (TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[i1]) >
    TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[next(i1,
    ConvexHull1)])) do
  begin

    while TGPoint(ConvexHull1.Items[i1]).Angle(ConvexHull2.Items[i2]) <
      TGPoint(ConvexHull1.Items[i1])
      .Angle(ConvexHull2.Items[prev(i2, ConvexHull2)]) do
      i2 := prev(i2, ConvexHull2);

    while TGPoint(ConvexHull2.Items[i2]).Angle(ConvexHull1.Items[i1]) >
      TGPoint(ConvexHull2.Items[i2])
      .Angle(ConvexHull1.Items[next(i1, ConvexHull1)]) do
      i1 := next(i1, ConvexHull1);

  end;

  i1min := i1;
  i2min := i2;


  //
  // constructing the dividing chain
  //

  el := nil;
  er := nil;

  TGPoint(ConvexHull1.Items[i1max]).bisector(ConvexHull2.Items[i2max])
    .MoveToList(chain); // initial ray
  Edge := chain.Items[0];
  if Edge.ey > 0 then // put v into a "conveniant large ordinate"
    v := TGPoint.Create(Edge.p2.getx + 6000 * Edge.ex,
      Edge.p2.gety + 6000 * Edge.ey, nil, nil)
  else
    v := TGPoint.Create(Edge.p2.getx - 6000 * Edge.ex, Edge.p2.gety - 6000 * Edge.ey,
      nil, nil);

  pl := ConvexHull1.Items[i1max];
  pr := ConvexHull2.Items[i2max];

  repeat

    GetVPoly(pl.GetOrigIndex, el);
    i1 := 0;
    GetVPoly(pr.GetOrigIndex, er);
    i2 := 0;

    if (el.Count = 0) and (er.Count = 0) then
    // only one bisector, can skip procedure
      goto done;

    ip1 := nil;
    ip2 := nil;
    mina := FindMinIPDistance(el, i1, ip1);
    minb := FindMinIPDistance(er, i2, ip2);

    // if mina=minb then raise ERangeError.Create('Wrong Cut');
    if mina = minb then
    begin // the bad part. some cut went the wrong way, but to keep it running we skip the procedure.
      form1.Label1.Visible := true;
      goto abort;
    end;

    if mina < minb then
    begin
      aex := TGLine(el.Items[i1]).ex;
      aey := TGLine(el.Items[i1]).ey;
      bex := Edge.ex;
      bey := Edge.ey;
      if aey < 0 then
        aex := -aex;
      if bey < 0 then
        bex := -bex;

      if ((aex < bex) and (aex >= 0)) or ((aex > bex) and (aex < 0)) then
        TGLine(el.Items[i1]).CutLeft(Edge)
      else
        TGLine(el.Items[i1]).CutRight(Edge);

      v.Free;
      v := ip1.clone as TGPoint;
      if TGLine(el.Items[i1]).BisectorOf[1] = pl.GetOrigIndex then
        pl := LGlobal.Items[TGLine(el.Items[i1]).BisectorOf[2]]
      else
        pl := LGlobal.Items[TGLine(el.Items[i1]).BisectorOf[1]];

      pl.bisector(pr).MoveToList(chain);

    end
    else
    begin
      aex := TGLine(er.Items[i2]).ex;
      aey := TGLine(er.Items[i2]).ey;
      bex := Edge.ex;
      bey := Edge.ey;
      if aey < 0 then
        aex := -aex;
      if bey < 0 then
        bex := -bex;

      if ((aex < bex) and (aex >= 0)) or ((aex > bex) and (aex < 0)) then
        TGLine(er.Items[i2]).CutRight(Edge)
      else
        TGLine(er.Items[i2]).CutLeft(Edge);

      v.Free;
      v := ip2.clone as TGPoint;
      if TGLine(er.Items[i2]).BisectorOf[1] = pr.GetOrigIndex then
        pr := LGlobal.Items[TGLine(er.Items[i2]).BisectorOf[2]]
      else
        pr := LGlobal.Items[TGLine(er.Items[i2]).BisectorOf[1]];

      pr.bisector(pl).MoveToList(chain);

    end;
    if ip1 <> nil then
      ip1.Free;
    if ip2 <> nil then
      ip2.Free;

    Edge.CutBoth(chain.last);

  done:

    Edge := chain.last;

  until ((Edge.BisectorOf[1] = TGPoint(ConvexHull1.Items[i1min]).GetOrigIndex) and
    (Edge.BisectorOf[2] = TGPoint(ConvexHull2.Items[i2min]).GetOrigIndex)) or
    ((Edge.BisectorOf[2] = TGPoint(ConvexHull1.Items[i1min]).GetOrigIndex) and
    (Edge.BisectorOf[1] = TGPoint(ConvexHull2.Items[i2min]).GetOrigIndex));

abort:

  DiscardLines; // doesn't work perfect, and is very inefficent

  for z := 0 to chain.Count - 1 do // move chain to LGlobal
    TGLine(chain.Items[z]).MoveToList(LGlobal);



  // building new chull

  i := i1min;
  TGraphObject(ConvexHull1.Items[i]).MoveToList(result);
  while i <> i1max do
  begin
    i := next(i, ConvexHull1);
    TGraphObject(ConvexHull1.Items[i]).MoveToList(result);
  end;

  i := i2max;
  TGraphObject(ConvexHull2.Items[i]).MoveToList(result);
  while i <> i2min do
  begin
    i := next(i, ConvexHull2);
    TGraphObject(ConvexHull2.Items[i]).MoveToList(result);
  end;

  // finished

  chain.Free;

  ConvexHull1.Free;
  ConvexHull2.Free;

  iterateList2.Free;
  iterateList1.Free;

end;

procedure TVoronoi.ClearLines; // Deletes all Lines from LGlobal
var
  z: integer;
begin
  for z := 0 to LGlobal.Count - 1 do
    if assigned(LGlobal.Items[z]) then
      if TObject(LGlobal.Items[z]) is TGLine then
      begin
        TGLine(LGlobal.Items[z]).Free;
        LGlobal.Items[z] := nil;
      end;

  LGlobal.Pack;
  for z := 0 to LGlobal.Count - 1 do
    if TObject(LGlobal.Items[z]) is TGPoint then
      TGPoint(LGlobal.Items[z]).ReIndex(true);

end;

procedure TVoronoi.CalcVoronoi; // Builds the Voronoi Diagram into LGlobal
var
  iterateList, chull: TList;
  L: TGLine;
  z: integer;
begin

  form1.Label1.Visible := false;
  application.ProcessMessages;

  // must clone all points out of LGlobal because all points will be moved and killed during iteration
  if getPointCount(LGlobal) < 2 then
    exit;

  iterateList := TList.Create;
  for z := 0 to LGlobal.Count - 1 do
    if assigned(LGlobal.Items[z]) then
      if TObject(LGlobal.Items[z]) is TGPoint then
        TGPoint(LGlobal.Items[z]).CloneToList(iterateList);

  // -------------------------------------------------
  chull := Iterate(iterateList);
  // -------------------------------------------------

  if ShowTriangulation then
  begin
    for z := 0 to LGlobal.Count - 1 do
      if TObject(LGlobal.Items[z]) is TGLine then
      begin
        L := LGlobal.Items[z];
        if L.BisectorOf[1] > -1 then
        begin
          TGLine.Create(LGlobal.Items[L.BisectorOf[1]],
            LGlobal.Items[L.BisectorOf[2]], TwoPoint, LGlobal, Canvas)
            .Color.Color := clgreen;
        end;
      end;
  end;

  if ShowCHull then
  begin
    for z := 0 to chull.Count - 2 do
    begin
      TGLine.Create(chull.Items[z], chull.Items[z + 1], TwoPoint, LGlobal,
        Canvas).Color.Color := clYellow;
    end;
    TGLine.Create(chull.Items[chull.Count - 1], chull.Items[0], TwoPoint,
      LGlobal, Canvas).Color.Color := clYellow;
  end;

  iterateList.Free;
end;

end.
