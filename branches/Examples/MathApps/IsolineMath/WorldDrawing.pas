unit WorldDrawing;
(*
  TWorldDrawing
  Object for drawing in world coordinates
  Now the object powering TMathImage based on Renate Schaaf's source
 *)

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Graphics;

type
  MathFloat = single; //double; extended;

  PFloatPoint = ^TFloatpoint;
  TFloatpoint = record
    x, y: MathFloat;
  end;

  TFloatPointArray = array of TFloatpoint;

  PD3FloatPoint = ^TD3FloatPoint;
  TD3FloatPoint = record
    x, y, z: MathFloat;
  end;

  POldFloatPoint = ^TOldFloatpoint;
  TOldFloatpoint = record
    x, y: MathFloat;
    Next: POldFloatPoint;
  end;


  POldD3FloatPoint = ^TOldD3FloatPoint;
  TOldD3FloatPoint = record
    x, y, z: MathFloat;
    Next: POldD3FloatPoint;
  end;

  Td3FloatPointArray = array of TD3FloatPoint;
  TColorArray = array of TColor;
  TFloatarray = array of MathFloat;


  TD3FloatPointers = record
    px, py, pz: ^MathFloat;
  end;

  TD3Triangle = record
    p, q, r: PD3FloatPoint;
    n: TD3FloatPoint; //(normal)
    FillColor, WireColor: ^TColor;
  end;

  TD3TriangleArray = array of TD3Triangle;

  TD3SurfaceCell = record
    p, q, r, s: PD3FloatPoint;
    FillColor, WireColor: ^TColor;
  end;

  TCube = record
     { TODO : Change that TCube }
    x1, y1, z1, x2, y2, z2: MathFloat;
    p1, p2, p3, p4, p5, p6, p7, p8: TD3FloatPoint;
    FillColor, WireColor: TColor;
  end;

  Td3FloatPointerArray = array of TD3FloatPointers;

  P4Cell = ^T4Cell;
  T4Cell = record
    Vertex: array[0..3] of TPoint;
    dist: MathFloat;
    FillColor, WireColor: TColor;
  end;

  P3Cell = ^T3Cell;
  T3Cell = record
    Vertex: array[0..2] of TPoint;
    dist, BDiff, BSpec: MathFloat;
    FillColor, WireColor: TColor;
  end;

  TPointArray = array of TPoint;

  Td3LineSegment = record
    p, q: PD3FloatPoint;
    N1, N2: TD3FloatPoint; //principal and secondary normal
    dist: MathFloat;
    Color: TColor;
    Width: MathFloat;
  end;

  Td3LineSegmentArray = array of Td3LineSegment;

  TNormalKind = (nkPrincipal, nkSecondary);

  TLightSource = class
  private
    fSourcePoint: TD3FloatPoint;
    fViewAngles: TFloatpoint;
    fyrot, fzrot, fdist: MathFloat;
    fFixed: Boolean;
    procedure SetYRot(Value: Integer);
    procedure SetZRot(Value: Integer);
    function GetYRot: Integer;
    function GetZRot: Integer;
    procedure SetDist(Value: MathFloat);
    procedure SetViewAngles(Value: TFloatpoint);
    procedure InitSourcePoint;
  public
    property YRotation: Integer read GetYRot write SetYRot;
    property ZRotation: Integer read GetZRot write SetZRot;
    property dist: MathFloat read fdist write SetDist;
    property fixed: Boolean read fFixed write fFixed;
    property ViewAngles: TFloatpoint read fViewAngles write SetViewAngles;
    property SourcePoint: TD3FloatPoint read fSourcePoint;
  end;



  { Object for drawing in world coordinates on any canvas. Use of this
  object is more advanced than using <See class=TMathImage>, because you need
  to set the world, and screen dimensions, and have to update them as needed
  if your area resizes. Second, there is no exception handling whatsoever done
  in this object. Third, you need to do your own clipping when using axes.
  The AxesRect property can be used }
  TWorldDrawing = class
  private
    fwidth, fHeight: Integer;    //Screen dimensions
    fd2x1, fd2xw, fd2y1, fd2yw: MathFloat;  //D2- worldsize
    fd2Axes: Boolean;    //Leave space for axes?

    fmaxxtw, fmaxytw, fmaxth: Integer;
    fd3ar: Boolean;    //true aspectratio in d3?
    fd3x1, fd3xw, fd3y1, fd3yw, fd3z1, fd3zw: MathFloat;   //D3-worldsize

    fd3alpha, fd3vd, fd3vdinv: MathFloat; //D3-lens: opening angle, viewdist
    fd3zr, fd3yr: MathFloat;   //D3 viewpoint angles
    fd3ViewPoint: TD3FloatPoint;   //D3 current viewpoint
    fd3ViewAngles: TFloatpoint;
    fd3L1, fd3L2: TLightSource;
    fd3xScale, fd3yScale, fd3zScale: MathFloat;  //D3 axes scalings
    fClipRect: TRect;

    //helper variables
    ax, bx, ay, by, axinv, ayinv, x1Snap, x2Snap, y1Snap, y2Snap: MathFloat;
    basex, basey, basez, frontx, fronty, frontz: MathFloat;
    arad, tana, thetaz, thetay, sinz, siny, cosz, cosy, coszinv,
      sinyinv, cosyinv,
      axd3, ayd3, azd3, axd3Inv, ayd3Inv, azd3Inv, bxd3, byd3, bzd3, ap, bxp, byp: MathFloat;
    rightz, righty: MathFloat;
    fDefaultFillColor, fDefaultWireColor: TColor;

    procedure scalar(xb, yb, zb: MathFloat; var r: MathFloat);
    procedure Project(xb, yb, zb: MathFloat; var u, v: MathFloat);
    procedure MakeScalings;
    procedure dist(xb, yb, zb: MathFloat; var r: MathFloat);
    procedure FindBase(var i1, i2, i3: Integer);
    procedure InitWorld;
    procedure DrawOneAxis(ACanvas: TCanvas; x1, y1, z1, x2, y2, z2: MathFloat; Arrows: Boolean);
    procedure Block(x, y, z: MathFloat; var xb, yb, zb: MathFloat);
    procedure ScaleVector(const v: TD3FloatPoint; var w: TD3FloatPoint);
    procedure ScaleNormal(const v: TD3FloatPoint; var w: TD3FloatPoint);
    procedure d3DrawBaseAxes(ACanvas: TCanvas; xLabel, yLabel,
      zLabel: string; xTicks, yTicks, zTicks: byte; yx, zx, xy, zy, xz,
      yz: MathFloat; Arrows: Boolean = True);
  protected
    (*Low level routines that require the pointers to have been set up *)
    procedure d3ResetWorld;
    procedure GetBrightness(const p, n: TD3FloatPoint; var BDiff, BSpec: MathFloat);
    procedure Draw4Cells(ACanvas: TCanvas; const Cells: array of T4Cell);
    procedure d3DrawTriangles(ACanvas: TCanvas; const Triangles: array of TD3Triangle);
    procedure d3DrawSurfaceCells(ACanvas: TCanvas; const SurfaceCells: array of TD3SurfaceCell);
    procedure d3DrawLitTriangles(ACanvas: TCanvas; const Triangles: array of TD3Triangle; diffuse, focussed: MathFloat);
    function DoorInDoorOut(c, xp, yp, xq, yq, xr, yr, p, q, r: MathFloat;
      var x1, y1, x2, y2: MathFloat): Boolean;
    procedure DrawLevelLines(ACanvas: TCanvas; const Triangles: array of TD3Triangle; Level: MathFloat);
    procedure DrawLevelLine(ACanvas: TCanvas; Triangle: TD3Triangle; Level: MathFloat);
    procedure DrawProjection(ACanvas: TCanvas; Triangle: TD3Triangle);
    procedure DrawProjections(ACanvas: TCanvas; const Triangles: array of TD3Triangle);
    procedure GetIlluminatedLinesegments(AColor: TColor; diffuse, focussed, RightIntensity: MathFloat;
      z1, z2, y1, y2: Integer; d1, d2: MathFloat; fixed: Boolean; var l: Td3LineSegmentArray);
    procedure DrawLineSegments(ACanvas: TCanvas; l: Td3LineSegmentArray);
  public
    property AxesClipRect: TRect read fClipRect;
    constructor Create;
    //general d2-Stuff
    { Use this first thing to tell the object the pixel dimensions for the drawing area }
    procedure SetScreen(AWidth, AHeight: Integer);
    { Set the bounds for your world coordinates. x1, y1 are the lower bounds,
    x2,y2 the upper ones. This TWorldDrawing does no exception handling, so
    you need to make sure that x1<x2 and y1<y2. The canvas this is intended
    for, needs to be passed, so the right amount of space can be left for
    axes drawing depending on the canvas's font. If you change the font, you need
    to reset the world }
    procedure SetWorld(ACanvas: TCanvas; x1, y1, x2, y2: MathFloat);
    procedure ResetWorld(ACanvas: TCanvas);
    procedure Setd2Axes(ACanvas: TCanvas; Value: Boolean);
    function Windowx(x: MathFloat): Integer;
    function Windowy(y: MathFloat): Integer;
    procedure WorldToScreen(const x, y: MathFloat; var xs, Ys: Integer);
    function WorldX(xs: longint): MathFloat;
    function WorldY(Ys: longint): MathFloat;
    procedure Convert(const src: TFloatpoint; var dest: TPoint);
    function Norm(x, y: MathFloat): MathFloat;
    procedure DrawPoint(ACanvas: TCanvas; x, y: MathFloat);
    procedure MoveToPoint(ACanvas: TCanvas; x, y: MathFloat);
    procedure DrawLine(ACanvas: TCanvas; x1, y1, x2, y2: MathFloat);
    procedure DrawLineTo(ACanvas: TCanvas; x, y: MathFloat);
    procedure DrawEllipse(ACanvas: TCanvas; x1, y1, x2, y2: MathFloat);
    procedure DrawCircle(ACanvas: TCanvas; xCenter, yCenter: MathFloat; PixRadius: Integer);
    procedure DrawRectangle(ACanvas: TCanvas; x1, y1, x2, y2: MathFloat);
    procedure DrawAxes(ACanvas: TCanvas; xLabel, yLabel: string;
      AxesColor: TColor; Arrows: Boolean = True);
    procedure DrawZeroLines(ACanvas: TCanvas; AColor: TColor);
    procedure DrawVector(ACanvas: TCanvas; x, y, A, b: MathFloat);
    procedure DrawPolyline(ACanvas: TCanvas; const FloatPointArray: array of TFloatpoint; PointCount: Integer);
    procedure DrawPolygon(ACanvas: TCanvas; const FloatPointArray: array of TFloatpoint; PointCount: Integer);
    procedure DrawPolyPolyline(ACanvas: TCanvas; const GraphList: array of TFloatPointArray);
    //general d3 stuff
    procedure d3SetWorld(x1, y1, z1, x2, y2, z2: MathFloat; AspectRatio: Boolean);
    procedure d3SetViewPoint(vd, alpha, yr, zr: MathFloat);
    procedure d3SetScales(xScale, yScale, zScale: MathFloat);
    procedure d3Window(x, y, z: MathFloat; var xs, Ys: longint);
    procedure PseudoD3World(xs, Ys: longint; var x, y, z: MathFloat);
    procedure d3Moveto(ACanvas: TCanvas; x, y, z: MathFloat);
    procedure d3DrawPoint(ACanvas: TCanvas; x, y, z: MathFloat);
    procedure d3DrawLine(ACanvas: TCanvas; x1, y1, z1, x2, y2, z2: MathFloat);
    procedure d3DrawLineto(ACanvas: TCanvas; x, y, z: MathFloat);
    procedure d3DrawAxes(ACanvas: TCanvas; xLabel, yLabel, zLabel: string;
      xTicks, yTicks, zTicks, xPos, yPos, zPos: byte; Arrows: Boolean = True);
    procedure d3DrawBestAxes(ACanvas: TCanvas; xLabel, yLabel, zLabel: string;
      xTicks, yTicks, zTicks: byte; Arrows: Boolean = True);
    procedure d3DrawCustomAxes(ACanvas: TCanvas;
      xmin, ymin, zmin, xmax, ymax, zmax: MathFloat;
      xLabel, yLabel, zLabel: string);
    procedure d3DrawWorldbox(ACanvas: TCanvas);
    procedure d3DrawBox(ACanvas: TCanvas; x1, y1, z1, x2, y2,
      z2: MathFloat);
    procedure d3DrawFullWorldBox(ACanvas: TCanvas);
    procedure d3drawZeroCross(ACanvas: TCanvas);
    procedure d3Polyline(ACanvas: TCanvas; const FloatPointArray: array of TD3FloatPoint; PointCount: Integer);
    procedure d3LitPolyLine(ACanvas: TCanvas;
      const FloatPointArray: array of TD3FloatPoint; PointCount: Integer;
      NormalKind: TNormalKind; ambient, directed: MathFloat; zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);
    procedure d3PolyPolyline(ACanvas: TCanvas; const GraphList: array of Td3FloatPointArray);

    //Surface stuff
    procedure d3DistanceToViewer(x, y, z: MathFloat; var r: MathFloat);
    procedure d3DrawSurface(ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray; fill: Boolean);
    procedure d3DrawLitSurface
      (ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray; diffuse, focussed: MathFloat);
    procedure d3DrawColorSurface(ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray;
      Colors: array of TColorArray);
    procedure d3DrawHeightCubes(ACanvas: TCanvas; const HeightArray: array of TFloatarray;
      const Colors: array of TColorArray);
    procedure d3DrawLitHeightCubes(ACanvas: TCanvas; const HeightArray: array of TFloatarray;
      const Colors: array of TColorArray; diffuse, focussed: MathFloat);
    procedure d3DrawLitLevelSurface
      (ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray;
      const Levels: array of MathFloat; const Colors: array of TColor;
      diffuse, focussed: MathFloat);
    procedure d3DrawCubes(ACanvas: TCanvas; const Cubes: array of TCube; fill: Boolean);
    procedure d3DrawLitCubes(ACanvas: TCanvas; const Cubes: array of TCube; diffuse, focussed: MathFloat);

    //Level stuff
    procedure DrawLevelCurves(ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray; Level: MathFloat);
    procedure DrawFilledLevelCurves(ACanvas: TCanvas; const SurfArray: array of Td3FloatPointArray;
      const Levels: array of MathFloat; const Colors: array of TColor);

    //properties
    property d2x1: MathFloat read fd2x1;
    property d2y1: MathFloat read fd2y1;
    property d2xw: MathFloat read fd2xw;
    property d2yw: MathFloat read fd2yw;
    property d2Axes: Boolean read fd2Axes;
    property d2x1Snap: MathFloat read x1Snap;
    property d2x2Snap: MathFloat read x2Snap;
    property d2y1Snap: MathFloat read y1Snap;
    property d2y2Snap: MathFloat read y2Snap;
    property d3ar: Boolean read fd3ar;
    property d3x1: MathFloat read fd3x1;
    property d3xw: MathFloat read fd3xw;
    property d3y1: MathFloat read fd3y1;
    property d3yw: MathFloat read fd3yw;
    property d3z1: MathFloat read fd3z1;
    property d3zw: MathFloat read fd3zw;
    property d3alpha: MathFloat read fd3alpha;
    property d3vd: MathFloat read fd3vd;
    property d3zr: MathFloat read fd3zr;
    property d3yr: MathFloat read fd3yr;
    property d3Xscale: MathFloat read fd3xScale;
    property d3Yscale: MathFloat read fd3yScale;
    property d3Zscale: MathFloat read fd3zScale;
    property LightSource1: TLightSource read fd3L1;
    property LightSource2: TLightSource read fd3L1;
  end;

procedure D3FloatPoint(x, y, z: MathFloat; var p: TD3FloatPoint);
procedure FloatPoint(x, y: MathFloat; var r: TFloatpoint);
procedure CrossProduct(x1, y1, z1, x2, y2, z2: MathFloat; var u1, u2, u3: MathFloat);
procedure GetLineSegments(const f: array of TD3FloatPoint;
  aCount: Integer; NormalKind: TNormalKind; var l: Td3LineSegmentArray);


//========================================================================
implementation
//========================================================================

const piInv = 2 / pi;


procedure D3FloatPoint(x, y, z: MathFloat; var p: TD3FloatPoint);
begin
  p.x := x;
  p.y := y;
  p.z := z;
end;

procedure FloatPoint(x, y: MathFloat; var r: TFloatpoint);
begin
  r.x := x;
  r.y := y;
end;

procedure d3Norm(const p: TD3FloatPoint; var r: MathFloat);
begin
  r := sqrt(sqr(p.x) + sqr(p.y) + sqr(p.z));
end;

{ TWorldDrawing }

constructor TWorldDrawing.Create;
begin
  inherited;
  fd3x1 := 0; fd3xw := 1; fd3y1 := 0; fd3yw := 1; fd3z1 := 0; fd3zw := 1;
  fd3vd := 6.4; fd3alpha := 6; fd3yr := 0; fd3zr := 0;
  fd3ar := True; fd3xScale := 1; fd3yScale := 1; fd3zScale := 1;
  fd2Axes := False;
  fd2x1 := 0; fd2xw := 1; fd2y1 := 0; fd2yw := 1;
  //all properties for which a 0 value makes sense must be set to 0
  //because 0 is never stored in the dfm file
  fwidth := 30; fHeight := 30; fmaxxtw := 20; fmaxytw := 20; fmaxth := 10;
  x1Snap := 0; x2Snap := 1; y1Snap := 0; y2Snap := 1;
  fd3L1 := TLightSource.Create;
  fd3L2 := TLightSource.Create;
  fd3L1.YRotation := 0;
  fd3L1.ZRotation := 30;
  fd3L1.dist := 2 * fd3vd;
  fd3L2.YRotation := 0;
  fd3L2.ZRotation := -60;
  fd3L2.dist := 2 * fd3vd;
  MakeScalings;
end;

procedure TWorldDrawing.SetScreen(AWidth, AHeight: Integer);
begin
  if (AWidth <> fwidth) or (AHeight <> fHeight) then
  begin
    fwidth := AWidth;
    fHeight := AHeight;
  end;
end;

procedure TWorldDrawing.Setd2Axes(ACanvas: TCanvas; Value: Boolean);
begin
  fd2Axes := Value;
  SetWorld(ACanvas, fd2x1, fd2y1, fd2x1 + fd2xw, fd2y1 + fd2yw);
end;

procedure TWorldDrawing.d3SetWorld(x1, y1, z1, x2, y2, z2: MathFloat; AspectRatio: Boolean);
begin
  //do exception handling in TMathImage
  fd3x1 := x1;
  fd3xw := x2 - x1;
  fd3y1 := y1;
  fd3yw := y2 - y1;
  fd3z1 := z1;
  fd3zw := z2 - z1;
  fd3ar := AspectRatio;
  InitWorld;
end;

procedure TWorldDrawing.d3Window(x, y, z: MathFloat; var xs, Ys: Integer);
var
  xb, yb, zb, u, v, Temp: MathFloat;
begin
  Block(x, y, z, xb, yb, zb);
  Project(xb, yb, zb, u, v);
  Temp := bxp + ap * u;
  if Temp < -22000 then xs := -22000 else if Temp > 22000 then xs := 22000 else
    xs := round(Temp);
  Temp := byp - ap * v;
  if Temp < -22000 then Ys := -22000 else if Temp > 22000 then Ys := 22000 else
    Ys := round(Temp);
end;

procedure TWorldDrawing.dist(xb, yb, zb: MathFloat; var r: MathFloat);
begin
  scalar(xb, yb, zb, r);
  r := fd3vd - r;
end;

procedure TWorldDrawing.Block(x, y, z: MathFloat; var xb, yb, zb: MathFloat);
begin
  xb := bxd3 + axd3 * x;
  yb := byd3 + ayd3 * y;
  zb := bzd3 + azd3 * z;
end;


procedure TWorldDrawing.d3DistanceToViewer(x, y, z: MathFloat;
//Note: this is the square distance, all we need to sort!
  var r: MathFloat);
var
  xb, yb, zb: MathFloat;
begin
  Block(x, y, z, xb, yb, zb);
  r := sqr(fd3vd * siny * sinz - yb) +
    sqr(fd3vd * cosy - zb) + sqr(fd3vd * siny * cosz - xb);
end;

procedure TWorldDrawing.FindBase(var i1, i2, i3: Integer);
var
  dmax, d: MathFloat; i, j, k: Integer;
begin
  i1 := -1; i2 := -1; i3 := -1;
  dmax := 0;
  for i := 0 to 1 do
    for j := 0 to 1 do
      for k := 0 to 1 do
      begin
        dist(-1 + 2 * i, -1 + 2 * j, -1 + 2 * k, d);
        dmax := max(dmax, d);
        if d = dmax then
        begin
          i1 := -1 + 2 * i; i2 := -1 + 2 * j; i3 := -1 + 2 * k;
        end;
      end;
end;

procedure TWorldDrawing.InitWorld;
var
  i1, i2, i3: Integer;
begin
  if fd3vd < 0.0001 then fd3vd := 0.0001;
  if fd3alpha > 179 then fd3alpha := 179;
  if fd3alpha < 0.01 then fd3alpha := 0.01;
  MakeScalings;
  FindBase(i1, i2, i3);
  if i1 = -1 then basex := fd3x1 else basex := fd3x1 + fd3xw;
  if i2 = -1 then basey := fd3y1 else basey := fd3y1 + fd3yw;
  if i3 = -1 then basez := fd3z1 else basez := fd3z1 + fd3zw;
  if i1 = 1 then frontx := fd3x1 else frontx := fd3x1 + fd3xw;
  if i2 = 1 then fronty := fd3y1 else fronty := fd3y1 + fd3yw;
  if i3 = 1 then frontz := fd3z1 else frontz := fd3z1 + fd3zw;
end;

procedure TWorldDrawing.Project(xb, yb, zb: MathFloat; var u,
  v: MathFloat);
var
  scal, d: MathFloat;
begin
  scalar(xb, yb, zb, scal);
  d := fd3vd - scal;
  if righty <> 0 then
    v := (zb - scal * cosy) * sinyinv
  else
    v := -(yb * sinz + xb * cosz) * cosyinv;
  if rightz <> 0 then
    u := (yb + sinz * (v * cosy - scal * siny)) * coszinv
  else
    u := -xb * sinz;
  if d <= 0 then d := 1.0E-10;
  d := 1 / d;
  u := u * d;
  v := v * d;
end;



procedure TWorldDrawing.PseudoD3World(xs, Ys: Integer; var x, y,
  z: MathFloat);
var
  u, v, xb, yb, zb: MathFloat;
begin
  u := (xs - bxp) / ap * fd3vd;
  v := (byp - Ys) / ap * fd3vd;
  zb := siny * v;
  yb := cosz * u - sinz * cosy * v;
  xb := -sinz * u - cosy * cosz * v;
  x := (xb - bxd3) / axd3;
  y := (yb - byd3) / ayd3;
  z := (zb - bzd3) / azd3;
end;

procedure TWorldDrawing.scalar(xb, yb, zb: MathFloat; var r: MathFloat);
begin
  r := yb * sinz * siny + zb * cosy + xb * siny * cosz;
end;

procedure TWorldDrawing.MakeScalings;
var
  A: MathFloat;
begin
  fd3vdinv := 1 / fd3vd;
  thetaz := 1 / 180 * pi * fd3zr;
  thetay := 1 / 180 * pi * fd3yr;
  arad := 1 / 360 * pi * fd3alpha;
  sinz := sin(thetaz); cosz := cos(thetaz);
  siny := sin(thetay); cosy := cos(thetay);
  if siny <> 0 then
    sinyinv := 1 / siny;
  if cosy <> 0 then
    cosyinv := 1 / cosy;
  if cosz <> 0 then
    coszinv := 1 / cosz;
  tana := sin(arad) / cos(arad);
  rightz := (fd3zr + 90) - 180 * round(1 / 180 * (fd3zr + 90.0));
  righty := fd3yr - 180 * round(1 / 180 * fd3yr);
  axd3 := fd3xw;
  ayd3 := fd3yw;
  azd3 := fd3zw;
  if not fd3ar then
  begin
    axd3 := 2 / axd3;
    ayd3 := 2 / ayd3;
    azd3 := 2 / azd3;
  end else
  begin
    A := 2 / max(max(fd3xScale * axd3, fd3yScale * ayd3), fd3zScale * azd3);
    ayd3 := fd3yScale * A; axd3 := fd3xScale * A; azd3 := fd3zScale * A;
  end;
  bxd3 := -axd3 * (fd3x1 + 0.5 * fd3xw);
  byd3 := -ayd3 * (fd3y1 + 0.5 * fd3yw);
  bzd3 := -azd3 * (fd3z1 + 0.5 * fd3zw);
  ap := min(fHeight, fwidth) * 0.5 / tana * fd3vdinv;
  bxp := fwidth * 0.5; byp := fHeight * 0.5;
  axd3Inv := 1 / axd3;
  ayd3Inv := 1 / ayd3;
  azd3Inv := 1 / azd3;
  D3FloatPoint(fd3vd * cosz * siny, fd3vd * sinz * siny, fd3vd * cosy, fd3ViewPoint);
  FloatPoint(thetaz, thetay, fd3ViewAngles);
  fd3L1.ViewAngles := fd3ViewAngles;
  fd3L2.ViewAngles := fd3ViewAngles;
end;


procedure TWorldDrawing.d3DrawAxes(ACanvas: TCanvas; xLabel, yLabel,
  zLabel: string; xTicks, yTicks, zTicks, xPos, yPos, zPos: byte; Arrows: Boolean = True);
var
  yx, zx, xy, zy, xz, yz: MathFloat;


begin {******* drawd3axes ******}
  yx := fd3y1; zx := fd3z1;
  xy := fd3x1; zy := fd3z1;
  xz := fd3x1; yz := fd3y1;
  case xPos of
    0: begin yx := fd3y1; zx := fd3z1; end;
    1: begin yx := fd3y1; zx := fd3z1 + fd3zw; end;
    2: begin yx := fd3y1 + fd3yw; zx := fd3z1; end;
    3: begin yx := fd3y1 + fd3yw; zx := fd3z1 + fd3zw; end;
  end;
  case yPos of
    0: begin xy := fd3x1; zy := fd3z1; end;
    1: begin xy := fd3x1; zy := fd3z1 + fd3zw; end;
    2: begin xy := fd3x1 + fd3xw; zy := fd3z1; end;
    3: begin xy := fd3x1 + fd3xw; zy := fd3z1 + fd3zw; end;
  end;
  case zPos of
    0: begin xz := fd3x1; yz := fd3y1; end;
    1: begin xz := fd3x1; yz := fd3y1 + fd3yw; end;
    2: begin xz := fd3x1 + fd3xw; yz := fd3y1; end;
    3: begin xz := fd3x1 + fd3xw; yz := fd3y1 + fd3yw; end;
  end;
  d3DrawBaseAxes(ACanvas, xLabel, yLabel, zLabel, xTicks, yTicks, zTicks, yx, zx, xy, zy, xz, yz, Arrows);
end;

procedure TWorldDrawing.d3DrawBaseAxes(ACanvas: TCanvas; xLabel, yLabel,
  zLabel: string; xTicks, yTicks, zTicks: byte; yx, zx, xy, zy, xz, yz: MathFloat; Arrows: Boolean = True);
var
  xs, Ys, i, iStart, Ticks: longint;
  SaveBrush: TBrush;
  SavePen: TPen;
  t: string;
  iTemp, Tick, log, invlog, invTick: MathFloat;


begin {******* drawd3axes ******}
  SavePen := TPen.Create;
  SaveBrush := TBrush.Create;
  SavePen.assign(ACanvas.Pen);
  SaveBrush.assign(ACanvas.Brush);
  ACanvas.Brush.Style := bsClear;
  DrawOneAxis(ACanvas, fd3x1, yx, zx, fd3x1 + fd3xw, yx, zx, Arrows);
  d3Window(fd3x1 + 0.5 * fd3xw, yx, zx, xs, Ys);
  with ACanvas do
    TextOut(xs - TextWIdth(xLabel) div 2, Ys -TextHeight(xLabel)-6, xLabel);
  DrawOneAxis(ACanvas, xy, fd3y1, zy, xy, fd3y1 + fd3yw, zy, Arrows);
  d3Window(xy, fd3y1 + 0.5 * fd3yw, zy, xs, Ys);
  ACanvas.TextOut(xs - ACanvas.TextWIdth(yLabel) div 2, Ys-ACanvas.TextHeight(yLabel)-6, yLabel);
  DrawOneAxis(ACanvas, xz, yz, fd3z1, xz, yz, fd3z1 + fd3zw, Arrows);
  d3Window(xz, yz, fd3z1 + 0.5 * fd3zw, xs, Ys);
  log := ln(10); invlog := 1 / log;
  with ACanvas do
    TextOut(xs +6, Ys, zLabel);
  if xTicks > 0 then
  begin
    iTemp := ln(1 / 8 * abs(fd3xw)) * invlog;
    if iTemp >= 0 then
      i := trunc(iTemp) else i := trunc(iTemp) - 1;
    Tick := exp(i * log);
    with ACanvas.Font do Size := Size - 1;
    if Tick > 0 then
    begin
      invTick := 1 / Tick;
      iStart := round(fd3x1 * invTick);
      while iStart * Tick < fd3x1 do inc(iStart);
      Ticks := round(fd3xw * invTick) div xTicks;
      i := iStart;
      if Ticks <= 500 then
        repeat
          d3Window(i * Tick, yx, zx, xs, Ys);
          t := FloatToStrf(i * Tick, ffgeneral, 3, 3);
          with ACanvas do
            if i > iStart then
            begin
              TextOut(xs - (TextWIdth(t) div 2), Ys + 6, t);
              MoveTo(xs, Ys);
              LineTo(xs, Ys + 6);
            end;
          i := i + Ticks;
        until i * Tick >= fd3x1 + fd3xw;
    end;
    with ACanvas.Font do Size := Size + 1;
  end;
  if yTicks > 0 then
  begin
    iTemp := ln(1 / 8 * abs(fd3yw)) * invlog;
    if iTemp >= 0 then
      i := trunc(iTemp) else i := trunc(iTemp) - 1;
    Tick := exp(i * log);
    with ACanvas.Font do Size := Size - 1;
    if Tick > 0 then
    begin
      invTick := 1 / Tick;
      iStart := round(fd3y1 * invTick);
      while iStart * Tick < fd3y1 do inc(iStart);
      Ticks := round(fd3yw * invTick) div yTicks;
      i := iStart;
      if Ticks <= 500 then
        repeat
          d3Window(xy, i * Tick, zy, xs, Ys);
          t := FloatToStrf(i * Tick, ffgeneral, 3, 3);
          with ACanvas do
            if i > iStart then
            begin
              TextOut(xs - (TextWIdth(t) div 2), Ys + 6, t);
              MoveTo(xs, Ys);
              LineTo(xs, Ys + 6);
            end;
          i := i + Ticks;
        until i * Tick >= fd3y1 + fd3yw;
    end;
    with ACanvas.Font do Size := Size + 1;
  end;
  if zTicks > 0 then
  begin
    iTemp := ln(1 / 8 * abs(fd3zw)) * invlog;
    if iTemp >= 0 then
      i := trunc(iTemp) else i := trunc(iTemp) - 1;
    Tick := exp(i * log);
    with ACanvas.Font do Size := Size - 1;
    if Tick > 0 then
    begin
      invTick := 1 / Tick;
      iStart := round(fd3z1 * invTick);
      while iStart * Tick <= fd3z1 do inc(iStart);
      Ticks := round(fd3zw * invTick) div zTicks;
      i := iStart;
      if Ticks <= 500 then
        repeat
          d3Window(xz, yz, i * Tick, xs, Ys);
          t := FloatToStrf(i * Tick, ffgeneral, 3, 3);
          with ACanvas do
          begin
            TextOut(xs - TextWIdth(t) - 6, Ys - (TextHeight(t) div 2), t);
            MoveTo(xs, Ys);
            LineTo(xs - 6, Ys);
          end;
          i := i + Ticks;
        until i * Tick >= fd3z1 + fd3zw;
    end;
    with ACanvas.Font do Size := Size + 1;
  end;
  ACanvas.Brush.assign(SaveBrush);
  ACanvas.Pen.assign(SavePen);
  SaveBrush.Free;
  SavePen.Free;
end;


procedure TWorldDrawing.d3DrawBox(ACanvas: TCanvas; x1, y1, z1, x2, y2,
  z2: MathFloat);
  procedure MakePoint(x, y, z: MathFloat; var p: TPoint);
  var
    xs, Ys: longint;
  begin
    d3Window(x, y, z, xs, Ys);
    p := Point(xs, Ys);
  end;
var
  p11, p12, p13, p14, p21, p22, p23, p24: TPoint;
begin
  MakePoint(x1, y1, z1, p11);
  MakePoint(x2, y1, z1, p12);
  MakePoint(x2, y2, z1, p13);
  MakePoint(x1, y2, z1, p14);
  MakePoint(x1, y1, z2, p21);
  MakePoint(x2, y1, z2, p22);
  MakePoint(x2, y2, z2, p23);
  MakePoint(x1, y2, z2, p24);
  with ACanvas do
  begin
    Polyline([p11, p12, p13, p14, p11]);
    Polyline([p21, p22, p23, p24, p21]);
    MoveTo(p11.x, p11.y); LineTo(p21.x, p21.y);
    MoveTo(p12.x, p12.y); LineTo(p22.x, p22.y);
    MoveTo(p13.x, p13.y); LineTo(p23.x, p23.y);
    MoveTo(p14.x, p14.y); LineTo(p24.x, p24.y);
  end;
end;



procedure TWorldDrawing.d3DrawCustomAxes(ACanvas: TCanvas; xmin, ymin,
  zmin, xmax, ymax, zmax: MathFloat; xLabel, yLabel, zLabel: string);
var xs, Ys: Integer;

begin
  DrawOneAxis(ACanvas, xmin, ymin, zmin, xmax, ymin, zmin, True);
  DrawOneAxis(ACanvas, xmin, ymin, zmin, xmin, ymax, zmin, True);
  DrawOneAxis(ACanvas, xmin, ymin, zmin, xmin, ymin, zmax, True);
  d3Window(xmax, ymin, zmin, xs, Ys);
  with ACanvas do
    TextOut(xs - TextWIdth(xLabel) - 3, Ys + 6, xLabel);
  d3Window(xmin, ymax, zmin, xs, Ys);
  ACanvas.TextOut(xs + 3, Ys + 6, yLabel);
  d3Window(xmin, ymin, zmax, xs, Ys);
  with ACanvas do
    TextOut(xs, Ys - 6 - TextHeight(zLabel), zLabel);
end;

procedure TWorldDrawing.d3DrawFullWorldBox(ACanvas: TCanvas);
begin
  d3DrawBox(ACanvas, fd3x1, fd3y1, fd3z1, fd3x1 + fd3xw, fd3y1 + fd3yw, fd3z1 + fd3zw);
end;

procedure TWorldDrawing.d3DrawLine(ACanvas: TCanvas; x1, y1, z1, x2, y2,
  z2: MathFloat);
var
  Points: array[0..2] of TPoint;
begin
  d3Window(x1, y1, z1, Points[0].x, Points[0].y);
  d3Window(x2, y2, z2, Points[1].x, Points[1].y);
  Points[2] := Points[0];
  ACanvas.Polyline(Points);
end;

procedure TWorldDrawing.d3DrawLineto(ACanvas: TCanvas; x, y, z: MathFloat);
var
  xs, Ys: longint;
begin
  d3Window(x, y, z, xs, Ys);
  ACanvas.LineTo(xs, Ys);
end;

procedure TWorldDrawing.d3drawZeroCross(ACanvas: TCanvas);
begin
  if 0 >= fd3x1 then if 0 <= fd3x1 + fd3xw then if 0 >= fd3z1 then
        if 0 <= fd3z1 + fd3zw then
          d3DrawLine(ACanvas, 0, fd3y1, 0, 0, fd3y1 + fd3yw, 0);
  if 0 >= fd3z1 then if 0 <= fd3z1 + fd3zw then if 0 >= fd3y1 then
        if 0 <= fd3y1 + fd3yw then
          d3DrawLine(ACanvas, fd3x1, 0, 0, fd3x1 + fd3xw, 0, 0);
  if 0 >= fd3y1 then if 0 <= fd3y1 + fd3yw then if 0 >= fd3x1 then
        if 0 <= fd3x1 + fd3xw then
          d3DrawLine(ACanvas, 0, 0, fd3z1, 0, 0, fd3z1 + fd3zw);
end;

procedure TWorldDrawing.d3Moveto(ACanvas: TCanvas; x, y, z: MathFloat);
var
  xs, Ys: longint;
begin
  d3Window(x, y, z, xs, Ys);
  ACanvas.MoveTo(xs, Ys);
end;

procedure TWorldDrawing.d3DrawPoint(ACanvas: TCanvas; x, y, z: MathFloat);
var
  xs, Ys: longint;
begin
  d3Window(x, y, z, xs, Ys);
  ACanvas.Pixels[xs, Ys] := ACanvas.Pen.Color;
end;

procedure TWorldDrawing.d3DrawWorldbox(ACanvas: TCanvas);
begin
  d3DrawLine(ACanvas, basex, basey, basez, frontx, basey, basez);
  d3DrawLine(ACanvas, basex, basey, basez, basex, fronty, basez);
  d3DrawLine(ACanvas, basex, basey, basez, basex, basey, frontz);
  d3DrawLine(ACanvas, basex, fronty, basez, frontx, fronty, basez);
  d3DrawLine(ACanvas, basex, fronty, basez, basex, fronty, frontz);
  d3DrawLine(ACanvas, basex, basey, frontz, frontx, basey, frontz);
  d3DrawLine(ACanvas, basex, basey, frontz, basex, fronty, frontz);
  d3DrawLine(ACanvas, frontx, basey, basez, frontx, fronty, basez);
  d3DrawLine(ACanvas, frontx, basey, basez, frontx, basey, frontz);
end;

procedure TWorldDrawing.d3Polyline(ACanvas: TCanvas;
  const FloatPointArray: array of TD3FloatPoint; PointCount: Integer);
var
  i: Integer; p: array of TPoint;
begin
  //Do exception checking in TMathImage
  SetLength(p, PointCount);
  for i := 0 to PointCount - 1 do
    with FloatPointArray[i] do
    begin
      d3Window(x, y, z, p[i].x, p[i].y);
    end;
  ACanvas.Polyline(p);
end;

function GetIlluminatedColor(AColor: TColor; BDiff, BSpec: MathFloat): TColor; forward;



procedure SortLineSegments(var AArray: Td3LineSegmentArray);

  procedure QuickSort(iLo, iHi:
    Integer);
  var
    Lo, Hi: Integer; Mid: Td3LineSegment; Temp: Td3LineSegment;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := AArray[(Lo + Hi) div 2];
    repeat
      while AArray[Lo].dist > Mid.dist do inc(Lo);
      while AArray[Hi].dist < Mid.dist do dec(Hi);
      if Lo <= Hi then
      begin
        Temp := AArray[Lo];
        AArray[Lo] := AArray[Hi];
        AArray[Hi] := Temp;
        inc(Lo);
        dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(iLo, Hi);
    if Lo < iHi then QuickSort(Lo, iHi);
  end;

begin
  QuickSort(0, High(AArray));
end;

procedure GetLineSegments(const f: array of TD3FloatPoint; aCount: Integer; NormalKind: TNormalKind; var l: Td3LineSegmentArray);
var i: Integer;
  x1, y1, z1, x2, y2, z2, X3, Y3, z3, u, v: MathFloat;
begin
  SetLength(l, (aCount - 1) div 2);
  for i := 0 to High(l) do
  begin
    l[i].p := @f[2 * i];
    l[i].q := @f[2 * i + 2];
    with f[2 * i] do
    begin
      x1 := x; y1 := y; z1 := z;
    end;
    with f[2 * i + 1] do
    begin
      x2 := x; y2 := y; z2 := z;
    end;
    with f[2 * i + 2] do
    begin
      X3 := x; Y3 := y; z3 := z;
    end;
    x1 := x2 - x1; y1 := y2 - y1; z1 := z2 - z1;
    if (x1 = 0) and (y1 = 0) and (z1 = 0) then
    begin
      D3FloatPoint(0, 0, 0, l[i].N1);
      D3FloatPoint(0, 0, 0, l[i].N2);
      Continue;
    end;
    x2 := (X3 - x2 - x1); y2 := (Y3 - y2 - y1); z2 := (z3 - z2 - z1);
    x1 := 0.5 * (l[i].q.x - l[i].p.x); y1 := 0.5 * (l[i].q.y - l[i].p.y); z1 := 0.5 * (l[i].q.z - l[i].p.z);
    u := sqr(x1) + sqr(y1) + sqr(z1);
    v := x1 * x2 + y1 * y2 + z1 * z2;
    X3 := u * x2 - v * x1;
    Y3 := u * y2 - v * y1;
    z3 := u * z2 - v * z1;
    u := 1 / sqr(u);
    X3 := u * X3; Y3 := u * Y3; z3 := u * z3;
    if (X3 = 0) and (Y3 = 0) and (z3 = 0) then
      if i = 0 then
      begin
        if x1 = 0 then
        begin
          X3 := 0; Y3 := z1; z3 := -y1;
        end
        else
        begin
          X3 := y1; Y3 := -x1; z3 := 0;
        end;
      end
      else
      begin
        l[i].N1 := l[i - 1].N1;
        l[i].N2 := l[i - 1].N2;
        Continue;
      end;
    //if NormalKind = nkSecondary then
    CrossProduct(x1, y1, z1, X3, Y3, z3, l[i].N2.x, l[i].N2.y, l[i].N2.z);
    l[i].N1.x := X3; l[i].N1.y := Y3; l[i].N1.z := z3;
  end;
 { l[aCount - 2].p := @f[aCount - 2];
  l[aCount - 2].q := @f[aCount - 1];
  with l[aCount - 2] do
  begin
    n1 := l[aCount - 3].n1;
    n2 := l[aCount - 3].n2;
  end; }
end;

procedure TWorldDrawing.GetIlluminatedLinesegments(AColor: TColor; diffuse, focussed, RightIntensity: MathFloat; z1, z2, y1, y2: Integer; d1, d2: MathFloat; fixed: Boolean; var l: Td3LineSegmentArray);
var i: Integer; bp, bs, bp1, bp2, bs1, bs2, bpSpec, bsSpec, b, BSpec, x, y, z, Norm1, Norm2,
  Norml1, Norml2, Normc, Normh1, Normh2, CamScalar1, CamScalar2: MathFloat;
  ns1, ns2, l1, l2, l1loc, l2loc, cam, h1, h2: TD3FloatPoint;
//this is a hodgepodge, so far. Needs to be organized  
begin
  fd3L1.fixed := fixed;
  fd3L2.fixed := fixed;
  fd3L1.dist := d1;
  fd3L1.YRotation := y1;
  fd3L1.ZRotation := z1;
  fd3L2.dist := d2;
  fd3L2.YRotation := y2;
  fd3L2.ZRotation := z2;
  l1 := fd3L1.SourcePoint;
  l2 := fd3L2.SourcePoint;
  bpSpec := 0;
  bsSpec := 0;
  for i := 0 to High(l) do
  begin
    with l[i] do
    begin
      Block(0.5 * (p.x + q.x), 0.5 * (p.y + q.y), 0.5 * (p.z + q.z), x, y, z);
      D3FloatPoint(l1.x - x, l1.y - y, l1.z - z, l1loc);
      D3FloatPoint(l2.x - x, l2.y - y, l2.z - z, l2loc);
      D3FloatPoint(fd3ViewPoint.x - x, fd3ViewPoint.y - y, fd3ViewPoint.z - z, cam);
      dist := sqr(cam.x) + sqr(cam.y) + sqr(cam.z); //squaredist
      ScaleNormal(N1, ns1);
      ScaleNormal(N2, ns2);

      CamScalar2 := cam.x * ns2.x + cam.y * ns2.y + cam.z * ns2.z;
      CamScalar1 := cam.x * ns1.x + cam.y * ns1.y + cam.z * ns1.z;
      d3Norm(l1loc, Norml1);
      d3Norm(l2loc, Norml2);
      Norml1 := 1 / Norml1;
      Norml2 := 1 / Norml2;
      d3Norm(ns1, Norm1);
      d3Norm(ns2, Norm2);
      if Norm1 = 0 then
      begin
        bp := 0;
       // Width := 0;
      end
      else
      begin
        bp1 := l1loc.x * ns1.x + l1loc.y * ns1.y + l1loc.z * ns1.z;
        bp1 := bp1 / Norm1 * Norml1;
        //bp1: light from light source 1 in principal direction
        if bp1 > 0 then //shines on direction in curvature
          if CamScalar1 > 0 then
            bp1 := bp1 * (1 - 0.1 * Norm1)
          else bp1 := 0;
        if bp1 < 0 then
          if CamScalar1 < 0 then
            bp1 := -bp1 * (1 + 0.1 * Norm1)
          else bp1 := 0;
        //Width := b;
        bp1 := bp1 * 49 * Norml1 * Norml1; //scaled by Light dist
        bp2 := l2loc.x * ns1.x + l2loc.y * ns1.y + l2loc.z * ns1.z;
        bp2 := bp2 / Norm1 * Norml2;
        if bp2 > 0 then //shines on direction in curvature
          if CamScalar1 > 0
            then
            bp2 := bp2 * (1 - 0.1 * Norm1)
          else
            bp2 := 0;
        if bp2 < 0 then
          if CamScalar1 < 0 then
            bp2 := -bp2 * (1 + 0.1 * Norm1)
          else
            bp2 := 0;
        bp2 := bp2 * 49 * Norml2 * Norml2;
        bp := RightIntensity * bp1 + (1 - RightIntensity) * bp2;
        Normc := sqrt(dist);
        Normc := 1 / Normc;
        D3FloatPoint(Normc * cam.x + l1loc.x * Norml1, Normc * cam.y + l1loc.y * Norml1, Normc * cam.z + l1loc.z * Norml1, h1);
        d3Norm(h1, Normh1);
        bp1 := ns1.x * h1.x + ns1.y * h1.y + ns1.z * h1.z;
        if bp1 * CamScalar1 > 0 then
          bp1 := abs(bp1) / Norm1 / Normh1
        else
          bp1 := 0;
        if bp1 > 0 then
        begin
          if bp1 > 1 then
            bp1 := 1;
          bp1 := exp(40 * ln(bp1));
          bp1 := bp1 * 55 * Norml1 * Norml1;
        end;
        D3FloatPoint(Normc * cam.x + l2loc.x * Norml2, Normc * cam.y + l2loc.y * Norml2, Normc * cam.z + l2loc.z * Norml2, h2);
        d3Norm(h2, Normh2);
        bp2 := ns1.x * h2.x + ns1.y * h2.y + ns1.z * h2.z;
        if CamScalar1 * bp2 > 0 then
          bp2 := abs(bp2) / Norm1 / Normh2
        else
          bp2 := 0;
        if bp2 > 0 then
        begin
          if bp2 > 1 then bp2 := 1;
          bp2 := exp(40 * ln(bp2));
          bp2 := bp2 * 55 * Norml2 * Norml2;
        end;
        bpSpec := RightIntensity * bp1 + (1 - RightIntensity) * bp2;
      end;
      if Norm2 = 0 then
      begin
        bs := 0;
       // Width := 0;
      end
      else
      begin
        Norm2 := 1 / Norm2;
        bs1 := l1loc.x * ns2.x + l1loc.y * ns2.y + l1loc.z * ns2.z;
        bs1 := bs1 * Norm2 * Norml1; //bs1: light from light source 2 in secondary direction
        if bs1 * CamScalar2 > 0 then
          bs1 := abs(bs1) * 49 * Norml1 * Norml1
        else
          bs1 := 0;
        bs2 := l2loc.x * ns2.x + l2loc.y * ns2.y + l2loc.z * ns2.z;
        if CamScalar2 * bs2 > 0 then
          bs2 := abs(bs2) * Norm2 * Norml2
        else
          bs2 := 0;
        bs2 := bs2 * 49 * Norml2 * Norml2;
        bs := RightIntensity * bs1 + (1 - RightIntensity) * bs2;
        bs1 := ns2.x * h1.x + ns2.y * h1.y + ns2.z * h1.z;
        if CamScalar2 * bs1 > 0 then
          bs1 := abs(bs1) * Norm2 / Normh1
        else
          bs1 := 0;
        if bs1 <> 0 then
        begin
          if bs1 > 1 then
            bs1 := 1;
          bs1 := exp(60 * ln(abs(bs1)));
          bs1 := bs1 * 55 * Norml1 * Norml1;
        end;
        bs2 := ns2.x * h2.x + ns2.y * h2.y + ns2.z * h2.z;
        if bs2 * CamScalar2 > 0 then
          bs2 := abs(bs2) * Norm2 / Normh2
        else
          bs2 := 0;
        if bs2 <> 0 then
        begin
          if bs2 > 1 then
            bs2 := 1;
          bs2 := exp(60 * ln(abs(bs2)));
          bs2 := bs2 * 55 * Norml2 * Norml2;
        end;
        bsSpec := RightIntensity * bs1 + (1 - RightIntensity) * bs2;
      end;
      bs := (diffuse + focussed * bs);
      bp := (diffuse + focussed * bp);
      b := bp + bs;
      b := b * 0.5 * sqr(fd3vd) / dist;
      BSpec := 0.7 * bpSpec + 0.3 * bsSpec;
      BSpec := focussed * 60 * BSpec*sqr(fd3vd)/dist;
      Color := GetIlluminatedColor(AColor, b, BSpec);
      //Width := 1;
    end;
  end;
end;

procedure TWorldDrawing.ScaleVector(const v: TD3FloatPoint; var w: TD3FloatPoint);
begin
  w.x := axd3 * v.x;
  w.y := ayd3 * v.y;
  w.z := azd3 * v.z;
end;

procedure TWorldDrawing.ScaleNormal(const v: TD3FloatPoint; var w: TD3FloatPoint);
begin
  w.x := axd3Inv * v.x;
  w.y := ayd3Inv * v.y;
  w.z := azd3Inv * v.z;
end;



procedure TWorldDrawing.d3LitPolyLine(ACanvas: TCanvas;
  const FloatPointArray: array of TD3FloatPoint; PointCount: Integer;
  NormalKind: TNormalKind;
  ambient, directed: MathFloat; zrot1, zrot2, yrot1, yrot2: Integer; dist1, dist2: MathFloat; fixed: Boolean);
var
  savecolor: TColor;
  lines: Td3LineSegmentArray;
  RightIntensity: MathFloat;
begin
  savecolor := ACanvas.Pen.Color;
  GetLineSegments(FloatPointArray, PointCount, NormalKind, lines);
  if NormalKind = nkPrincipal then RightIntensity := 0.8 else RightIntensity := 0.2;
  GetIlluminatedLinesegments(savecolor, ambient, directed, RightIntensity, zrot1, zrot2, yrot1, yrot2, dist1, dist2, fixed, lines);
  DrawLineSegments(ACanvas, lines);
end;

procedure TWorldDrawing.d3PolyPolyline(ACanvas: TCanvas;
  const GraphList: array of Td3FloatPointArray);
var i: Integer;
begin
  for i := Low(GraphList) to High(GraphList) do
    d3Polyline(ACanvas, GraphList[i], Length(GraphList[i]));
end;


(***************  procedures and types for surface drawing ******************)

procedure CrossProduct(x1, y1, z1, x2, y2, z2: MathFloat; var u1, u2, u3: MathFloat);
begin
  u1 := y1 * z2 - z1 * y2;
  u2 := z1 * x2 - x1 * z2;
  u3 := x1 * y2 - y1 * x2;
end;

procedure TWorldDrawing.GetBrightness(const p, n: TD3FloatPoint; var BDiff, BSpec: MathFloat);
var
  xb2, yb2, zb2, Normu, Normv,
    Norml, ViewScalar, LightScalar: MathFloat;
  u, v, lloc, h: TD3FloatPoint;
  CanSee: Boolean;
begin
  ScaleNormal(n, u);
  {Block(p.x + n.x, p.y + n.y, p.z + n.z, xb1, yb1, zb1);}
  Block(p.x, p.y, p.z, xb2, yb2, zb2);
  {u1 := xb1 - xb2; u2 := yb1 - yb2; u3 := zb1 - zb2;}
  Normu := sqrt(sqr(u.x) + sqr(u.y) + sqr(u.z));
  if Normu = 0 then
  begin
    BDiff := 0; BSpec := 0; exit;
  end;
  D3FloatPoint(8 * (cosz * 1.732 - sinz) * (siny * 1.732 - cosy) - xb2, 8 * (sinz * 1.732 + cosz) * (siny * 1.732 - cosy) - yb2, 8 * (cosy * 1.732 + siny) - zb2, lloc);
  //lightsource vector
  D3FloatPoint(fd3vd * cosz * siny - xb2, fd3vd * sinz * siny - yb2, fd3vd * cosy - zb2, v);
   //viewpoint vector
  ViewScalar := v.x * u.x + v.y * u.y + v.z * u.z;
  LightScalar := lloc.x * u.x + lloc.y * u.y + lloc.z * u.z;
  //DotProduct of view- and light- vectors with normal
  CanSee := ViewScalar * LightScalar > 0;
  //Viewer can see the side of the triangle which is illuminated?
  d3Norm(lloc, Norml);
  d3Norm(v, Normv);
  Norml := 1 / Norml;
  Normu := 1 / Normu;
  Normv := 1 / Normv;
  D3FloatPoint(v.x * Normv + lloc.x * Norml, v.y * Normv + lloc.y * Norml, v.z * Normv + lloc.z * Norml, h);
  if CanSee then
    BDiff := abs(LightScalar) * Norml * Normu
  else
    BDiff := 0.5 * abs(LightScalar) * Norml * Normu;
   //light up shadows
  d3Norm(h, Normv);
  if Normv = 0 then
  begin
    BSpec := 0; exit;
  end;
  LightScalar := h.x * u.x + h.y * u.y + h.z * u.z;
  if CanSee then
  begin
    BSpec := abs(LightScalar) / Normv * Normu;
    BSpec := exp(30 * ln(BSpec));
  end
  else
    BSpec := 0;
end;

type
  TReal = double;

// RGB, each 0 to 255, to HSV.
// H = 0.0 to 360.0 (corresponding to 0..360.0 degrees around hexcone)
// S = 0.0 (shade of gray) to 1.0 (pure color)
// V = 0.0 (black) to 1.0 {white)

// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 592.

procedure RGBToHSV(const r, g, b: TReal; var h, s, v: TReal);
var
  delta: TReal;
  min: TReal;
begin
  min := MinValue([r, g, b]); // USES Math
  v := MaxValue([r, g, b]);

  delta := v - min;

  // Calculate saturation: saturation is 0 if r, g and b are all 0
  if v = 0.0
    then s := 0
  else s := delta / v;
  if s = 0.0
    then h := 0 // Achromatic: When s = 0, h is undefined
  else begin // Chromatic
    delta := 1 / delta;
    if r = v
      then // between yellow and magenta [degrees]
      h := 60.0 * (g - b) * delta
    else
      if g = v
        then // between cyan and yellow
        h := 120.0 + 60.0 * (b - r) * delta
      else
        if b = v
          then // between magenta and cyan
          h := 240.0 + 60.0 * (r - g) * delta;

    if h < 0.0
      then h := h + 360.0
  end
end {RGBtoHSV};





// Based on C Code in "Computer Graphics -- Principles and Practice,"
// Foley et al, 1996, p. 593.
//
// H = 0.0 to 360.0 (corresponding to 0..360 degrees around hexcone)
// NaN (undefined) for S = 0
// S = 0.0 (shade of gray) to 1.0 (pure color)
// V = 0.0 (black) to 1.0 (white)

procedure HSVtoRGB(const h, s, v: TReal; var r, g, b: TReal);
var
  f: TReal;
  i: Integer;
  hTemp: TReal; // since H is CONST parameter
  p, q, t: TReal;
begin
  if s = 0.0 // color is on black-and-white center line
    then begin
    {IF       IsNaN(H)
    THEN BEGIN}
    r := v; // achromatic: shades of gray
    g := v;
    b := v
    {END
    ELSE RAISE EColorError.Create('HSVtoRGB: S = 0 and H has a value');}
  end

  else begin // chromatic color
    if h = 360.0 // 360 degrees same as 0 degrees
      then hTemp := 0.0
    else hTemp := h;

    hTemp := 1 / 60 * hTemp; // h is now IN [0,6)
    i := trunc(hTemp); // largest integer <= h
    f := hTemp - i; // fractional part of h

    p := v * (1.0 - s);
    q := v * (1.0 - (s * f));
    t := v * (1.0 - (s * (1.0 - f)));

    case i of
      0: begin r := v; g := t; b := p end;
      1: begin r := q; g := v; b := p end;
      2: begin r := p; g := v; b := t end;
      3: begin r := p; g := q; b := v end;
      4: begin r := t; g := p; b := v end;
      5: begin r := v; g := p; b := q end
    end
  end
end {HSVtoRGB};

{Translated C-code from Microsoft Knowledge Base
-------------------------------------------
Converting Colors Between RGB and HLS (HBS)
Article ID: Q29240
Creation Date: 26-APR-1988
Revision Date: 02-NOV-1995
The information in this article applies to:

Microsoft Windows Software Development Kit (SDK) for Windows versions 3.1 and 3.0
Microsoft Win32 Application Programming Interface (API) included with:

    - Microsoft Windows NT versions 3.5 and 3.51
    - Microsoft Windows 95 version 4.0
SUMMARY


The code fragment below converts colors between RGB (Red, Green, Blue) and HLS/HBS (Hue, Lightness, Saturation/Hue, Brightness, Saturation).


MORE INFORMATION


/* Color Conversion Routines --

RGBtoHLS() takes a DWORD RGB value, translates it to HLS, and stores the results in the global vars H, L, and S. HLStoRGB takes the current values of H, L, and S and returns the equivalent value in an RGB DWORD. The vars H, L, and S are only written to by:


   1. RGBtoHLS (initialization)
   2. The scroll bar handlers
A point of reference for the algorithms is Foley and Van Dam, "Fundamentals of Interactive Computer Graphics," Pages 618-19. Their algorithm is in floating point. CHART implements a less general (hardwired ranges) integral algorithm.
There are potential round-off errors throughout this sample. ((0.5 + x)/y) without floating point is phrased ((x + (y/2))/y), yielding a very small round-off error. This makes many of the following divisions look strange. */

*************************************************************************) }
const
  HLSMAX = 240; // H,L, and S vary over 0-HLSMAX
  RGBMAX = 255; // R,G, and B vary over 0-RGBMAX
    // HLSMAX BEST IF DIVISIBLE BY 6
    // RGBMAX, HLSMAX must each fit in a byte.
{ Hue is undefined if Saturation is 0 (grey-scale)
  This value determines where the Hue scrollbar is
  initially set for achromatic colors }
  UNDEFINED = HLSMAX * 2 div 3;

procedure RGBtoHLS(r, g, b: Integer; var h, l, s: Integer);
var
  // R, G, B: Integer;              (* input RGB values *)
  // H, L, S: Integer;
  cmax, cmin: byte; (* max and min RGB values *)
  Rdelta, Gdelta, Bdelta: Integer; (* intermediate value: % of spread from max*)
begin
   (* get R, G, and B out of DWORD *)
  // R := GetRValue(RGBColor);
  // G := GetGValue(RGBColor);
  // B := GetBValue(RGBColor);

   (* calculate lightness *)
  cmax := r;
  if g > cmax then cmax := g;
  if b > cmax then cmax := b;

  cmin := r;
  if g < cmin then cmin := g;
  if b < cmin then cmin := b;

  l := (((cmax + cmin) * HLSMAX) + RGBMAX) div (2 * RGBMAX);

  if (cmax = cmin) then // r=g=b --> achromatic case
  begin
    s := 0; // saturation
    h := UNDEFINED; // hue
  end else
  begin // chromatic case
     { saturation }
    if l <= (HLSMAX div 2) then
      s := (((cmax - cmin) * HLSMAX) + ((cmax + cmin) div 2)) div (cmax + cmin)
    else
      s := (((cmax - cmin) * HLSMAX) + ((2 * RGBMAX - cmax - cmin) div 2))
        div (2 * RGBMAX - cmax - cmin);

     (* hue *)
    Rdelta := (((cmax - r) * (HLSMAX div 6)) + ((cmax - cmin) div 2)) div (cmax - cmin);
    Gdelta := (((cmax - g) * (HLSMAX div 6)) + ((cmax - cmin) div 2)) div (cmax - cmin);
    Bdelta := (((cmax - b) * (HLSMAX div 6)) + ((cmax - cmin) div 2)) div (cmax - cmin);

    if r = cmax then
      h := Bdelta - Gdelta
    else if g = cmax then
      h := (HLSMAX div 3) + Rdelta - Bdelta
    else (* B = cMax *)
      h := ((2 * HLSMAX) div 3) + Gdelta - Rdelta;

    h := h mod HLSMAX;
    if h < 0 then
      inc(h, HLSMAX);
  end;
 // Result.Hue        := H;
 // Result.Luminance  := L;
 // Result.Saturation := S;
end;

function HueToRGB(N1, N2, hue: Integer): Integer;
(* utility routine for HLStoRGB *)
begin
  hue := hue mod HLSMAX;
   (* range check: note values passed add div subtract thirds of range *)
  if hue < 0 then
    inc(hue, HLSMAX);

   (* return r,g, or b value from this tridrant *)
  if hue < (HLSMAX div 6) then
    Result := (N1 + (((N2 - N1) * hue + (HLSMAX div 12)) div (HLSMAX div 6))) else
    if hue < (HLSMAX div 2) then
      Result := N2 else
      if hue < ((HLSMAX * 2) div 3) then
        Result := (N1 + (((N2 - N1) * (((HLSMAX * 2) div 3) - hue) + (HLSMAX div 12)) div (HLSMAX div 6)))
      else
        Result := N1;
end;

procedure HLStoRGB(hue, Luminance, Saturation: Integer; var r, g, b: Integer);
var
 //  R, G, B: Integer;              (* RGB component values *)
  Magic1, Magic2: Integer; (* calculated magic numbers (really!) *)
begin
  if Saturation = 0 then (* achromatic case *)
  begin
    r := (Luminance * RGBMAX) div HLSMAX;
    g := r;
    b := r;
    if hue <> UNDEFINED then
    begin
  (* ERROR *)
    end
  end else
  begin (* chromatic case *)
      (* set up magic numbers *)
    if (Luminance <= (HLSMAX div 2)) then
      Magic2 := (Luminance * (HLSMAX + Saturation) + (HLSMAX div 2)) div HLSMAX
    else
      Magic2 := Luminance + Saturation - ((Luminance * Saturation) + (HLSMAX div 2)) div HLSMAX;
    Magic1 := 2 * Luminance - Magic2;
      (* get RGB, change units from HLSMAX to RGBMAX *)
    r := (HueToRGB(Magic1, Magic2, hue + (HLSMAX div 3)) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
    g := (HueToRGB(Magic1, Magic2, hue) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
    b := (HueToRGB(Magic1, Magic2, hue - (HLSMAX div 3)) * RGBMAX + (HLSMAX div 2)) div HLSMAX;
  end;
   //Result :=  RGB(R,G,B);
end;



function GetIlluminatedColor(AColor: TColor; BDiff, BSpec: MathFloat): TColor;
var
  ri, gi, bi: Integer;
begin
  {epsilon := 1.E-12;
  ri := GetRValue(AColor);
  gi := GetGValue(AColor);
  bi := GetBValue(AColor);
  r := 1 / 255 * ri; g := 1 / 255 * gi; b := 1 / 255 * bi;
  RGBtoHLS(ri, gi, bi, Hi, li, si);
  li := round(5 / 3 * li * Brightness);
  if li > HLSMAX then li := HLSMAX;
  if li < 0 then li := 0;
  HLStoRGB(Hi, li, si, ri, gi, bi);
  RGBToHSV(r, g, b, h, s, v);
  v := 5 / 3 * v * Brightness;
  if v > 1 then v := 1;
  if v < 0 then v := 0;
  HSVtoRGB(h, s, v, r, g, b);
  t := 0.5;
  l := 1 - t;
  r := 1 / 255 * t * ri + l * r;
  g := 1 / 255 * t * gi + l * g;
  b := 1 / 255 * t * bi + l * b;
  if r > 1 then r := 1;
  if g > 1 then g := 1;
  if b > 1 then b := 1;
  if r < epsilon then r := 0;
  if b < epsilon then b := 0;
  if g < epsilon then g := 0;
  Result := RGB(round(255 * r), round(255 * g), round(255 * b));}
  ri := GetRValue(AColor);
  gi := GetGValue(AColor);
  bi := GetBValue(AColor);
  ri := round(BDiff * ri + BSpec);
  if ri > 255 then ri := 255;
  gi := round(BDiff * gi + BSpec);
  if gi > 255 then gi := 255;
  bi := round(BDiff * bi + 1.1 * BSpec);
  if bi > 255 then bi := 255;
  Result := RGB(ri, gi, bi);
end;



procedure sort4Cells(var AArray: array of T4Cell);

  procedure QuickSort4(iLo, iHi:
    Integer);
  var
    Lo, Hi: Integer; Mid: T4Cell; Temp: T4Cell;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := AArray[(Lo + Hi) div 2];
    repeat
      while AArray[Lo].dist > Mid.dist do inc(Lo);
      while AArray[Hi].dist < Mid.dist do dec(Hi);
      if Lo <= Hi then
      begin
        Temp := AArray[Lo];
        AArray[Lo] := AArray[Hi];
        AArray[Hi] := Temp;
        inc(Lo);
        dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort4(iLo, Hi);
    if Lo < iHi then QuickSort4(Lo, iHi);
  end;


begin
  QuickSort4(0, High(AArray));
end;

procedure Sort3Cells(var AArray: array of T3Cell);

  procedure QuickSort3(iLo, iHi:
    Integer);
  var
    Lo, Hi: Integer; Mid: T3Cell; Temp: T3Cell;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := AArray[(Lo + Hi) div 2];
    repeat
      while AArray[Lo].dist > Mid.dist do inc(Lo);
      while AArray[Hi].dist < Mid.dist do dec(Hi);
      if Lo <= Hi then
      begin
        Temp := AArray[Lo];
        AArray[Lo] := AArray[Hi];
        AArray[Hi] := Temp;
        inc(Lo);
        dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort3(iLo, Hi);
    if Lo < iHi then QuickSort3(Lo, iHi);
  end;

begin
  QuickSort3(0, High(AArray));
end;



procedure GetTriangles(const SurfArray: array of Td3FloatPointArray; var Triangles: TD3TriangleArray);
var i, j, imax, jmax, Current: Integer;
begin
  imax := High(SurfArray);
  jmax := High(SurfArray[0]);
  SetLength(Triangles, 2 * imax * jmax);
  Current := 0;
  for i := 0 to imax - 1 do
    for j := 0 to jmax - 1 do
    begin
      if not (odd(i) or odd(j)) or (odd(i) and odd(j)) then
      begin
        with Triangles[Current] do
        begin
          p := @SurfArray[i][j];
          q := @SurfArray[i + 1][j];
          r := @SurfArray[i][j + 1];
        end;
        inc(Current);
        with Triangles[Current] do
        begin
          p := @SurfArray[i + 1][j + 1];
          q := @SurfArray[i + 1][j];
          r := @SurfArray[i][j + 1];
        end;
        inc(Current);
      end
      else
      begin
        with Triangles[Current] do
        begin
          p := @SurfArray[i][j];
          q := @SurfArray[i][j + 1];
          r := @SurfArray[i + 1][j + 1];
        end;
        inc(Current);
        with Triangles[Current] do
        begin
          p := @SurfArray[i + 1][j];
          q := @SurfArray[i + 1][j + 1];
          r := @SurfArray[i][j];
        end;
        inc(Current);
      end;
    end;
  for i := 0 to High(Triangles) do
    with Triangles[i] do
      CrossProduct(p.x - r.x, p.y - r.y, p.z - r.z, q.x - r.x, q.y - r.y, q.z - r.z, n.x, n.y, n.z);
end;


(*************** End: procedures and types for surface drawing ******************)


(*************** The surface drawing routines *****************************)


procedure TWorldDrawing.d3DrawLitSurface(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray; diffuse, focussed: MathFloat);

var
  Triangles: TD3TriangleArray;
  i: Integer;
begin
  GetTriangles(SurfArray, Triangles);
  fDefaultFillColor := ACanvas.Brush.Color;
  for i := 0 to High(Triangles) do
    Triangles[i].FillColor := @fDefaultFillColor;
  d3DrawLitTriangles(ACanvas, Triangles, diffuse, focussed);
end;

procedure TWorldDrawing.d3DrawLitTriangles(ACanvas: TCanvas;
  const Triangles: array of TD3Triangle; diffuse, focussed: MathFloat);
var Cells: array of T3Cell;
  i: Integer;
  c: TD3FloatPoint;
  SaveBrush: TBrush;
  SavePen: TPen;
begin
  SaveBrush := TBrush.Create;
  SavePen := TPen.Create;
  SaveBrush.assign(ACanvas.Brush);
  SavePen.assign(ACanvas.Pen);
  SetLength(Cells, Length(Triangles));
  for i := 0 to High(Triangles) do
  begin
    with Triangles[i], Cells[i] do
    begin
      c.x := 1 / 3 * (r.x + q.x + p.x);
      c.y := 1 / 3 * (r.y + q.y + p.y);
      c.z := 1 / 3 * (r.z + q.z + r.z);
      d3DistanceToViewer(c.x, c.y, c.z, dist);
      d3Window(p.x, p.y, p.z, Vertex[0].x, Vertex[0].y);
      d3Window(q.x, q.y, q.z, Vertex[1].x, Vertex[1].y);
      d3Window(r.x, r.y, r.z, Vertex[2].x, Vertex[2].y);
    end;
    GetBrightness(c, Triangles[i].n, Cells[i].BDiff, Cells[i].BSpec);
    Cells[i].FillColor := GetIlluminatedColor(Triangles[i].FillColor^, (diffuse + Cells[i].BDiff * focussed), focussed * 100 * Cells[i].BSpec);
  end;
  Sort3Cells(Cells);
  ACanvas.Brush.Style := bssolid;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  for i := 0 to High(Cells) do
    with Cells[i] do
    begin
      ACanvas.Brush.Color := FillColor;
      ACanvas.Pen.Color := FillColor;
      ACanvas.Polygon(Cells[i].Vertex);
    end;
  ACanvas.Brush.assign(SaveBrush);
  ACanvas.Pen.assign(SavePen);
  SaveBrush.Free;
  SavePen.Free;
end;


procedure TWorldDrawing.d3DrawSurface(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray; fill: Boolean);
var
  i, j, imax, jmax: Integer;
  p: TD3FloatPoint;
  SurfaceCells: array of TD3SurfaceCell;
  Current: Integer;
begin
  imax := High(SurfArray);
  jmax := High(SurfArray[0]);
  //assume all same length!! { TODO : warning }
  if not fill then
  begin
    for i := 0 to imax do
      d3Polyline(ACanvas, SurfArray[i], jmax + 1);
    for j := 0 to jmax do
    begin
      p := SurfArray[0][j];
      d3Moveto(ACanvas, p.x, p.y, p.z);
      for i := 1 to imax do
      begin
        p := SurfArray[i][j];
        d3DrawLineto(ACanvas, p.x, p.y, p.z);
      end;
    end;
  end
  else
  begin
    fDefaultFillColor := ACanvas.Brush.Color;
    fDefaultWireColor := ACanvas.Pen.Color;
    SetLength(SurfaceCells, imax * jmax);
    Current := 0;
    for i := 0 to imax - 1 do
      for j := 0 to jmax - 1 do
      begin
        SurfaceCells[Current].p := @SurfArray[i][j];
        SurfaceCells[Current].q := @SurfArray[i + 1][j];
        SurfaceCells[Current].r := @SurfArray[i + 1][j + 1];
        SurfaceCells[Current].s := @SurfArray[i][j + 1];
        SurfaceCells[Current].WireColor := @fDefaultWireColor;
        SurfaceCells[Current].FillColor := @fDefaultFillColor;
        inc(Current);
      end;
    d3DrawSurfaceCells(ACanvas, SurfaceCells);
  end;
end;



procedure TWorldDrawing.d3DrawTriangles(ACanvas: TCanvas;
  const Triangles: array of TD3Triangle);
var Cells: array of T3Cell;
  i: Integer;
  d: MathFloat;
  SaveBrush, SavePen: TColor;
begin
  SaveBrush := ACanvas.Brush.Color;
  SavePen := ACanvas.Pen.Color;
  SetLength(Cells, Length(Triangles));
  for i := 0 to High(Cells) do
  begin
   // New(Cells[i]);
    with Triangles[i], Cells[i] do
    begin
      d3Window(p.x, p.y, p.z, Vertex[0].x, Vertex[0].y);
      d3DistanceToViewer(p.x, p.y, p.z, dist);
      d3Window(q.x, q.y, q.z, Vertex[1].x, Vertex[1].y);
      d3DistanceToViewer(q.x, q.y, q.z, d);
      dist := dist + d;
      d3Window(r.x, r.y, r.z, Vertex[2].x, Vertex[2].y);
      d3DistanceToViewer(r.x, r.y, r.z, d);
      dist := dist + d;
    end;
  end;
  Sort3Cells(Cells);
  for i := 0 to High(Cells) do
  begin
    ACanvas.Brush.Color := Triangles[i].FillColor^;
    ACanvas.Pen.Color := Triangles[i].WireColor^;
    ACanvas.Polygon(Cells[i].Vertex);
  end;
//  for i := 0 to High(Cells) do
  //  dispose(Cells[i]);
  ACanvas.Brush.Color := SaveBrush;
  ACanvas.Pen.Color := SavePen;
end;

procedure TWorldDrawing.Draw4Cells(ACanvas: TCanvas; const Cells: array of T4Cell);
var
  i: Integer;
  SaveBrush: TBrush;
  SavePen: TPen;
begin
  SaveBrush := TBrush.Create;
  SavePen := TPen.Create;
  SaveBrush.assign(ACanvas.Brush);
  SavePen.assign(ACanvas.Pen);
  for i := 0 to High(Cells) do
  begin
    ACanvas.Brush.Color := Cells[i].FillColor;
    ACanvas.Pen.Color := Cells[i].WireColor;
    ACanvas.Polygon(Cells[i].Vertex);
  end;
  ACanvas.Brush.assign(SaveBrush);
  ACanvas.Pen.assign(SavePen);
  SaveBrush.Free;
  SavePen.Free;
end;

procedure TWorldDrawing.d3DrawSurfaceCells(ACanvas: TCanvas;
  const SurfaceCells: array of TD3SurfaceCell);
var Cells: array of T4Cell;
  i: Integer;
  d: MathFloat;
begin
  SetLength(Cells, Length(SurfaceCells));
  for i := 0 to High(Cells) do
  begin
  //  New(Cells[i]);
    with SurfaceCells[i], Cells[i] do
    begin
      d3Window(p.x, p.y, p.z, Vertex[0].x, Vertex[0].y);
      d3DistanceToViewer(p.x, p.y, p.z, dist);
      d3Window(q.x, q.y, q.z, Vertex[1].x, Vertex[1].y);
      d3DistanceToViewer(q.x, q.y, q.z, d);
      dist := dist + d;
      d3Window(r.x, r.y, r.z, Vertex[2].x, Vertex[2].y);
      d3DistanceToViewer(r.x, r.y, r.z, d);
      dist := dist + d;
      d3Window(s.x, s.y, s.z, Vertex[3].x, Vertex[3].y);
      d3DistanceToViewer(s.x, s.y, s.z, d);
      dist := dist + d;
    end;
    Cells[i].FillColor := SurfaceCells[i].FillColor^;
    Cells[i].WireColor := SurfaceCells[i].WireColor^;
  end;
  sort4Cells(Cells);
  Draw4Cells(ACanvas, Cells);
//  for i := 0 to High(Cells) do
//    dispose(Cells[i]);
end;


procedure TWorldDrawing.d3DrawCubes(ACanvas: TCanvas; const Cubes: array of TCube; fill: Boolean);
var
  Cells: array of TD3SurfaceCell;
  i, j: Integer;
  SavePen: TColor;
begin
  if not fill then
  begin
    SavePen := ACanvas.Pen.Color;
    for i := 0 to High(Cubes) do
      with Cubes[i] do
      begin
        ACanvas.Pen.Color := Cubes[i].WireColor;
        d3DrawBox(ACanvas, x1, y1, z1, x2, y2, z2);
      end;
    ACanvas.Pen.Color := SavePen;
  end
  else
  begin
    SetLength(Cells, 6 * Length(Cubes));
    for i := Low(Cubes) to High(Cubes) do
      with Cubes[i] do
      begin
        with Cells[6 * i] do
        begin
          p := @p1; q := @p2;
          r := @p3; s := @p4;
        end;
        with Cells[6 * i + 1] do
        begin
          p := @p1; q := @p2;
          r := @p6; s := @p5;
        end;
        with Cells[6 * i + 2] do
        begin
          p := @p2; q := @p3;
          r := @p7; s := @p6;
        end;
        with Cells[6 * i + 3] do
        begin
          p := @p3; q := @p4;
          r := @p8; s := @p7;
        end;
        with Cells[6 * i + 4] do
        begin
          p := @p4; q := @p1;
          r := @p5; s := @p8;
        end;
        with Cells[6 * i + 5] do
        begin
          p := @p5; q := @p6;
          r := @p7; s := @p8;
        end;
        for j := 0 to 5 do
        begin
          Cells[6 * i + j].WireColor := @WireColor;
          Cells[6 * i + j].FillColor := @FillColor;
        end;

      end;
    d3DrawSurfaceCells(ACanvas, Cells);
  end;
end;


procedure TWorldDrawing.d3DrawColorSurface(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray;
  Colors: array of TColorArray);
var
  i, j, imax, jmax: Integer;
  SurfaceCells: array of TD3SurfaceCell;
  Current: Integer;
begin
  imax := High(SurfArray);
  jmax := High(SurfArray[0]);
  //assume al{ TODO : Warning }l same length!!
  SetLength(SurfaceCells, imax * jmax);
  Current := 0;
  fDefaultWireColor := ACanvas.Pen.Color;
  for i := 0 to imax - 1 do
    for j := 0 to jmax - 1 do
    begin
      SurfaceCells[Current].p := @SurfArray[i][j];
      SurfaceCells[Current].q := @SurfArray[i + 1][j];
      SurfaceCells[Current].r := @SurfArray[i + 1][j + 1];
      SurfaceCells[Current].s := @SurfArray[i][j + 1];
      SurfaceCells[Current].WireColor := @fDefaultWireColor;
      SurfaceCells[Current].FillColor := @Colors[i][j];
      inc(Current);
    end;
  d3DrawSurfaceCells(ACanvas, SurfaceCells);
end;



procedure TWorldDrawing.DrawAxes(ACanvas: TCanvas; xLabel, yLabel: string;
  AxesColor: TColor; Arrows: Boolean = True);
var
  xs, Ys, i, iStart, Ticks: Integer;
  SavePen: TPen; SaveBrush: TBrush;
  t: string;
  iTemp, xTick, yTick, inv, log, invlog: MathFloat;

  function min(i, j: longint): longint;
  begin
    if i < j then Result := i else Result := j;
  end;

begin
  if d2Axes then
  begin
    SavePen := TPen.Create;
    SaveBrush := TBrush.Create;
    SavePen.assign(ACanvas.Pen);
    SaveBrush.assign(ACanvas.Brush);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Width := 1;
    ACanvas.Pen.Color := AxesColor;
    DrawLine(ACanvas, x1Snap, y1Snap, x2Snap, y1Snap);
    log := ln(10);
    invlog := 1 / log;
    iTemp := ln(0.125 * (fd2xw)) * invlog;
    if iTemp >= 0 then
      i := trunc(iTemp) else i := trunc(iTemp) - 1;
    xTick := exp(i * log);
    iTemp := invlog * ln(0.125 * (fd2yw));
    if iTemp >= 0 then
      i := trunc(iTemp) else i := trunc(iTemp) - 1;
    yTick := exp(i * log);
    inv := 1 / xTick;
    if xTick > 0 then
      if abs(fd2x1 * inv) < maxint then
      begin
        iStart := round(fd2x1 * inv);
        while iStart * xTick < fd2x1 do inc(iStart);
        i := iStart;
        Ticks := round((fd2xw) * inv);
        with ACanvas.Font do
          Size := Size - 1;
        if Ticks <= 2000 then
          repeat
            WorldToScreen(i * xTick, y1Snap, xs, Ys);
            ACanvas.MoveTo(xs, Ys);
            ACanvas.LineTo(xs, Ys + 4);
            if (i - iStart) mod (Ticks div 4) = 0 then
            begin
              t := FloatToStrf(i * xTick, ffgeneral, 3, 3);
              with ACanvas do
              begin
                TextOut(xs - (TextWIdth(t) div 2), Ys + 6, t);
                MoveTo(xs, Ys);
                LineTo(xs, Ys + 6);
              end;
            end;
            inc(i)
          until i * xTick > fd2x1 + fd2xw;
        with ACanvas.Font do Size := Size + 1;
      end;
    WorldToScreen(x2Snap, y1Snap, xs, Ys);
    if Arrows then
    begin
      ACanvas.MoveTo(xs - 6, Ys - 6);
      ACanvas.LineTo(xs, Ys);
      ACanvas.MoveTo(xs - 6, Ys + 6);
      ACanvas.LineTo(xs, Ys);
    end;
    ACanvas.TextOut(fwidth - ACanvas.TextWIdth(xLabel) - 2, fHeight - ACanvas.TextHeight(xLabel) - 2, xLabel);
    DrawLine(ACanvas, x1Snap, y1Snap, x1Snap, y2Snap);
    inv := 1 / yTick;
    if yTick > 0 then
      if abs(fd2y1 * inv) < maxint then
      begin
        iStart := round(fd2y1 * inv);
        while iStart * yTick < fd2y1 do inc(iStart);
        i := iStart;
        Ticks := round((fd2yw) * inv);
        with ACanvas.Font do
          Size := Size - 1;
        if Ticks <= 2000 then
          repeat
            WorldToScreen(x1Snap, i * yTick, xs, Ys);
            ACanvas.MoveTo(xs, Ys);
            ACanvas.LineTo(xs - 4, Ys);
            if (i - iStart) mod (Ticks div 4) = 0 then
            begin
              t := FloatToStrf(i * yTick, ffgeneral, 3, 3);
              with ACanvas do
              begin
                TextOut(xs - TextWIdth(t) - 6, Ys - TextHeight(t) div 2, t);
                MoveTo(xs, Ys);
                LineTo(xs - 6, Ys);
              end;
            end;
            inc(i);
          until i * yTick > fd2y1 + fd2yw;
        with ACanvas.Font do
          Size := Size + 1;
      end;
    WorldToScreen(x1Snap, y2Snap, xs, Ys);
    if Arrows then
    begin
      ACanvas.MoveTo(xs + 6, Ys + 6);
      ACanvas.LineTo(xs, Ys);
      ACanvas.MoveTo(xs - 6, Ys + 6);
      ACanvas.LineTo(xs, Ys);
    end;
    ACanvas.TextOut(2, 2, yLabel);
    ACanvas.Pen.assign(SavePen);
    ACanvas.Brush.assign(SaveBrush);
    SaveBrush.Free;
    SavePen.Free;
  end;
end;

procedure TWorldDrawing.DrawZeroLines(ACanvas: TCanvas; AColor: TColor);
var save: TColor;
begin
  save := ACanvas.Pen.Color;
  ACanvas.Pen.Color := AColor;
  DrawLine(ACanvas, 0, y1Snap, 0, y2Snap);
  DrawLine(ACanvas, x1Snap, 0, x2Snap, 0);
  ACanvas.Pen.Color := save;
end;

procedure TWorldDrawing.DrawEllipse(ACanvas: TCanvas; x1, y1, x2,
  y2: MathFloat);
var x1s, Y1s, x2s, Y2s: Integer;
begin
  WorldToScreen(x1, y1, x1s, Y1s);
  WorldToScreen(x2, y2, x2s, Y2s);
  if Y1s < Y2s then
    ACanvas.Ellipse(x1s, Y1s, x2s, Y2s)
  else
    ACanvas.Ellipse(x1s, Y2s, x2s, Y1s);
end;


procedure TWorldDrawing.DrawLevelCurves(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray; Level: MathFloat);
var
  Triangles: TD3TriangleArray;
begin
  GetTriangles(SurfArray, Triangles);
  DrawLevelLines(ACanvas, Triangles, Level);
end;

procedure TWorldDrawing.DrawLine(ACanvas: TCanvas; x1, y1, x2,
  y2: MathFloat);
var
  pnts: array[0..2] of TPoint;
begin
 { pnts[0].x:=round(bx+ax*x1);
  pnts[0].y:=round(by+ay*y1);
  pnts[1].x:=round(bx+ax*x2);
  pnts[1].y:=round(by+ay*y2);
  pnts[2]:=pnts[0];}
  WorldToScreen(x1, y1, pnts[0].x, pnts[0].y);
  WorldToScreen(x2, y2, pnts[1].x, pnts[1].y);
  pnts[2] := pnts[0];
  ACanvas.Polyline(pnts);
end;

procedure TWorldDrawing.DrawLineTo(ACanvas: TCanvas; x, y: MathFloat);
var xs, Ys: Integer;
begin
  WorldToScreen(x, y, xs, Ys);
  ACanvas.LineTo(xs, Ys);
end;

procedure TWorldDrawing.DrawOneAxis(ACanvas: TCanvas; x1, y1, z1, x2, y2,
  z2: MathFloat; Arrows: Boolean);
var
  Norms, wx, wy: MathFloat;
  xs1, Ys1, xs2, Ys2: longint; vsx, vsy, inv: MathFloat;
begin
  d3DrawLine(ACanvas, x1, y1, z1, x2, y2, z2);
  if Arrows then
  begin
    d3Window(x1, y1, z1, xs1, Ys1);
    d3Window(x2, y2, z2, xs2, Ys2);
    vsx := (xs2 - xs1); vsy := (Ys2 - Ys1);
    Norms := sqrt(vsx * vsx + vsy * vsy);
    if Norms > 0 then
    begin
      Norms := 1 / Norms;
      vsx := vsx * Norms; vsy := vsy * Norms;
      wx := (-vsx + vsy) / sqrt(2); wy := (-vsy - vsx) / sqrt(2);
      ACanvas.MoveTo(xs2, Ys2);
      ACanvas.LineTo(xs2 + round(8 * wx), Ys2 + round(8 * wy));
      inv := 1 / sqrt(2);
      wx := (-vsx - vsy) * inv; wy := (-vsy + vsx) * inv;
      ACanvas.MoveTo(xs2, Ys2);
      ACanvas.LineTo(xs2 + round(8 * wx), Ys2 + round(8 * wy));
    end;
  end;
end;

procedure TWorldDrawing.DrawPoint(ACanvas: TCanvas; x, y: MathFloat);
var xs, Ys: Integer;
begin
  WorldToScreen(x, y, xs, Ys);
  ACanvas.Pixels[xs, Ys] := ACanvas.Pen.Color;
end;

procedure TWorldDrawing.DrawPolygon(ACanvas: TCanvas;
  const FloatPointArray: array of TFloatpoint; PointCount: Integer);
var
  i: Integer; p: array of TPoint;
begin
  //Do exception checking in TMathImage
  SetLength(p, PointCount);
  for i := 0 to PointCount - 1 do
    Convert(FloatPointArray[i], p[i]);
  ACanvas.Polygon(p);
end;

procedure TWorldDrawing.DrawPolyline(ACanvas: TCanvas;
  const FloatPointArray: array of TFloatpoint; PointCount: Integer);
var
  i: Integer; p: array of TPoint;
begin
  //Win95/98 GDI only accepts up to 16320 points in p.
  SetLength(p, PointCount);
  for i := 0 to PointCount - 1 do
    Convert(FloatPointArray[i], p[i]);
  ACanvas.Polyline(p);
end;



procedure TWorldDrawing.DrawPolyPolyline(ACanvas: TCanvas;
  const GraphList: array of TFloatPointArray);
var i: Integer;
begin
  for i := Low(GraphList) to High(GraphList) do
    DrawPolyline(ACanvas, GraphList[i], Length(GraphList[i]));
end;

procedure TWorldDrawing.DrawRectangle(ACanvas: TCanvas; x1, y1, x2,
  y2: MathFloat);
var x1s, Y1s, x2s, Y2s: Integer;
begin
  WorldToScreen(x1, y1, x1s, Y1s);
  WorldToScreen(x2, y2, x2s, Y2s);
  ACanvas.Rectangle(x1s, Y2s, x2s, Y1s);
end;

procedure TWorldDrawing.DrawVector(ACanvas: TCanvas; x, y, A,
  b: MathFloat);
var
  aw, bw, xw, yw, u1, u2, v1, v2: Integer;
  n: MathFloat;
  pts: array[0..5] of TPoint;
begin
  WorldToScreen(A + x, b + y, v1, v2);
  WorldToScreen(x, y, xw, yw);
  pts[0] := Point(xw, yw);
  pts[1] := Point(v1, v2);
  aw := v1 - xw;
  bw := v2 - yw;
  n := Norm(bw - aw, aw + bw);
  if n > 0 then
  begin
    n := 1 / n;
    u1 := round(8.0 * (bw - aw) * n);
    u2 := round(8.0 * (-bw - aw) * n);
    pts[2] := Point(v1 + u1, v2 + u2);
    pts[3] := pts[1];
    u1 := round(8.0 * (-aw - bw) * n);
    u2 := round(8.0 * (aw - bw) * n);
    pts[4] := Point(v1 + u1, v2 + u2);
    pts[5] := pts[3];
    ACanvas.Polyline(pts);
  end;
end;


procedure TWorldDrawing.MoveToPoint(ACanvas: TCanvas; x, y: MathFloat);
var xs, Ys: Integer;
begin
  WorldToScreen(x, y, xs, Ys);
  ACanvas.MoveTo(xs, Ys);
end;

function TWorldDrawing.Norm(x, y: MathFloat): MathFloat;
begin
  Result := sqrt(sqr(x) + sqr(y));
end;

procedure GetExpoMant(x: MathFloat; var A: Integer; var m: MathFloat);
{Only works for x>0}
var r: MathFloat;
begin
  r := ln(x) / ln(10);
  if r >= 0 then
    A := trunc(r)
  else
    A := trunc(r) - 1;
  m := x * exp(-ln(10) * A);
end;

function MaxTextWidth(ACanvas: TCanvas; xx1, xx2: MathFloat): Integer;
var
  iTemp, xTick, xTickInv: MathFloat;
  i, iStart, Ticks, w: longint;
begin
  iTemp := ln(0.125 * abs(xx2 - xx1)) / ln(10);
  if iTemp >= 0 then i := trunc(iTemp) else i := trunc(iTemp) - 1;
  xTick := exp(i * ln(10));
  xTickInv := 1 / xTick;
  if (abs(xx1 * xTickInv) >= maxint) then
  begin
    Result := 0;
    exit; //beyond range don't draw any ticks
  end;
  iStart := round(xx1 * xTickInv);
  while iStart * xTick < xx1 do
    inc(iStart);
  Ticks := round((xx2 - xx1) * xTickInv);
  ACanvas.Font.Size := ACanvas.Font.Size - 1;
  w := ACanvas.TextWIdth(FloatToStrf(iStart * xTick, ffgeneral, 3, 3));
  for i := 1 to 4 do
    w := max(w, ACanvas.TextWIdth(FloatToStrf((iStart + i * (Ticks div 4)) *
      xTick, ffgeneral, 3, 3)));
  Result := w;
  ACanvas.Font.Size := ACanvas.Font.Size + 1;
end;

procedure TWorldDrawing.SetWorld(ACanvas: TCanvas; x1, y1, x2, y2: MathFloat);
var
  XPixelWidth, YPixelWidth, XPixelStart, YPixelstart: Integer;
  ex, k1: Integer;
  m: MathFloat;

begin
  fd2x1 := x1; fd2xw := x2 - x1; fd2y1 := y1; fd2yw := y2 - y1;
  if fd2Axes then
  begin
    fmaxxtw := MaxTextWidth(ACanvas, x1, x2);
    fmaxytw := MaxTextWidth(ACanvas, y1, y2);
    fmaxth := ACanvas.TextHeight('-1.234');
    XPixelWidth := fwidth - 10 - fmaxxtw - fmaxytw;
    YPixelWidth := fHeight - 3 * fmaxth - fmaxth div 2;
    XPixelStart := 10 + fmaxytw;
    YPixelstart := 2 * fmaxth - fmaxth div 8;
  end else
  begin
    XPixelWidth := fwidth;
    XPixelStart := 0;
    YPixelWidth := fHeight;
    YPixelstart := 0;
  end;
  m := (x2 - x1) / XPixelWidth;
  GetExpoMant(m, ex, m);
  ax := exp(-ln(10) * (ex - 3)) / round(1000 * m);
  axinv := 1 / ax;
  k1 := round(0.5 * (XPixelWidth - ax * (x1 + x2)));
  bx := XPixelStart + k1;
  m := (y2 - y1) / YPixelWidth;
  GetExpoMant(m, ex, m);
  ay := -exp(-ln(10) * (ex - 3)) / round(1000 * m);
  ayinv := 1 / ay;
  k1 := round(0.5 * (YPixelWidth + ay * (y1 + y2)));
  by := fHeight - YPixelstart - k1;
  x1Snap := WorldX(XPixelStart);
  x2Snap := WorldX(XPixelStart + XPixelWidth);
  y1Snap := WorldY(fHeight - YPixelstart);
  y2Snap := WorldY(fHeight - YPixelstart - YPixelWidth);
  if d2Axes then
    fClipRect := Rect(XPixelStart + 1, fHeight - YPixelstart - YPixelWidth, XPixelStart + XPixelWidth + 1, fHeight - YPixelstart)
  else
    fClipRect := Rect(0, 0, fwidth, fHeight);
end;


function TWorldDrawing.WorldX(xs: Integer): MathFloat;
begin
  Result := (xs - bx) * axinv;
end;

function TWorldDrawing.WorldY(Ys: Integer): MathFloat;
begin
  Result := (Ys - by) * ayinv;
end;



procedure TWorldDrawing.d3SetScales(xScale, yScale, zScale: MathFloat);
begin
   //do exception handling in TMathImage
  fd3xScale := xScale;
  fd3yScale := yScale;
  fd3zScale := zScale;
  InitWorld;
end;

procedure TWorldDrawing.d3SetViewPoint(vd, alpha, yr, zr: MathFloat);
begin
   //do exception handling in TMathImage
  if vd > 0 then
    fd3vd := vd;
  if alpha > 0 then
    if alpha < 180 then
      fd3alpha := alpha;
  if yr > -180 then if yr < 180 then
      fd3yr := yr;
  if zr > -180 then if zr < 180 then
      fd3zr := zr;
  InitWorld;
end;

procedure TWorldDrawing.d3DrawLitCubes(ACanvas: TCanvas;
  const Cubes: array of TCube; diffuse, focussed: MathFloat);
var
  Cells: array of TD3Triangle;
  i, j: Integer;
begin
  SetLength(Cells, 12 * Length(Cubes));
  for i := 0 to High(Cubes) do
    with Cubes[i] do
    begin

      with Cells[12 * i] do
      begin
        p := @p1; q := @p2;
        r := @p3;
      end;
      with Cells[12 * i + 1] do
      begin
        p := @p1; q := @p3;
        r := @p4;
      end;
      with Cells[12 * i + 2] do
      begin
        p := @p2; q := @p3;
        r := @p6;
      end;
      with Cells[12 * i + 3] do
      begin
        p := @p3; q := @p6;
        r := @p7;
      end;
      with Cells[12 * i + 4] do
      begin
        p := @p3; q := @p4;
        r := @p8;
      end;
      with Cells[12 * i + 5] do
      begin
        p := @p3; q := @p8;
        r := @p7;
      end;
      with Cells[12 * i + 6] do
      begin
        p := @p1; q := @p4;
        r := @p8;
      end;
      with Cells[12 * i + 7] do
      begin
        p := @p1; q := @p8;
        r := @p5;
      end;
      with Cells[12 * i + 8] do
      begin
        p := @p1; q := @p2;
        r := @p5;
      end;
      with Cells[12 * i + 9] do
      begin
        p := @p2; q := @p5;
        r := @p6;
      end;
      with Cells[12 * i + 10] do
      begin
        p := @p5; q := @p6;
        r := @p8;
      end;
      with Cells[12 * i + 11] do
      begin
        p := @p6; q := @p8;
        r := @p7;
      end;
      for j := 0 to 11 do
      begin
        Cells[12 * i + j].WireColor := @WireColor;
        Cells[12 * i + j].FillColor := @FillColor;
      end;
    end;
  d3DrawLitTriangles(ACanvas, Cells, diffuse, focussed);
end;

procedure MakeCubes(var Cubes: array of TCube; const HeightArray: array of TFloatarray; const Colors: array of TColorArray; imax, jmax: Integer; PenColor: TColor);
var
  i, j, Current: Integer;
begin
  //This is just the start of trying something else.
  //This heightmap stuff doesn't really work without
  //more sophisticated hidden parts removal.
  Current := 0;
  for i := 0 to imax do
    for j := 0 to jmax do
    begin
      Cubes[Current].x1 := i;
      Cubes[Current].y1 := j;
      Cubes[Current].z1 := 0;
      Cubes[Current].x2 := i + 1;
      Cubes[Current].y2 := j + 1;
      Cubes[Current].z2 := HeightArray[i][j];
      with Cubes[Current] do
      begin
        D3FloatPoint(x1, y1, z1, p1);
        D3FloatPoint(x2, y1, z1, p2);
        D3FloatPoint(x2, y2, z1, p3);
        D3FloatPoint(x1, y2, z1, p4);
        D3FloatPoint(x1, y1, z2, p5);
        D3FloatPoint(x2, y1, z2, p6);
        D3FloatPoint(x2, y2, z2, p7);
        D3FloatPoint(x1, y2, z2, p8);
      end;
      Cubes[Current].FillColor := Colors[i][j];
      Cubes[Current].WireColor := PenColor;
      inc(Current);
    end;
end;



procedure TWorldDrawing.d3DrawHeightCubes(ACanvas: TCanvas;
  const HeightArray: array of TFloatarray; const Colors: array of TColorArray);
var
  Cubes: array of TCube;
  imax, jmax: Integer;
begin
  imax := High(HeightArray);
  jmax := High(HeightArray[0]);
  SetLength(Cubes, (imax + 1) * (jmax + 1));
  MakeCubes(Cubes, HeightArray, Colors, imax, jmax, ACanvas.Pen.Color);
  d3DrawCubes(ACanvas, Cubes, True);
end;

procedure TWorldDrawing.d3DrawLitHeightCubes(ACanvas: TCanvas;
  const HeightArray: array of TFloatarray; const Colors: array of TColorArray; diffuse, focussed: MathFloat);
var
  Cubes: array of TCube;
  imax, jmax: Integer;
begin
  imax := High(HeightArray);
  jmax := High(HeightArray[0]);
  SetLength(Cubes, (imax + 1) * (jmax + 1));
  MakeCubes(Cubes, HeightArray, Colors, imax, jmax, ACanvas.Pen.Color);
  d3DrawLitCubes(ACanvas, Cubes, diffuse, focussed);
end;



procedure TWorldDrawing.DrawCircle(ACanvas: TCanvas; xCenter,
  yCenter: MathFloat; PixRadius: Integer);
var xs, Ys: Integer;
begin
  WorldToScreen(xCenter, yCenter, xs, Ys);
  ACanvas.Ellipse(xs - PixRadius, Ys - PixRadius, xs + PixRadius, Ys + PixRadius);
end;

procedure TWorldDrawing.ResetWorld(ACanvas: TCanvas);
begin
  Setd2Axes(ACanvas, fd2Axes);
end;

procedure TWorldDrawing.d3ResetWorld;
begin
  InitWorld;
end;

(****************  Levels Stuff *******************)


function TWorldDrawing.DoorInDoorOut(c, xp, yp, xq, yq,
  xr, yr, p, q, r: MathFloat; var x1, y1, x2, y2: MathFloat): Boolean;
{(xp,yp),(xq,yq),(xr,yr) are meshpoints of a triangular graph cell,
p,q,r are the function values at the meshpoints, c is the z-level which we want to
draw a level line for. If the result is true, there is a level line
through the triangle. In this case (x1,y1) and (x2,y2) return the
endpoints of the (straight) level line.}

var
  doors: Integer;
  doorx, doory: array[1..2] of MathFloat;
  t: MathFloat;
begin
  Result := False;
  if not (((q - c) * (p - c) <= 0) or ((r - c) * (q - c) <= 0)) then
    exit; //testing 2 is enough
  doors := 0;
  if (q - c) * (p - c) <= 0 then
  begin
    if q = p then //q=p=c
    begin
      x1 := xp;
      y1 := yp;
      x2 := xq;
      y2 := yq;
      Result := True;
      exit;
    end;
    inc(doors);
    t := (c - p) / (q - p);
    doorx[doors] := t * xq + (1 - t) * xp;
    doory[doors] := t * yq + (1 - t) * yp;
  end;
  if (r - c) * (q - c) <= 0 then
  begin
    if q = r then //q=r=c
    begin
      x1 := xr;
      y1 := yr;
      x2 := xq;
      y2 := yq;
      Result := True;
      exit;
    end;
    inc(doors);
    t := (c - q) / (r - q);
    doorx[doors] := t * xr + (1 - t) * xq;
    doory[doors] := t * yr + (1 - t) * yq;
  end;
  if doors = 1 then
  begin
    if (p - c) * (r - c) <= 0 then
    begin
      if p = r then //p=r=c
      begin
        x1 := xr;
        y1 := yr;
        x2 := xp;
        y2 := yp;
        Result := True;
        exit;
      end;
      inc(doors);
      t := (c - r) / (p - r);
      doorx[doors] := t * xp + (1 - t) * xr;
      doory[doors] := t * yp + (1 - t) * yr;
    end;
  end;
  if doors = 2 then
  begin
    Result := True;
    x1 := doorx[1]; y1 := doory[1];
    x2 := doorx[2]; y2 := doory[2];
  end;
end;


function SplitTriangle(c: MathFloat; tr: TD3Triangle; var tr1, tr2, tr3:
  TD3Triangle; var NewPoint1, NewPoint2: PD3FloatPoint): Boolean;
var
  t1, t2, xp, yp, p, xq, yq, q, xr, yr, r, x1, y1, x2, y2, epsilon: MathFloat;
begin
  Result := False;
  epsilon := 1.0E-12;
  if not (((c - tr.p.z) * (tr.q.z - c) > epsilon) or ((c - tr.p.z) * (tr.r.z - c) > epsilon)) then
    exit; //testing 2 is enough
  xp := tr.p.x; yp := tr.p.y; p := tr.p.z;
  xq := tr.q.x; yq := tr.q.y; q := tr.q.z;
  xr := tr.r.x; yr := tr.r.y; r := tr.r.z;
  if (c - p) * (q - c) > 0 then //sign change p-q
  begin
    t1 := (c - q) / (p - q);
    x1 := t1 * xp + (1 - t1) * xq;
    y1 := t1 * yp + (1 - t1) * yq;
    if (c - p) * (r - c) >= 0 then //sign change p-r
    begin
      if p = r then
        exit;
      t2 := (c - r) / (p - r);
      x2 := t2 * xp + (1 - t2) * xr;
      y2 := t2 * yp + (1 - t2) * yr;
      Result := True;
      tr1.p := tr.p;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.q;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.q;
      tr3.q := tr.r;
      tr3.r := NewPoint2;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end
    else //sign change must be q-r
    begin
      if r = q then
        exit;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      tr1.p := tr.q;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.p;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.r;
      tr3.r := NewPoint2;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end;
  end
  else
  begin
    if (c - p) * (r - c) > 0 then
      //sign change p-r which implies sign change q-r
    begin
      if p = r then
        exit;
      t1 := (c - r) / (p - r);
      x1 := t1 * xp + (1 - t1) * xr;
      y1 := t1 * yp + (1 - t1) * yr;
      if q = r then
        exit;
      if p = q then
        exit;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.p := tr.q;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.r;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.q;
      tr3.r := NewPoint1;
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end
    else
    begin
      //now sign change must be q-r, and c=p, so:
      x1 := xp; y1 := yp;
      t2 := (c - r) / (q - r);
      x2 := t2 * xq + (1 - t2) * xr;
      y2 := t2 * yq + (1 - t2) * yr;
      Result := True;
      New(NewPoint1);
      NewPoint1.x := x1;
      NewPoint1.y := y1;
      NewPoint1.z := c;
      New(NewPoint2);
      NewPoint2.x := x2;
      NewPoint2.y := y2;
      NewPoint2.z := c;
      tr1.p := tr.q;
      tr1.q := NewPoint1;
      tr1.r := NewPoint2;
      tr2.p := tr.r;
      tr2.q := NewPoint1;
      tr2.r := NewPoint2;
      tr3.p := tr.p;
      tr3.q := tr.q;
      tr3.r := NewPoint2;
      //still need to come up with 3 triangles, though it splits in 2
      tr1.FillColor := nil;
      tr2.FillColor := nil;
      tr3.FillColor := nil;
    end;
  end;
end;


procedure TWorldDrawing.DrawLevelLine(ACanvas: TCanvas;
  Triangle: TD3Triangle; Level: MathFloat);
var
  x1, y1, x2, y2: MathFloat;
begin
  if DoorInDoorOut(Level, Triangle.p.x, Triangle.p.y, Triangle.q.x, Triangle.q.y, Triangle.r.x, Triangle.r.y, Triangle.p.z, Triangle.q.z, Triangle.r.z, x1, y1, x2, y2) then
    DrawLine(ACanvas, x1, y1, x2, y2);
end;

procedure TWorldDrawing.DrawLevelLines(ACanvas: TCanvas;
  const Triangles: array of TD3Triangle; Level: MathFloat);
var i: Integer;
begin
  for i := 0 to High(Triangles) do
    DrawLevelLine(ACanvas, Triangles[i], Level);
end;


procedure TWorldDrawing.d3DrawLitLevelSurface(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray; const Levels: array of MathFloat;
  const Colors: array of TColor; diffuse, focussed: MathFloat);
var
  i, j,
    ColCount, SplitCount,
    TriangleCount, NewPointCount,
    TriangleLength, NewPointLength: Integer;
  Level: MathFloat;
  Triangles: TD3TriangleArray;
  NewPoints: array of PD3FloatPoint;
  NewPoint1, NewPoint2: PD3FloatPoint;
  tr1, tr2, tr3: TD3Triangle;
begin
  ColCount := High(Colors);
  if ColCount > High(Levels) then
    ColCount := High(Levels);
  GetTriangles(SurfArray, Triangles);
  TriangleCount := Length(Triangles);
  SetLength(Triangles, TriangleCount + 200);
  TriangleLength := Length(Triangles);
  SetLength(NewPoints, 200);
  NewPointLength := 200;
  NewPointCount := 0;
  i := 0;
  while i < TriangleCount do
  begin
    SplitCount := 0;
    for j := 0 to ColCount do
    begin
      if SplitTriangle(Levels[j], Triangles[i], tr1, tr2, tr3, NewPoint1, NewPoint2) then
      begin
        inc(SplitCount);
        if NewPointCount > NewPointLength - 2 then
        begin
          NewPointLength := NewPointLength + 100;
          SetLength(NewPoints, NewPointLength);
        end;
        NewPoints[NewPointCount] := NewPoint1;
        inc(NewPointCount);
        NewPoints[NewPointCount] := NewPoint2;
        inc(NewPointCount);
        if TriangleCount > TriangleLength - 2 then
        begin
          TriangleLength := TriangleLength + 100;
          SetLength(Triangles, TriangleLength);
        end;
        Triangles[i] := tr1;
        Triangles[TriangleCount] := tr2;
        inc(TriangleCount);
        Triangles[TriangleCount] := tr3;
        inc(TriangleCount);
      end
      else
        if SplitCount > 0 then break;
    end;
    inc(i);
  end;
  for i := 0 to TriangleCount - 1 do
    with Triangles[i] do
    begin
      Level := (p.z + q.z + r.z) / 3;
      for j := 0 to ColCount - 1 do
        if Levels[j] <= Level then
          if Levels[j + 1] >= Level then
          begin
            FillColor := @Colors[j];
            break;
          end;
      if Levels[ColCount] < Level then
        FillColor := @Colors[ColCount];
      if Levels[0] > Level then
        FillColor := @Colors[0];
    end;
  SetLength(Triangles, TriangleCount);
  d3DrawLitTriangles(ACanvas, Triangles, diffuse, focussed);
  for i := 0 to NewPointCount - 1 do
    dispose(NewPoints[i]);
end;

procedure TWorldDrawing.DrawProjection(ACanvas: TCanvas;
  Triangle: TD3Triangle);
var
  ptns: array[0..2] of TPoint;
  p: TFloatpoint;
begin
  with Triangle.p^ do
  begin
    FloatPoint(x, y, p);
    Convert(p, ptns[0]);
  end;
  with Triangle.q^ do
  begin
    FloatPoint(x, y, p);
    Convert(p, ptns[1]);
  end;
  with Triangle.r^ do
  begin
    FloatPoint(x, y, p);
    Convert(p, ptns[2]);
  end;
  if Triangle.FillColor <> nil then
  begin
    ACanvas.Brush.Color := Triangle.FillColor^;
    ACanvas.Pen.Style := psCLear;
    ACanvas.Polygon(ptns);
  end;
end;

procedure TWorldDrawing.DrawFilledLevelCurves(ACanvas: TCanvas;
  const SurfArray: array of Td3FloatPointArray; const Levels: array of MathFloat;
  const Colors: array of TColor);
var
  i, j,
    ColCount, SplitCount,
    TriangleCount, NewPointCount,
    TriangleLength, NewPointLength: Integer;
  Level: MathFloat;
  Done: Boolean;
  Triangles: TD3TriangleArray;
  NewPoints: array of PD3FloatPoint;
  NewPoint1, NewPoint2: PD3FloatPoint;
  tr1, tr2, tr3: TD3Triangle;
  SavePen: TPen;
  SaveBrush: TBrush;
begin
  SavePen := TPen.Create;
  SaveBrush := TBrush.Create;
  SavePen.assign(ACanvas.Pen);
  SaveBrush.assign(ACanvas.Brush);
  ColCount := High(Colors);
  if ColCount > High(Levels) then
    ColCount := High(Levels);
  GetTriangles(SurfArray, Triangles);
  TriangleCount := Length(Triangles);
  SetLength(Triangles, TriangleCount + 200);
  TriangleLength := Length(Triangles);
  SetLength(NewPoints, 200);
  NewPointLength := 200;
  NewPointCount := 0;
  i := 0;
  while i < TriangleCount do
  begin
    SplitCount := 0;
    for j := 0 to ColCount do
    begin
      if SplitTriangle(Levels[j], Triangles[i], tr1, tr2, tr3, NewPoint1, NewPoint2) then
      begin
        inc(SplitCount);
        if NewPointCount > NewPointLength - 2 then
        begin
          NewPointLength := NewPointLength + 100;
          SetLength(NewPoints, NewPointLength);
        end;
        NewPoints[NewPointCount] := NewPoint1;
        inc(NewPointCount);
        NewPoints[NewPointCount] := NewPoint2;
        inc(NewPointCount);
        if TriangleCount > TriangleLength - 2 then
        begin
          TriangleLength := TriangleLength + 100;
          SetLength(Triangles, TriangleLength);
        end;
        Triangles[i] := tr1;
        Triangles[TriangleCount] := tr2;
        inc(TriangleCount);
        Triangles[TriangleCount] := tr3;
        inc(TriangleCount);
      end
      else
        if SplitCount > 0 then break;
    end;
    inc(i);
  end;
  for i := 0 to TriangleCount - 1 do
    with Triangles[i] do
    begin
      Done := False;
      Level := 0.33333333333 * (p.z + q.z + r.z);
      for j := 0 to ColCount - 1 do
      begin
        if Levels[j] <= Level then
          if Level <= Levels[j + 1] then
          begin
            FillColor := @Colors[j];
            Done := True;
            break;
          end;
      end;
      if not Done then
      begin
        if Level >= Levels[ColCount] then
          FillColor := @Colors[ColCount]
        else
          if Level <= Levels[0] then
            FillColor := @Colors[0];
      end;
    end;
  for i := 0 to TriangleCount - 1 do
    DrawProjection(ACanvas, Triangles[i]);
  for i := 0 to NewPointCount - 1 do
    dispose(NewPoints[i]);
  ACanvas.Pen.assign(SavePen);
  ACanvas.Brush.assign(SaveBrush);
  SavePen.Free;
  SaveBrush.Free;
end;

procedure TWorldDrawing.DrawProjections(ACanvas: TCanvas;
  const Triangles: array of TD3Triangle);
var
  i: Integer;
  SaveBrush: TBrush;
  SavePen: TPen;
begin
  SaveBrush := TBrush.Create;
  SavePen := TPen.Create;
  SaveBrush.assign(ACanvas.Brush);
  SavePen.assign(ACanvas.Pen);
  for i := 0 to High(Triangles) do
    DrawProjection(ACanvas, Triangles[i]);
  ACanvas.Brush.assign(SaveBrush);
  ACanvas.Pen.assign(SavePen);
  SaveBrush.Free;
  SavePen.Free;
end;

//need to do some "range checking" for safety. It's too cumbersome
  //for the user to check whether the floats are in a safe region
  //for mapping. 22000 is roughly the number at which line drawing
  //doesn't work anymore. Internal procedures are now Convert and
  //WorldToScreen, for speed.

function TWorldDrawing.Windowx(x: MathFloat): longint;
var Temp: MathFloat;
begin
  Temp := bx + ax * x;
  if Temp < -22000 then Result := -22000 else if Temp > 22000 then
    Result := 22000 else Result := round(Temp);
end;

function TWorldDrawing.Windowy(y: MathFloat): longint;
var Temp: MathFloat;
begin
  Temp := by + ay * y;
  if Temp < -22000 then Result := -22000 else if Temp > 22000 then
    Result := 22000 else Result := round(Temp);
end;



procedure TWorldDrawing.Convert(const src: TFloatpoint; var dest: TPoint);
var
  Temp: MathFloat;
begin
  Temp := bx + ax * src.x;
  if Temp < -22000 then dest.x := -22000 else if Temp > 22000 then
    dest.x := 22000 else
    dest.x := round(Temp);
  Temp := by + ay * src.y;
  if Temp < -22000 then dest.y := -22000 else if Temp > 22000 then
    dest.y := 22000 else
    dest.y := round(Temp);
end;


procedure TWorldDrawing.WorldToScreen(const x, y: MathFloat; var xs, Ys: Integer);
var Temp: MathFloat;
begin
  Temp := bx + ax * x;
  if Temp < -22000 then xs := -22000 else if Temp > 22000 then
    xs := 22000 else xs := round(Temp);
  Temp := by + ay * y;
  if Temp < -22000 then Ys := -22000 else if Temp > 22000 then
    Ys := 22000 else Ys := round(Temp);
end;


procedure TWorldDrawing.DrawLineSegments(ACanvas: TCanvas;
  l: Td3LineSegmentArray);
var i: Integer; savecolor: TColor;
begin
  savecolor := ACanvas.Pen.Color;
  SortLineSegments(l);
  for i := 0 to High(l) do
  begin
    ACanvas.Pen.Color := l[i].Color;
    with l[i] do
      d3DrawLine(ACanvas, p.x, p.y, p.z, q.x, q.y, q.z);
  end;
  ACanvas.Pen.Color := savecolor;
end;

procedure TWorldDrawing.d3DrawBestAxes(ACanvas: TCanvas; xLabel, yLabel,
  zLabel: string; xTicks, yTicks, zTicks: byte; Arrows: Boolean = True);
begin
  d3DrawBaseAxes(ACanvas, xLabel, yLabel, zLabel, xTicks, yTicks, zTicks,
    fronty, basez, frontx, basez, frontx, basey, Arrows);
end;

{ TLightSource }

function TLightSource.GetYRot: Integer;
begin
  Result := round(fyrot * 180 / pi);
end;

function TLightSource.GetZRot: Integer;
begin
  Result := round(fzrot * 180 / pi);
end;

procedure TLightSource.InitSourcePoint;
begin
  if not fFixed then
    with fViewAngles do
      D3FloatPoint(fdist * cos(x + fzrot) * sin(y + fyrot), fdist * sin(x + fzrot) * sin(y + fyrot),
        fdist * cos(y + fyrot), fSourcePoint);
end;

procedure TLightSource.SetDist(Value: MathFloat);
begin
  if Value > 0 then
  begin
    fdist := Value;
    InitSourcePoint;
  end;
end;


procedure TLightSource.SetViewAngles(Value: TFloatpoint);
begin
  fViewAngles := Value;
  InitSourcePoint;
end;

procedure TLightSource.SetYRot(Value: Integer);
begin
  if Value <= 90 then
    if Value >= -90 then
    begin
      fyrot := 1 / 180 * pi * Value;
      InitSourcePoint;
    end;
end;

procedure TLightSource.SetZRot(Value: Integer);
begin
  if Value <= 180 then
    if Value >= -180 then
    begin
      fzrot := 1 / 180 * pi * Value;
      InitSourcePoint;
    end;
end;

end.

