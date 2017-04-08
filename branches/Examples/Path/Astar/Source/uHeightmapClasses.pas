unit uHeightmapClasses;
//CMSnow.SingleColor := clWhite32;  CMSnow.SetupCurve(0.9,0.95,2,2);
//CMDirt.SingleColor := clRed32;  CMDirt.SetupCurve(0.5,0.6,0.9,0.95);
//CMGrass.SingleColor := clGreen32;  CMGrass.SetupCurve(0.2,0.3,0.5,0.6);
//CMSand.SingleColor := clYellow32;  CMSand.SetupCurve(-2,-1,0.2,0.3);
//SandPanel
 //function WinColor(Color32: TColor32): TColor;
//function Color32(WinColor: TColor): TColor32; overload;
interface

uses
  Windows, Graphics, Classes,
  Math, //max min
  GR32, GLVectorTypes, GLVectorGeometry, uGrid, Dialogs,
  SysUtils;

type
  TRGBTripleArray = array [0..32767] of TRGBTriple;
  THeightmap = class
  private
    FSizeX, FSizeY : integer;
    FBitmap  : TBitmap;

//    function GetHeightSafe(x, y: integer): single;
    function GetNormal(x, y: single): TD3DVector;
  public
    Scale, SunDirection : TD3DVector;

    Height : TSingleGrid;
    Slope : TSingleGrid;
    Random2D : TSingleGrid;
    Shadow : TSingleGrid;
    Shade : TSingleGrid;

    property Normal[x,y : single] : TD3DVector read GetNormal;

    property SizeX : integer read FSizeX;
    property SizeY : integer read FSizeY;

    procedure NewRandomValues;

    constructor Create(SizeX, SizeY : integer);
    destructor Destroy;override; //

    function IndexFromXY(x,y : integer) : integer;

    // Interpolations
    function Interpolate(a, b, x : single) : single;
    function InterpolateLinear(a, b, x : single) : single;
    function InterpolateCos(a, b, x : single) : single;

    procedure AddNoise(Frequency : single; Amplitude : single);
    procedure Subdivide(Lines : integer; DeltaHeight : single);
    procedure ClampToLevel(Level : single);
    procedure MakeIsland(Margin : single);
    procedure Rescale(NewMin, NewMax : single);

    procedure LoadFromFile(fileName : string);

    procedure GetHeightExtremes(var MaxHeight, MinHeight : single);
    procedure BoxLower(x,y : integer; Depth, Radius : single);
    procedure CircleLower(x,y : integer; Depth, Radius : single);

    procedure SetupSlope(Threshhold : single);
    function CalcSlope(x,y : integer) : single;
    function GetSlopeMax : single;
    function GetSlopeMin: single;

    function CalcShadeFor(x, y: integer): single;
    procedure SetupShade;

    function CalcShadowFor(x,y : integer; MaxHeight : single) : single;
    procedure SetupShadows;

    procedure RenderTo(Bitmap32: TBitmap32; Water : boolean;WaterLevel : single=0.5);
    procedure RenderSlopesTo(Bitmap : TBitmap32);
    procedure RenderShadedTo(Bitmap : TBitmap32; Ambient : single=0);
    procedure RenderShadowTo(Bitmap : TBitmap32);
  end;

  TColorModifier = class
    Bitmap : TBitmap32;
    SingleColor : TColor32;

    function GetPixel(x,y : integer) : TColor32;
    constructor Create(FileName : string);
    destructor Destroy;override;

    procedure GetColorAndLerp(x,y : integer; Height, Slope : single; var outColor32 : TColor32; var Lerp : single); virtual;
  end;

  TColorModifierSlope = class(TColorModifier)
    procedure GetColorAndLerp(x,y : integer; Height, Slope : single; var outColor32 : TColor32; var Lerp : single); override;
  end;

  TColorModifierHeight = class(TColorModifier)
    StartHeight : single;
    FullOnHeight : single;
    StopHeight : single;
    FullOffHeight : single;

    procedure SetupCurve(StartHeight, FullOnHeight, StopHeight, FullOffHeight : single);

    procedure GetColorAndLerp(x,y : integer; Height, Slope : single; var outColor32 : TColor32; var Lerp : single); override;
  end;

  TColorModifierHandler = class
  private
    FColorModifierList : TList;
    FHeightToBitmapX : single;
    FHeightToBitmapY : single;
    FDrawingX, FDrawingY : integer;
    FNoTexture: boolean;

    function GetColorModifier(i: integer): TColorModifier;
    procedure SetNoTexture(const Value: boolean);
  public
    Heightmap : THeightmap;
    Shaded : boolean;
    Ambient : single;

    procedure Add(ColorModifier : TColorModifier);
    function Count : integer;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;

    function LerpColor32(Color1, Color2 : TColor32; x : single) : TColor32;
    procedure RenderToBitmap(Bitmap : TBitmap32);
    function GetColor(x,y : single) : TColor32;

procedure RenderBaseToBitmap(Bitmap: TBitmap32);
function GetBaseColor(x, y: single): TColor32;

    property ColorModifier[i : integer] : TColorModifier read GetColorModifier;
    property NoTexture : boolean read FNoTexture write SetNoTexture;
  end;

implementation

{ THeightmap }

procedure THeightmap.AddNoise(Frequency, Amplitude: single);
var
  x, y : integer;
  px, py : single;
begin
  NewRandomValues;
  for x := 0 to FSizeX-1 do
  begin
    px := x/FSizeX;
    for y := 0 to FSizeY-1 do
    begin
      py := y/FSizeY;
      Height[x,y] := Height[x,y] + Random2D.InterpolatedCos[px*Frequency,py*Frequency]*Amplitude;
    end;
  end;
end;

procedure THeightmap.ClampToLevel(Level: single);
var
  i : integer;
begin
  for i := 0 to Height.TotalSize-1 do
    if Height.Storage[i]<Level then
      Height.Storage[i] := Level;
end;

constructor THeightmap.Create(SizeX, SizeY: integer);
begin
  FSizeX := SizeX;
  FSizeY := SizeY;

  Height := TSingleGrid.Create(FSizeX, FSizeY);
  Random2D := TSingleGrid.Create(FSizeX, FSizeY);
  Slope := TSingleGrid.Create(FSizeX, FSizeY);
  Shadow := TSingleGrid.Create(FSizeX, FSizeY);
  Shade := TSingleGrid.Create(FSizeX, FSizeY);

  Height.Clear;

  FBitmap:=TBitmap.Create;
  FBitmap.PixelFormat:=pf24bit;
  FBitmap.Width :=SizeX;
  FBitmap.Height:=SizeY;

  Scale.X := 1;
  Scale.Y := 1;
  Scale.Z := FSizeX * 0.1;

  // 10% up and down!
  // 100m x 100m => 10m up/down
  //

  SunDirection.X := 1;
  SunDirection.Y := 1;
  SunDirection.Z := 0.5;

  NormalizeVector(SunDirection.v);
end;

destructor THeightmap.Destroy;
begin
  Height.Free;
  Random2D.Free;
  Slope.Free;
  Shadow.Free;
  Shade.Free;
end;

procedure THeightmap.GetHeightExtremes(var MaxHeight, MinHeight: single);
var
  i : integer;
  f : single;
begin
  MaxHeight := -10000;
  MinHeight := 10000;
  for i := 0 to Height.TotalSize-1 do
  begin
    f := Height.Storage[i];

    MaxHeight := Max(MaxHeight, f);
    MinHeight := Min(MinHeight, f);
  end;

  if MaxHeight = MinHeight then
  begin
    MinHeight := 0;
    MaxHeight := 1;
  end;
end;

procedure THeightmap.BoxLower(x, y: integer; Depth, Radius: single);
var
  gx, gy, iRadius : integer;
  //px, py, dist2 : single;
  //Radius2 : single;
  //f, ix : single;
  fMax, fMin : single;
begin
  GetHeightExtremes(fMax, fMin);
  //Radius2 := sqr(Radius);

  iRadius := trunc(Radius);

  for gx := x-iRadius to x+iRadius do
  begin
    if gx<0 then continue;
    if gx>=FSizeX then continue;

    for gy := y-iRadius to y+iRadius do
    begin
      if gy<0 then continue;
      if gy>=FSizeY then continue;

      Height[gx,gy] := Height[gx,gy]+Depth;
    end;
  end;
end;

procedure THeightmap.CircleLower(x,y : integer; Depth, Radius : single);
var
  gx, gy, iRadius : integer;
 // px, py,
  dist2 : single;
  Radius2 : single;
  f, ix : single;
  fMax, fMin : single;
begin
  GetHeightExtremes(fMax, fMin);
  Radius2 := sqr(Radius);

  iRadius := trunc(Radius);

  for gx := x-iRadius to x+iRadius do
  begin
    if gx<0 then continue;
    if gx>=FSizeX then continue;

    for gy := y-iRadius to y+iRadius do
    begin
      if gy<0 then continue;
      if gy>=FSizeY then continue;

      Dist2 := sqr(x-gx)+sqr(y-gy);

      if Dist2>=Radius2 then
        Continue;

      f := Height[gx,gy];

      ix := sqrt(Dist2)/Radius;

      f := max(fMin, InterpolateCos(f+depth, f, ix));

      Height[gx,gy] := f;
    end;
  end;
end;

function THeightmap.Interpolate(a, b, x: single): single;
begin
  //result := InterpolateLinear(a,b,x);
  result := InterpolateCos(a,b,x);
end;

{ function Cosine_Interpolate(a, b, x)
	ft = x * 3.1415927
	f = (1 - cos(ft)) * .5

	return  a*(1-f) + b*f
  end of function//}

function THeightmap.InterpolateCos(a, b, x: single): single;
var
  ft,f : single;
begin
  ft := x * Pi;
  f := (1-cos(ft))*0.5;

  result := a*(1-f)+b*f;
end;

function THeightmap.InterpolateLinear(a, b, x: single): single;
begin
  result := a*(1-x)+b*x;
end;

{  function Noise1(integer x, integer y)
    n = x + y * 57
    n = (n<<13) ^ n;
    return ( 1.0 - ( (n * (n * n * 15731 + 789221) + 1376312589) & 7fffffff) / 1073741824.0);
  end function//}

procedure THeightmap.MakeIsland(Margin : single);
var
  x, y : integer;
  px, py : single;
  maxx, maxy : single;
  pmax : single;

  BottomMarginY, TopMarginY,
  LeftMarginX, RightMarginX : single;
begin
  LeftMarginX := Margin*FSizeX;
  RightMarginX := FSizeX-Margin*FSizeX;

  TopMarginY := Margin*FSizeY;
  BottomMarginY := FSizeY-Margin*FSizeY;

  GetHeightExtremes(MaxX, MaxY);

  for x := 0 to FSizeX-1 do
  begin
    if x<LeftMarginX then
      px := x/LeftMarginX
    else if x>RightMarginX then
      px := 1 - (x-RightMarginX)/LeftMarginX
    else
      px := 1;

    for y := 0 to FSizeY-1 do
    begin
      if y<TopMarginY then
        py := y/TopMarginY
      else if y>BottomMarginY then
        py := 1-(y-BottomMarginY)/TopMarginY
      else
        py := 1;

      pmax := min(px,py);

      Height[x,y] := InterpolateCos(MaxY, Height[x,y],  pmax);
    end;
  end;//}
end;

procedure THeightmap.NewRandomValues;
var
  i : integer;
begin
  // Create a lot of random values for the perlin noise
  for i := 0 to Random2D.TotalSize-1 do
    Random2D.Storage[i] := 2*random-1;
end;

procedure THeightmap.RenderTo(Bitmap32: TBitmap32; Water : boolean; WaterLevel : single=0.5);
var
  f : single;
  b : byte;
  x,y : integer;
  fMax, fMin : single;
//  AdjustedWaterLevel : single;
begin
  GetHeightExtremes(fMax, fMin);

//  AdjustedWaterLevel := (WaterLevel-fMin)/(fMax-fMin);

  Bitmap32.Clear;

  for y := 0 to FSizey-1 do
  begin
    for x := 0 to FSizeX-1 do
    begin
      // Make sure that we get a value in the range 0 to 1
      f := (Height[x,y]-fMin)/(fMax-fMin);

      if not Water then
      begin
        b := trunc(f*255);
        Bitmap32.Pixel[x,y] := Color32(b,b,b);
      end else
      if Height[x,y]<=WaterLevel then
      begin
        b := trunc(f*255);
        Bitmap32.Pixel[x,y] := Color32(255,b,b);
      end else
      begin
        f := (Height[x,y]-WaterLevel)/(fMax-WaterLevel);
        b := trunc(f*255);
        Bitmap32.Pixel[x,y] := Color32(b,b,b);
      end;//}
    end;
  end;
end;

procedure THeightmap.Rescale(NewMin, NewMax: single);
var
  x,y : integer;
  f, fMax, fMin : single;
begin
  GetHeightExtremes(fMax, fMin);

  for y := 0 to FSizey-1 do
  begin
    for x := 0 to FSizeX-1 do
    begin
      // Make sure that we get a value in the range 0 to 1
      f := (Height[x,y]-fMin)/(fMax-fMin);

      f := f*(NewMax-NewMin) + NewMin;

      Height[x,y] := f;
    end;
  end;
end;

function THeightmap.CalcSlope(x, y: integer): single;
var
  ThisHeight : single;
  Slope : single;
  gx, gy : integer;
begin
  // Slope = the highest delta height of the eight neighbours of this node!
  ThisHeight := Height[x,y];

  Slope := 0;

  for gx := x-1 to x+1 do
  begin
    if gx<0 then continue;
    if gx>=FSizeX then continue;

    for gy := y-1 to y+1 do
    begin
      if gy<0 then continue;
      if gy>=FSizeY then continue;

      Slope := max(Slope, abs(ThisHeight-Height[gx, gy]));
    end;
  end;

  result := Slope;
end;

procedure THeightmap.SetupSlope(Threshhold : single);
var
 // i,
  x, y : integer;
  f : single;
  fMaxSlope, fNewMaxSlope : single;
begin
  Slope.Clear;
//  f := 0;
  fMaxSlope := 0;
  fNewMaxSlope := 0;

  for y := 0 to FSizey-1 do
  begin
    for x := 0 to FSizeX-1 do
    begin
      f := CalcSlope(x,y);
      fMaxSlope := max(abs(f), fMaxSlope);
      Slope[x,y] := f;
    end;
  end;
  //showmessage(Floattostr(fMaxSlope));//0.05...
  for y := 0 to FSizey-1 do
  begin
    for x := 0 to FSizeX-1 do
    begin
      f := Slope[x,y]/fMaxSlope;

      if f<Threshhold then
        f := 0
      else
        f := (f-Threshhold)/(1-Threshhold);

      Slope[x,y] := f;

      fNewMaxSlope := max(fNewMaxSlope, f);
    end
  end;
end;

function THeightmap.IndexFromXY(x, y: integer): integer;
begin
  result := x+y*SizeX;
end;

procedure THeightmap.RenderSlopesTo(Bitmap : TBitmap32);
var
  f : single;
  x, y : integer;
  hx, hy : single;
  b : byte;
//  ThisNormal : TD3DVector;
  FHeightToBitmapX, FHeightToBitmapY : single;
begin
  Bitmap.Clear(Color32(0,0,0));

  FHeightToBitmapX := (SizeX/Bitmap.Width);
  FHeightToBitmapY := (SizeY/Bitmap.Height);

  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      hx := x * FHeightToBitmapX;
      hy := y * FHeightToBitmapY;

      f := Slope.InterpolatedCos[hx, hy];

      b := trunc(255*f);

      Bitmap.Pixels[x,y] := Color32(b,b,b);
    end;
  end;
end;

function THeightmap.GetSlopeMax: single;
var
  i : integer;
begin
  result := 0;
  for i := 0 to Slope.TotalSize-1 do
    result := max(result, single(Slope.Storage[i]));
end;

function THeightmap.GetSlopeMin: single;
var
  i : integer;
begin
  result := 1;//0;
  for i := 0 to Slope.TotalSize-1 do
    result := min(result, single(Slope.Storage[i]));
end;
{function THeightmap.GetHeightSafe(x, y: integer): single;
begin
  if x<0 then x :=0;
  if x>=FSizeX then x:=FSizeX-1;

  if y<0 then y :=0;
  if y>=FSizeY then y:=FSizeY-1;

  result := Height[x,y];
end; }

procedure THeightmap.RenderShadedTo(Bitmap : TBitmap32; Ambient : single=0);
var
  f : single;
  x, y : integer;
  hx, hy : single;
  b : byte;
//  ThisNormal : TD3DVector;
  FHeightToBitmapX, FHeightToBitmapY : single;
begin
  Bitmap.Clear(Color32(0,0,0));

  FHeightToBitmapX := (SizeX/Bitmap.Width);
  FHeightToBitmapY := (SizeY/Bitmap.Height);

  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      hx := x * FHeightToBitmapX;
      hy := y * FHeightToBitmapY;

      f := (1-Ambient) * Shade.InterpolatedCos[hx, hy] + Ambient;

      b := trunc(255*f);

      Bitmap.Pixels[x,y] := Color32(b,b,b);
    end;
  end;
end;

function THeightmap.GetNormal(x, y: single): TD3DVector;
  function GetNormalHelper(x,y : integer) : TD3DVector;
  var
     dx, dy : Single;
  begin
    if (x>=SizeX) or (y>=SizeY) or (x<0) or (y<0) then
    begin
      result.x:=0;
      result.x:=0;
      result.z:=1;

      exit;
    end;

    if x>0 then
      if x<SizeX-1 then
         dx:=(Height[x+1, y]-Height[x-1, y])*scale.v.X*scale.v.Z
      else dx:=(Height[x, y]-Height[x-1, y])*scale.v.X*scale.v.Z
    else dx:=(Height[x+1, y]-Height[x, y])*scale.v.X*scale.v.Z;
    if y>0 then
      if y<SizeY-1 then
         dy:=(Height[x, y+1]-Height[x, y-1])*scale.v.Y*scale.v.Z
      else dy:=(Height[x, y]-Height[x, y-1])*scale.v.Y*scale.v.Z
    else
    dy:=(Height[x, y+1]-Height[x, y])*scale.v.Y*scale.v.Z;
    Result.v.X:=dx;
    Result.v.Y:=dy;
    Result.v.Z:=1;
    NormalizeVector(Result.v);
  end;
var
  intX, intY : integer;
  fractX, fractY : single;
  n1, n2, n3, n4, i1, i2 : TD3DVector;
begin
  intX := trunc(x); fractX := x-intX;
  intY := trunc(y); fractY := y-intY;

//  result := GetNormalHelper(intX, intY);

  n1 := GetNormalHelper(intX, intY);
  n2 := GetNormalHelper(intX, intY+1);
  n3 := GetNormalHelper(intX+1, intY);
  n4 := GetNormalHelper(intX+1, intY+1);

  n1 := GetNormalHelper(intX, intY);
  n2 := GetNormalHelper(intX+1, intY);
  n3 := GetNormalHelper(intX, intY+1);
  n4 := GetNormalHelper(intX+1, intY+1);

  i1.v := VectorLerp(n1.v, n2.v, fractX);
  i2.v := VectorLerp(n3.v, n4.v, fractX);

  result.v := VectorLerp(i1.v, i2.v, fractY);//}
end;

function THeightmap.CalcShadeFor(x, y: integer): single;
var
  ThisNormal : TD3DVector;
  f : single;
begin
  ThisNormal := Normal[x, y];

  f := VectorDotProduct(ThisNormal.v, SunDirection.v);

  if f<0 then f := 0;
  if f>1 then f := 1;//}

  result := f;
end;

procedure THeightmap.Subdivide(Lines: integer; DeltaHeight: single);
  procedure RandomKAndM(var k,m : single);
  var
    x1,y1,
    x2,y2 : single;
  begin
    repeat
      x1 := random(FSizeX);
      y1 := random(FSizeY);

      x2 := random(FSizeX);
      y2 := random(FSizeY);
    until x1<>x2;

    k := (y1-y2)/(x1-x2);
    m := y1-k*x1;
  end;
var
  k,m : single;
  i,x,y : integer;
  Left : boolean;
  Elevate : boolean;
begin
  for i := 0 to Lines-1 do
  begin
    RandomKAndM(k,m);
    Left := random>0.5;

    for x := 0 to FSizeX-1 do
      for y := 0 to FSizeY-1 do
      begin
        Elevate := (x>k*y+m);

        if Left then
          Elevate := not Elevate;

        if Elevate then
          Height[x,y] := Height[x,y]+DeltaHeight
        else
          Height[x,y] := Height[x,y]-DeltaHeight
      end;
  end;
end;

procedure THeightmap.LoadFromFile(fileName: string);
var
  Bitmap32 : TBitmap32;
  x,y : integer;
  Color : TColor32;
 // i,c : byte;
  BitArray : set of Byte;
begin
  Bitmap32 := TBitmap32.Create;
  Bitmap32.SetSize(SizeX, SizeY);

  try
    Bitmap32.LoadFromFile(fileName);

    for x := 0 to SizeX-1 do
      for y := 0 to SizeY-1 do
      begin
        Color := Bitmap32.Pixels[x,y];
        Height[x,y] := RedComponent(Color) / 255;

        BitArray := BitArray + [RedComponent(Color)];
      end;

   { c := 0;
    for i := 0 to 255 do
      if i in BitArray then inc(c); }
    //ShowMessage(inttostr(c));

  finally
    Bitmap32.Free;
  end;
end;

procedure THeightmap.RenderShadowTo(Bitmap: TBitmap32);
var
  f : single;
  x, y : integer;
  hx, hy : single;
  b : byte;
//  ThisNormal : TD3DVector;
  FHeightToBitmapX, FHeightToBitmapY : single;
begin
  Bitmap.Clear(Color32(0,0,0));

  FHeightToBitmapX := (SizeX/Bitmap.Width);
  FHeightToBitmapY := (SizeY/Bitmap.Height);

  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      hx := x * FHeightToBitmapX;
      hy := y * FHeightToBitmapY;

      //f := (1-Ambient) * Shade[hx, hy] + Ambient;
      f :=  1-Shadow.InterpolatedCos[hx, hy];

      b := trunc(255*f);

      Bitmap.Pixels[x,y] := Color32(b,b,b);
    end;
  end;
end;

procedure THeightmap.SetupShadows;
var
  maxh, minh : single;
  x, y : integer;
begin
  GetHeightExtremes(maxh, minh);

  for x := 0 to FSizeX-1 do
    for y := 0 to FSizeY-1 do
      Shadow[x,y] := CalcShadowFor(x,y, maxh);//}

  Shadow.AntiAlias(0.1);
  Shadow.AntiAlias(0.1);
  Shadow.AntiAlias(0.1);
end;

function THeightmap.CalcShadowFor(x, y: integer; MaxHeight : single): single;
var
  k1,k2 : single;
  Move : TD3DVector;
  GroundHeight : single;
  MaxGroundHeight : single;
  CurrentRayHeight : single;
  Ray : TD3DVector;
  rx, ry : integer;
  ThisHeight : single;
  function IsInRange : boolean;
  begin
    result := Shadow.InRange(trunc(x+Move.x), trunc(y+Move.y));

    if CurrentRayHeight > MaxGroundHeight then
      result := false;//}
  end;
begin
  result := 0;

  Ray := SunDirection;
  ScaleVector(Ray.v,-1);

  GroundHeight := Height[x,y] * Scale.z;
  MaxGroundHeight := MaxHeight * Scale.z;

  // Walk the X line
  MakeVector(Move.v,0,0,0);
  if Ray.x<>0 then
  begin
    k1 := Ray.y/Ray.x;
    k2 := Ray.z/Ray.x;

    while IsInRange do
    begin
      if Ray.x>0 then
        Move.x := Move.x + 1
      else
        Move.x := Move.x - 1;

      Move.y := Move.x * k1;
      Move.z := Move.x * k2;

      rx := trunc(x+Move.x);
      ry := trunc(y+Move.y);

      if x=rx then
        continue;

      if not IsInRange then
        break;

      ThisHeight := Height[rx, ry] * Scale.z;
      CurrentRayHeight := GroundHeight-Move.z;

      // Is the horizon below the sunray?
      if ThisHeight>CurrentRayHeight then
      begin
        result := 1;
        exit;
      end;
    end;
  end;//}


  // Walk the Y line
  MakeVector(Move.v,0,0,0);
  if Ray.y<>0 then
  begin
    k1 := Ray.x/Ray.y;
    k2 := Ray.z/Ray.y;

    while IsInRange do
    begin
      if Ray.y>0 then
        Move.y := Move.y + 1
      else
        Move.y := Move.y - 1;

      Move.x := Move.y * k1;
      Move.z := Move.y * k2;

      rx := trunc(x+Move.x);
      ry := trunc(y+Move.y);

      if y = ry then
        continue;

      if not IsInRange then
        break;

      ThisHeight := Height[rx, ry] * Scale.z;
      CurrentRayHeight := GroundHeight-Move.z;

      // Is the horizon below the sunray?
      if ThisHeight>CurrentRayHeight then
      begin
        result := 1;
        exit;
      end;
    end;
  end;//}
end;

procedure THeightmap.SetupShade;
var
//  maxh, minh : single;
  x, y : integer;
begin
  for x := 0 to FSizeX-1 do
    for y := 0 to FSizeY-1 do
      Shade[x,y] := CalcShadeFor(x,y);//}

  Shade.AntiAlias(0.5);
{  Shade.AntiAlias(0.5);
  Shade.AntiAlias(0.1);
  Shade.AntiAlias(0.1);//}
end;


{ TColorModifier }

constructor TColorModifier.Create(FileName: string);
begin
  if FileName<>'' then
  begin
    Bitmap := TBitmap32.Create;
    Bitmap.LoadFromFile(FileName);
  end;
end;

destructor TColorModifier.Destroy;
begin
  Bitmap.Free;

  inherited;
end;

procedure TColorModifier.GetColorAndLerp(x, y: integer; Height,
  Slope: single; var outColor32: TColor32; var Lerp: single);
begin
  outColor32 := Color32(trunc(255*Height), trunc(255*Height), trunc(255*Height));
  Lerp := 1;
  //result := Color32(trunc(255*Slope), trunc(255*Slope), trunc(255*Slope));
  //Result := ColorModifier[0].GetPixel(FDrawingX,FDrawingY)
end;

function TColorModifier.GetPixel(x, y: integer): TColor32;
var
  modY, modX : integer;
begin
  if Bitmap=nil then
  begin
    result := SingleColor;
  end else
  begin
    modY := y mod Bitmap.Height;
    modX := x mod Bitmap.Width;

    result := Bitmap.Pixel[modx, mody];
  end;
end;

{ TColorModifierHandler }

procedure TColorModifierHandler.Add(ColorModifier: TColorModifier);
begin
  FColorModifierList.Add(ColorModifier);
end;

procedure TColorModifierHandler.Clear;
var
  i : integer;
begin
  for i := 0 to Count-1 do
    ColorModifier[i].Free;

  FColorModifierList.Clear;
end;

function TColorModifierHandler.Count: integer;
begin
  result := FColorModifierList.Count;
end;

constructor TColorModifierHandler.Create;
begin
  Ambient := 0.1;
  Shaded := false;
  FColorModifierList := TList.Create;
end;

destructor TColorModifierHandler.Destroy;
begin
  Clear;
  FColorModifierList.Free;

  inherited;
end;

function TColorModifierHandler.GetColor(x, y: single): TColor32;
var
  i : integer;
  Height : single;
  Slope : single;
//  MustLerp : boolean;
  resColor32 : TColor32;
  newColor32 : TColor32;
  Lerp : single;
  LerpSum : single;
  CM : TColorModifier;
begin
  Slope := HeightMap.Slope[trunc(x), trunc(y)];
  //Height := HeightMap.Height[trunc(x), trunc(y)];
  Height := HeightMap.Height.InterpolatedCos[x,y];

  resColor32 := Color32(0,0,0);
//  MustLerp := false;
  LerpSum := 0;

  if Height=0.6 then
    LerpSum:=0;

  for i := 0 to Count-1 do
  begin
    CM := ColorModifier[i];
    CM.GetColorAndLerp(FDrawingX, FDrawingY, Height, Slope, newColor32, Lerp);

    LerpSum := LerpSum + Lerp;

    if NoTexture then
      newColor32 := CM.SingleColor;

    resColor32 := LerpColor32(newColor32, resColor32, Lerp);
  end;
  result := resColor32;

  Assert(LerpSum>0, Format('Height %f not handled!',[Height]));
end;

procedure TColorModifierHandler.RenderBaseToBitmap(Bitmap: TBitmap32);
var
  x, y : integer;
  hx, hy : single;
  Color : TColor32; //  Shade : single;   ShadowShade : single;
begin
  Bitmap.Clear(clBlack32);

  FHeightToBitmapX := (HeightMap.SizeX/Bitmap.Width);
  FHeightToBitmapY := (HeightMap.SizeY/Bitmap.Height);

  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      FDrawingX := x;
      FDrawingY := y;

      hx := FDrawingX * FHeightToBitmapX;
      hy := FDrawingY * FHeightToBitmapY;

      Color := GetBaseColor(hx,hy);

      Bitmap.Pixel[FDrawingX,FDrawingY] := Color;
    end;
  end;
end;

function TColorModifierHandler.GetBaseColor(x, y: single): TColor32;
var
  i : integer;
  Height : single;
  Slope : single;
//  MustLerp : boolean;
  resColor32 : TColor32;
  newColor32 : TColor32;
  Lerp : single;
  LerpSum : single;
  CM : TColorModifier;
begin
  Slope := HeightMap.Slope[trunc(x), trunc(y)];
  //Height := HeightMap.Height[trunc(x), trunc(y)];
  Height := HeightMap.Height.InterpolatedCos[x,y];

  resColor32 := Color32(0,0,0);
//  MustLerp := false;
  LerpSum := 0;

  if Height=0.6 then
    LerpSum:=0;

  for i := 0 to Count-1 do
  begin
    CM := ColorModifier[i];
    CM.GetColorAndLerp(FDrawingX, FDrawingY, Height, Slope, newColor32, Lerp);

    LerpSum := LerpSum + Lerp;

    if NoTexture then
      newColor32 := CM.SingleColor;

    //resColor32 := LerpColor32(newColor32, resColor32, Lerp);
    resColor32 := newColor32;
  end;
  result := resColor32;

  Assert(LerpSum>0, Format('Height %f not handled!',[Height]));
end;


function TColorModifierHandler.GetColorModifier(
  i: integer): TColorModifier;
begin
  result := TColorModifier(FColorModifierList[i]);
end;

function TColorModifierHandler.LerpColor32(Color1, Color2: TColor32;
  x: single): TColor32;
var
  r,g,b : byte;
begin
  Assert(x>=0,'Lerp x must be >= 0');
  Assert(x<=1,'Lerp x must be <= 1');
  r := trunc(RedComponent(Color1)*x+RedComponent(Color2)*(1-x));
  g := trunc(GreenComponent(Color1)*x+GreenComponent(Color2)*(1-x));
  b := trunc(BlueComponent(Color1)*x+BlueComponent(Color2)*(1-x));

  result := Color32(r,g,b);
end;

procedure TColorModifierHandler.RenderToBitmap(Bitmap: TBitmap32);
var
  x, y : integer;
  hx, hy : single;
  Color : TColor32;
  Shade : single;
  ShadowShade : single;
begin
  Bitmap.Clear(clBlack32);

  FHeightToBitmapX := (HeightMap.SizeX/Bitmap.Width);
  FHeightToBitmapY := (HeightMap.SizeY/Bitmap.Height);

  for x := 0 to Bitmap.Width-1 do
  begin
    for y := 0 to Bitmap.Height-1 do
    begin
      FDrawingX := x;
      FDrawingY := y;

      hx := FDrawingX * FHeightToBitmapX;
      hy := FDrawingY * FHeightToBitmapY;

      Color := GetColor(hx,hy);

      if Shaded then
      begin
        Shade := Heightmap.Shade.InterpolatedCos[hx,hy];
        //Shade := min(1, Shade);

        ShadowShade := 1-Heightmap.Shadow.InterpolatedCos[hx, hy];

        Shade := min(Shade, ShadowShade);

        // Use the ambient light
        Shade := (Shade*(1-Ambient))+Ambient;

        if Shade=0 then
          Color := clGreen32
        else
          Color := LerpColor32(Color, clBlack32, Shade);
      end;

      Bitmap.Pixel[FDrawingX,FDrawingY] := Color;
    end;
  end;
end;

procedure TColorModifierHandler.SetNoTexture(const Value: boolean);
begin
  FNoTexture := Value;
end;

{ TColorModifierHeight }

procedure TColorModifierHeight.GetColorAndLerp(x, y: integer; Height,
  Slope: single; var outColor32: TColor32; var Lerp: single);
begin
  // Assume no influence
  Lerp := 0;
  outColor32 := GetPixel(x,y);

  if (Height>=StartHeight) and (Height<=FullOnHeight) then
  begin
    Lerp := (Height-StartHeight)/(FullOnHeight-StartHeight);
    exit;
  end;

  if (Height>=FullOnHeight) and (Height<=StopHeight) then
  begin
    Lerp := 1;
    exit;
  end;

  if (Height>=StopHeight) and (Height<=FullOffHeight) then
  begin
    Lerp := 1-(Height-StopHeight)/(FullOffHeight-StopHeight);
    exit;
  end;//}
end;

procedure TColorModifierHeight.SetupCurve(StartHeight, FullOnHeight,
  StopHeight, FullOffHeight: single);
begin
  self.StartHeight := StartHeight;
  self.FullOnHeight := FullOnHeight;
  self.StopHeight := StopHeight;
  self.FullOffHeight := FullOffHeight;
end;

{ TColorModifierSlope }

procedure TColorModifierSlope.GetColorAndLerp(x, y: integer; Height,
  Slope: single; var outColor32: TColor32; var Lerp: single);
begin
  inherited;

  outColor32 := GetPixel(x,y);
  Lerp := slope+0.2;
  if Lerp>1 then
    Lerp := 1;
end;


end.
