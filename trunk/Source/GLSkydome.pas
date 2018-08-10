﻿//
// This unit is part of the GLScene Project, http://glscene.org
//
{
  Skydome object
}
unit GLSkydome;

interface

{$I GLScene.inc}

uses
{$IFDEF USE_FASTMATH}
  Neslib.FastMath,
{$ENDIF}
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,
  Vcl.Graphics,

  OpenGLAdapter,
  OpenGLTokens,
  GLScene,
  GLContext,
  GLState,
  GLVectorGeometry,
  GLGraphics,
  GLVectorTypes,
  GLColor,
  GLRenderContextInfo;

type
   TGLStarRecord = packed record
      RA : Word;              // x100 builtin factor, degrees
      DEC : SmallInt;         // x100 builtin factor, degrees
      BVColorIndex : Byte;    // x100 builtin factor
      VMagnitude : Byte;      // x10 builtin factor
   end;
   PGLStarRecord = ^TGLStarRecord;

  TGLSkyDomeBand = class(TCollectionItem)
  private
    FStartAngle: Single;
    FStopAngle: Single;
    FStartColor: TGLColor;
    FStopColor: TGLColor;
    FSlices: Integer;
    FStacks: Integer;
  protected
    function GetDisplayName: string; override;
    procedure SetStartAngle(const val: Single);
    procedure SetStartColor(const val: TGLColor);
    procedure SetStopAngle(const val: Single);
    procedure SetStopColor(const val: TGLColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChange(sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo);
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StartColor: TGLColor read FStartColor write SetStartColor;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property StopColor: TGLColor read FStopColor write SetStopColor;
    property Slices: Integer read FSlices write SetSlices default 12;
    property Stacks: Integer read FStacks write SetStacks default 1;
  end;

  TGLSkyDomeBands = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TGLSkyDomeBand);
    function GetItems(index: Integer): TGLSkyDomeBand;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGLSkyDomeBand;
    function FindItemID(ID: Integer): TGLSkyDomeBand;
    property Items[index: Integer]: TGLSkyDomeBand read GetItems write SetItems;
    default;
    procedure NotifyChange;
    procedure BuildList(var rci: TGLRenderContextInfo);
  end;

  TGLSkyDomeStar = class(TCollectionItem)
  private
    FRA, FDec: Single;
    FMagnitude: Single;
    FColor: TColor;
    FCacheCoord: TAffineVector; // cached cartesian coordinates
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
      {Right Ascension, in degrees. }
    property RA: Single read FRA write FRA;
    {Declination, in degrees. }
    property Dec: Single read FDec write FDec;
    {Absolute magnitude. }
    property Magnitude: Single read FMagnitude write FMagnitude;
    {Color of the star. }
    property Color: TColor read FColor write FColor;
  end;

  TGLSkyDomeStars = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TGLSkyDomeStar);
    function GetItems(index: Integer): TGLSkyDomeStar;
    procedure PrecomputeCartesianCoordinates;
  public
    constructor Create(AOwner: TComponent);
    function Add: TGLSkyDomeStar;
    function FindItemID(ID: Integer): TGLSkyDomeStar;
    property Items[index: Integer]: TGLSkyDomeStar read GetItems write SetItems;
    default;
    procedure BuildList(var rci: TGLRenderContextInfo; twinkle: Boolean);
    {Adds nb random stars of the given color. 
       Stars are homogenously scattered on the complete sphere, not only the band defined or visible dome. }
    procedure AddRandomStars(const nb: Integer; const color: TColor; const limitToTopDome: Boolean = False); overload;
    procedure AddRandomStars(const nb: Integer; const ColorMin, ColorMax:TVector3b; const Magnitude_min, Magnitude_max: Single;const limitToTopDome: Boolean = False); overload;
    {Load a 'stars' file, which is made of TGLStarRecord. 
       Not that '.stars' files should already be sorted by magnitude and color. }
    procedure LoadStarsFile(const starsFileName: string);
  end;

  TGLSkyDomeOption = (sdoTwinkle);
  TGLSkyDomeOptions = set of TGLSkyDomeOption;

  (*Renders a sky dome always centered on the camera. 
       If you use this object make sure it is rendered *first*, as it ignores
       depth buffering and overwrites everything. All children of a skydome
       are rendered in the skydome's coordinate system. 
       The skydome is described by "bands", each "band" is an horizontal cut
       of a sphere, and you can have as many bands as you wish. 
       Estimated CPU cost (K7-500, GeForce SDR, default bands): 
        800x600 fullscreen filled: 4.5 ms (220 FPS, worst case)
        Geometry cost (0% fill): 0.7 ms (1300 FPS, best case) *)
  TGLSkyDome = class(TGLCameraInvariantObject)
  private
     
    FOptions: TGLSkyDomeOptions;
    FBands: TGLSkyDomeBands;
    FStars: TGLSkyDomeStars;
  protected
    procedure SetBands(const val: TGLSkyDomeBands);
    procedure SetStars(const val: TGLSkyDomeStars);
    procedure SetOptions(const val: TGLSkyDomeOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  published
    property Bands: TGLSkyDomeBands read FBands write SetBands;
    property Stars: TGLSkyDomeStars read FStars write SetStars;
    property Options: TGLSkyDomeOptions read FOptions write SetOptions default [];
  end;

  TEarthSkydomeOption = (esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest);
  TEarthSkydomeOptions = set of TEarthSkydomeOption;

  {Render a skydome like what can be seen on earth. 
     Color is based on sun position and turbidity, to "mimic" atmospheric
     Rayleigh and Mie scatterings. The colors can be adjusted to render
     weird/extra-terrestrial atmospheres too. 
     The default slices/stacks values make for an average quality rendering,
     for a very clean rendering, use 64/64 (more is overkill in most cases).
     The complexity is quite high though, making a T&L 3D board a necessity
     for using TGLEarthSkyDome. }
  TGLEarthSkyDome = class(TGLSkyDome)
  private
    FSunElevation: Single;
    FTurbidity: Single;
    FCurSunColor, FCurSkyColor, FCurHazeColor: TColorVector;
    FCurHazeTurbid, FCurSunSkyTurbid: Single;
    FSunZenithColor: TGLColor;
    FSunDawnColor: TGLColor;
    FHazeColor: TGLColor;
    FSkyColor: TGLColor;
    FNightColor: TGLColor;
    FDeepColor: TGLColor;
    FSlices, FStacks: Integer;
    FExtendedOptions: TEarthSkydomeOptions;
    FMorning: boolean;
  protected
    procedure Loaded; override;
    procedure SetSunElevation(const val: Single);
    procedure SetTurbidity(const val: Single);
    procedure SetSunZenithColor(const val: TGLColor);
    procedure SetSunDawnColor(const val: TGLColor);
    procedure SetHazeColor(const val: TGLColor);
    procedure SetSkyColor(const val: TGLColor);
    procedure SetNightColor(const val: TGLColor);
    procedure SetDeepColor(const val: TGLColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChanged(Sender: TObject);
    procedure PreCalculate;
    procedure RenderDome;
    function CalculateColor(const theta, cosGamma: Single): TColorVector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure SetSunAtTime(HH, MM: Single);
  published
      {Elevation of the sun, measured in degrees. }
    property SunElevation: Single read FSunElevation write SetSunElevation;
    {Expresses the purity of air.  Value range is from 1 (pure athmosphere) to 120 (very nebulous) }
    property Turbidity: Single read FTurbidity write SetTurbidity;
    property SunZenithColor: TGLColor read FSunZenithColor write SetSunZenithColor;
    property SunDawnColor: TGLColor read FSunDawnColor write SetSunDawnColor;
    property HazeColor: TGLColor read FHazeColor write SetHazeColor;
    property SkyColor: TGLColor read FSkyColor write SetSkyColor;
    property NightColor: TGLColor read FNightColor write SetNightColor;
    property DeepColor: TGLColor read FDeepColor write SetDeepColor;
    property ExtendedOptions: TEarthSkydomeOptions read FExtendedOptions write FExtendedOptions;
    property Slices: Integer read FSlices write SetSlices default 24;
    property Stacks: Integer read FStacks write SetStacks default 48;
  end;

{ Computes position on the unit sphere of a star record (Z=up). }
function StarRecordPositionZUp(const starRecord : TGLStarRecord) : TAffineVector;
{ Computes position on the unit sphere of a star record (Y=up). }
function StarRecordPositionYUp(const starRecord : TGLStarRecord) : TAffineVector;
{ Computes star color from BV index (RGB) and magnitude (alpha). }
function StarRecordColor(const starRecord : TGLStarRecord; bias : Single) : TVector;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function StarRecordPositionYUp(const starRecord : TGLStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCosine(starRecord.DEC*(0.01*PI/180), Result.Y, f);
   SinCosine(starRecord.RA*(0.01*PI/180), f, Result.X, Result.Z);
end;

function StarRecordPositionZUp(const starRecord : TGLStarRecord) : TAffineVector;
var
   f : Single;
begin
   SinCosine(starRecord.DEC*(0.01*PI/180), Result.Z, f);
   SinCosine(starRecord.RA*(0.01*PI/180), f, Result.X, Result.Y);
end;

function StarRecordColor(const starRecord : TGLStarRecord; bias : Single) : TVector;
const
   // very *rough* approximation
   cBVm035 : TVector = (X:0.7; Y:0.8; Z:1.0; W:1);
   cBV015  : TVector = (X:1.0; Y:1.0; Z:1.0; W:1);
   cBV060  : TVector = (X:1.0; Y:1.0; Z:0.7; W:1);
   cBV135  : TVector = (X:1.0; Y:0.8; Z:0.7; W:1);
var
   bvIndex100 : Integer;
begin
   bvIndex100:=starRecord.BVColorIndex-50;
   // compute RGB color for B&V index
   if bvIndex100<-035 then
      Result:=cBVm035
   else if bvIndex100<015 then
      VectorLerp(cBVm035, cBV015, (bvIndex100+035)*(1/(015+035)), Result)
   else if bvIndex100<060 then
      VectorLerp(cBV015, cBV060, (bvIndex100-015)*(1/(060-015)), Result)
   else if bvIndex100<135 then
      VectorLerp(cBV060, cBV135, (bvIndex100-060)*(1/(135-060)), Result)
   else Result:=cBV135;
   // compute transparency for VMag
   // the actual factor is 2.512, and not used here
   Result.W:=PowerSingle(1.2, -(starRecord.VMagnitude*0.1-bias));
end;


// ------------------
// ------------------ TGLSkyDomeBand ------------------
// ------------------

constructor TGLSkyDomeBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStartColor := TGLColor.Create(Self);
  FStartColor.Initialize(clrBlue);
  FStartColor.OnNotifyChange := OnColorChange;
  FStopColor := TGLColor.Create(Self);
  FStopColor.Initialize(clrBlue);
  FStopColor.OnNotifyChange := OnColorChange;
  FSlices := 12;
  FStacks := 1;
end;

destructor TGLSkyDomeBand.Destroy;
begin
  FStartColor.Free;
  FStopColor.Free;
  inherited Destroy;
end;

procedure TGLSkyDomeBand.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDomeBand then
  begin
    FStartAngle := TGLSkyDomeBand(Source).FStartAngle;
    FStopAngle := TGLSkyDomeBand(Source).FStopAngle;
    FStartColor.Assign(TGLSkyDomeBand(Source).FStartColor);
    FStopColor.Assign(TGLSkyDomeBand(Source).FStopColor);
    FSlices := TGLSkyDomeBand(Source).FSlices;
    FStacks := TGLSkyDomeBand(Source).FStacks;
  end;
  inherited Destroy;
end;

function TGLSkyDomeBand.GetDisplayName: string;
begin
  Result := Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

procedure TGLSkyDomeBand.SetStartAngle(const val: Single);
begin
  FStartAngle := ClampValue(val, -90, 90);
  if FStartAngle > FStopAngle then FStopAngle := FStartAngle;
  TGLSkyDomeBands(Collection).NotifyChange;
end;

procedure TGLSkyDomeBand.SetStartColor(const val: TGLColor);
begin
  FStartColor.Assign(val);
end;

procedure TGLSkyDomeBand.SetStopAngle(const val: Single);
begin
  FStopAngle := ClampValue(val, -90, 90);
  if FStopAngle < FStartAngle then
    FStartAngle := FStopAngle;
  TGLSkyDomeBands(Collection).NotifyChange;
end;

procedure TGLSkyDomeBand.SetStopColor(const val: TGLColor);
begin
  FStopColor.Assign(val);
end;

procedure TGLSkyDomeBand.SetSlices(const val: Integer);
begin
  if val < 3 then
    FSlices := 3
  else
    FSlices := val;
  TGLSkyDomeBands(Collection).NotifyChange;
end;

procedure TGLSkyDomeBand.SetStacks(const val: Integer);
begin
  if val < 1 then
    FStacks := 1
  else
    FStacks := val;
  TGLSkyDomeBands(Collection).NotifyChange;
end;

procedure TGLSkyDomeBand.OnColorChange(sender: TObject);
begin
  TGLSkyDomeBands(Collection).NotifyChange;
end;

procedure TGLSkyDomeBand.BuildList(var rci: TGLRenderContextInfo);
// coordinates system note: X is forward, Y is left and Z is up

// always rendered as sphere of radius 1
  procedure RenderBand(start, stop: Single; const colStart, colStop:
    TColorVector);
  var
    i: Integer;
    f, r, r2: Single;
    vertex1, vertex2: TVector;
  begin
    vertex1.W := 1;
    if start = -90 then
    begin
      // triangle fan with south pole
      gl.Begin_(GL_TRIANGLE_FAN);
      gl.Color4fv(@colStart);
      gl.Vertex3f(0, 0, -1);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(stop), vertex1.Z, r);
      gl.Color4fv(@colStop);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        gl.Vertex4fv(@vertex1);
      end;
      gl.End_;
    end
    else if stop = 90 then
    begin
      // triangle fan with north pole
      gl.Begin_(GL_TRIANGLE_FAN);
      gl.Color4fv(@colStop);
      gl.Vertex3fv(@ZHmgPoint);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      gl.Color4fv(@colStart);
      for i := Slices downto 0 do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        gl.Vertex4fv(@vertex1);
      end;
      gl.End_;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      gl.Begin_(GL_TRIANGLE_STRIP);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        gl.Color4fv(@colStart);
        gl.Vertex4fv(@vertex1);
        SinCosine(i * f, r2, vertex2.Y, vertex2.X);
        gl.Color4fv(@colStop);
        gl.Vertex4fv(@vertex2);
      end;
      gl.End_;
    end;
  end;

var
  n: Integer;
  t, t2: Single;
begin
  if StartAngle = StopAngle then
    Exit;
  for n := 0 to Stacks - 1 do
  begin
    t := n / Stacks;
    t2 := (n + 1) / Stacks;
    RenderBand(Lerp(StartAngle, StopAngle, t),
      Lerp(StartAngle, StopAngle, t2),
      VectorLerp(StartColor.Color, StopColor.Color, t),
      VectorLerp(StartColor.Color, StopColor.Color, t2));
  end;
end;

// ------------------
// ------------------ TGLSkyDomeBands ------------------
// ------------------

constructor TGLSkyDomeBands.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TGLSkyDomeBand);
end;

function TGLSkyDomeBands.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TGLSkyDomeBands.SetItems(index: Integer; const val: TGLSkyDomeBand);
begin
  inherited Items[index] := val;
end;

function TGLSkyDomeBands.GetItems(index: Integer): TGLSkyDomeBand;
begin
  Result := TGLSkyDomeBand(inherited Items[index]);
end;

function TGLSkyDomeBands.Add: TGLSkyDomeBand;
begin
  Result := (inherited Add) as TGLSkyDomeBand;
end;

function TGLSkyDomeBands.FindItemID(ID: Integer): TGLSkyDomeBand;
begin
  Result := (inherited FindItemID(ID)) as TGLSkyDomeBand;
end;

procedure TGLSkyDomeBands.NotifyChange;
begin
  if Assigned(owner) and (owner is TGLBaseSceneObject) then TGLBaseSceneObject(owner).StructureChanged;
end;

procedure TGLSkyDomeBands.BuildList(var rci: TGLRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TGLSkyDomeStar ------------------
// ------------------

constructor TGLSkyDomeStar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TGLSkyDomeStar.Destroy;
begin
  inherited Destroy;
end;

procedure TGLSkyDomeStar.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDomeStar then
  begin
    FRA := TGLSkyDomeStar(Source).FRA;
    FDec := TGLSkyDomeStar(Source).FDec;
    FMagnitude := TGLSkyDomeStar(Source).FMagnitude;
    FColor := TGLSkyDomeStar(Source).FColor;
    SetVector(FCacheCoord, TGLSkyDomeStar(Source).FCacheCoord);
  end;
  inherited Destroy;
end;

function TGLSkyDomeStar.GetDisplayName: string;
begin
  Result := Format('RA: %5.1f / Dec: %5.1f', [RA, Dec]);
end;

// ------------------
// ------------------ TGLSkyDomeStars ------------------
// ------------------

constructor TGLSkyDomeStars.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TGLSkyDomeStar);
end;

function TGLSkyDomeStars.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TGLSkyDomeStars.SetItems(index: Integer; const val: TGLSkyDomeStar);
begin
  inherited Items[index] := val;
end;

function TGLSkyDomeStars.GetItems(index: Integer): TGLSkyDomeStar;
begin
  Result := TGLSkyDomeStar(inherited Items[index]);
end;

function TGLSkyDomeStars.Add: TGLSkyDomeStar;
begin
  Result := (inherited Add) as TGLSkyDomeStar;
end;

function TGLSkyDomeStars.FindItemID(ID: Integer): TGLSkyDomeStar;
begin
  Result := (inherited FindItemID(ID)) as TGLSkyDomeStar;
end;

procedure TGLSkyDomeStars.PrecomputeCartesianCoordinates;
var
  i: Integer;
  star: TGLSkyDomeStar;
  raC, raS, decC, decS: Single;
begin
  // to be enhanced...
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    SinCosine(star.DEC * cPIdiv180, decS, decC);
    SinCosine(star.RA * cPIdiv180, decC, raS, raC);
    star.FCacheCoord.X := raC;
    star.FCacheCoord.Y := raS;
    star.FCacheCoord.Z := decS;
  end;
end;

procedure TGLSkyDomeStars.BuildList(var rci: TGLRenderContextInfo; twinkle:
  Boolean);
var
  i, n: Integer;
  star: TGLSkyDomeStar;
  lastColor: TColor;
  lastPointSize10, pointSize10: Integer;
  color, twinkleColor: TColorVector;

  procedure DoTwinkle;
  begin
    if (n and 63) = 0 then
    begin
      twinkleColor := VectorScale(color, Random * 0.6 + 0.4);
      gl.Color3fv(@twinkleColor.X);
      n := 0;
    end
    else
      Inc(n);
  end;

begin
  if Count = 0 then
    Exit;
  PrecomputeCartesianCoordinates;
  lastColor := -1;
  n := 0;
  lastPointSize10 := -1;

  rci.GLStates.Enable(stPointSmooth);
  rci.GLStates.Enable(stAlphaTest);
  rci.GLStates.SetGLAlphaFunction(cfNotEqual, 0.0);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOne);

  gl.Begin_(GL_POINTS);
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    pointSize10 := Round((4.5 - star.Magnitude) * 10);
    if pointSize10 <> lastPointSize10 then
    begin
      if pointSize10 > 15 then
      begin
        gl.End_;
        lastPointSize10 := pointSize10;
        rci.GLStates.PointSize := pointSize10 * 0.1;
        gl.Begin_(GL_POINTS);
      end
      else if lastPointSize10 <> 15 then
      begin
        gl.End_;
        lastPointSize10 := 15;
        rci.GLStates.PointSize := 1.5;
        gl.Begin_(GL_POINTS);
      end;
    end;
    if lastColor <> star.FColor then
    begin
      color := ConvertWinColor(star.FColor);
      if twinkle then
      begin
        n := 0;
        DoTwinkle;
      end
      else
        gl.Color3fv(@color.X);
      lastColor := star.FColor;
    end
    else if twinkle then
      DoTwinkle;
    gl.Vertex3fv(@star.FCacheCoord.X);
  end;
  gl.End_;

  // restore default AlphaFunc
  rci.GLStates.SetGLAlphaFunction(cfGreater, 0);
end;

procedure TGLSkyDomeStars.AddRandomStars(const nb: Integer; const color: TColor;
  const limitToTopDome: Boolean = False);
var
  i: Integer;
  coord: TAffineVector;
  star: TGLSkyDomeStar;
begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.Z := Random
    else
      coord.Z := Random * 2 - 1;
    // calculate RA and Dec
    star.Dec := ArcSine(coord.Z) * c180divPI;
    star.Ra := Random * 360 - 180;
    // pick a color
    star.Color := color;
    // pick a magnitude
    star.Magnitude := 3;
  end;
end;

procedure TGLSkyDomeStars.AddRandomStars(const nb: Integer; const ColorMin,
  ColorMax: TVector3b;
  const Magnitude_min, Magnitude_max: Single;
  const limitToTopDome: Boolean = False);

  function RandomTT(Min, Max: Byte): Byte;
  begin
    Result := Min + Random(Max - Min);
  end;

var
  i: Integer;
  coord: TAffineVector;
  star: TGLSkyDomeStar;

begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.Z := Random
    else
      coord.Z := Random * 2 - 1;
    // calculate RA and Dec
    star.Dec := ArcSine(coord.Z) * c180divPI;
    star.Ra := Random * 360 - 180;
    // pick a color
    star.Color := RGB2Color(RandomTT(ColorMin.X, ColorMax.X),
      RandomTT(ColorMin.Y, ColorMax.Y),
      RandomTT(ColorMin.Z, ColorMax.Z));
    // pick a magnitude
    star.Magnitude := Magnitude_min + Random * (Magnitude_max - Magnitude_min);
  end;
end;

procedure TGLSkyDomeStars.LoadStarsFile(const starsFileName: string);
var
  fs: TFileStream;
  sr: TGLStarRecord;
  colorVector: TColorVector;
begin
  fs := TFileStream.Create(starsFileName, fmOpenRead + fmShareDenyWrite);
  try
    while fs.Position < fs.Size do
    begin
      fs.Read(sr, SizeOf(sr));
      with Add do
      begin
        RA := sr.RA * 0.01;
        DEC := sr.DEC * 0.01;
        colorVector := StarRecordColor(sr, 3);
        Magnitude := sr.VMagnitude * 0.1;
        if sr.VMagnitude > 35 then
          Color := ConvertColorVector(colorVector, colorVector.W)
        else
          Color := ConvertColorVector(colorVector);
      end;
    end;
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TGLSkyDome ------------------
// ------------------

constructor TGLSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FBands := TGLSkyDomeBands.Create(Self);
  with FBands.Add do
  begin
    StartAngle := 0;
    StartColor.Color := clrWhite;
    StopAngle := 15;
    StopColor.Color := clrBlue;
  end;
  with FBands.Add do
  begin
    StartAngle := 15;
    StartColor.Color := clrBlue;
    StopAngle := 90;
    Stacks := 4;
    StopColor.Color := clrNavy;
  end;
  FStars := TGLSkyDomeStars.Create(Self);
end;

destructor TGLSkyDome.Destroy;
begin
  FStars.Free;
  FBands.Free;
  inherited Destroy;
end;

procedure TGLSkyDome.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDome then
  begin
    FBands.Assign(TGLSkyDome(Source).FBands);
    FStars.Assign(TGLSkyDome(Source).FStars);
  end;
  inherited;
end;

procedure TGLSkyDome.SetBands(const val: TGLSkyDomeBands);
begin
  FBands.Assign(val);
  StructureChanged;
end;

procedure TGLSkyDome.SetStars(const val: TGLSkyDomeStars);
begin
  FStars.Assign(val);
  StructureChanged;
end;

procedure TGLSkyDome.SetOptions(const val: TGLSkyDomeOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    if sdoTwinkle in FOptions then
      ObjectStyle := ObjectStyle + [osDirectDraw]
    else
    begin
      ObjectStyle := ObjectStyle - [osDirectDraw];
      DestroyHandle;
    end;
    StructureChanged;
  end;
end;

procedure TGLSkyDome.BuildList(var rci: TGLRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.GLStates do
  begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stBlend);
    DepthWriteMask := False;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.90;
  gl.Scalef(f, f, f);

  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));
end;

// ------------------
// ------------------ TGLEarthSkyDome ------------------
// ------------------

constructor TGLEarthSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMorning:=true;
  Bands.Clear;
  FSunElevation := 75;
  FTurbidity := 15;
  FSunZenithColor := TGLColor.CreateInitialized(Self, clrWhite, OnColorChanged);
  FSunDawnColor := TGLColor.CreateInitialized(Self, Vectormake(1, 0.5, 0, 0),OnColorChanged);
  FHazeColor := TGLColor.CreateInitialized(Self, VectorMake(0.9, 0.95, 1, 0),OnColorChanged);
  FSkyColor := TGLColor.CreateInitialized(Self, VectorMake(0.45, 0.6, 0.9, 0),OnColorChanged);
  FNightColor := TGLColor.CreateInitialized(Self, clrTransparent,OnColorChanged);
  FDeepColor := TGLColor.CreateInitialized(Self, VectorMake(0, 0.2, 0.4, 0));
  FStacks := 24;
  FSlices := 48;
  PreCalculate;
end;

destructor TGLEarthSkyDome.Destroy;
begin
  FSunZenithColor.Free;
  FSunDawnColor.Free;
  FHazeColor.Free;
  FSkyColor.Free;
  FNightColor.Free;
  FDeepColor.Free;
  inherited Destroy;
end;

procedure TGLEarthSkyDome.Assign(Source: TPersistent);
begin
  if Source is TGLSkyDome then
  begin
    FSunElevation := TGLEarthSkyDome(Source).SunElevation;
    FTurbidity := TGLEarthSkyDome(Source).Turbidity;
    FSunZenithColor.Assign(TGLEarthSkyDome(Source).FSunZenithColor);
    FSunDawnColor.Assign(TGLEarthSkyDome(Source).FSunDawnColor);
    FHazeColor.Assign(TGLEarthSkyDome(Source).FHazeColor);
    FSkyColor.Assign(TGLEarthSkyDome(Source).FSkyColor);
    FNightColor.Assign(TGLEarthSkyDome(Source).FNightColor);
    FSlices := TGLEarthSkyDome(Source).FSlices;
    FStacks := TGLEarthSkyDome(Source).FStacks;
    PreCalculate;
  end;
  inherited;
end;

procedure TGLEarthSkyDome.Loaded;
begin
  inherited;
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSunElevation(const val: Single);
var
  newVal: single;
begin
  newval := clampValue(val, -90, 90);
  if FSunElevation <> newval then
  begin
    FSunElevation := newval;
    PreCalculate;
  end;
end;

procedure TGLEarthSkyDome.SetTurbidity(const val: Single);
begin
  FTurbidity := ClampValue(val, 1, 120);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSunZenithColor(const val: TGLColor);
begin
  FSunZenithColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSunDawnColor(const val: TGLColor);
begin
  FSunDawnColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetHazeColor(const val: TGLColor);
begin
  FHazeColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSkyColor(const val: TGLColor);
begin
  FSkyColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetNightColor(const val: TGLColor);
begin
  FNightColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetDeepColor(const val: TGLColor);
begin
  FDeepColor.Assign(val);
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSlices(const val: Integer);
begin
  if val>6 then FSlices:=val else FSlices:=6;
  StructureChanged;
end;

procedure TGLEarthSkyDome.SetStacks(const val: Integer);
begin
  if val>1 then FStacks:=val else FStacks:=1;
  StructureChanged;
end;

procedure TGLEarthSkyDome.BuildList(var rci: TGLRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.GLStates do
  begin
    CurrentProgram := 0;
    Disable(stLighting);
    if esoDepthTest in FExtendedOptions then
    begin
      Enable(stDepthTest);
      DepthFunc := cfLEqual;
    end
    else
      Disable(stDepthTest);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stBlend);
    Disable(stAlphaTest);
    DepthWriteMask := False;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.95;
  gl.Scalef(f, f, f);

  RenderDome;
  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));

  // restore
  rci.GLStates.DepthWriteMask := True;
end;

procedure TGLEarthSkyDome.OnColorChanged(Sender: TObject);
begin
  PreCalculate;
end;

procedure TGLEarthSkyDome.SetSunAtTime(HH, MM: Single);
const
  cHourToElevation1: array[0..23] of Single =
  (-45, -67.5, -90, -57.5, -45, -22.5, 0, 11.25, 22.5, 33.7, 45, 56.25, 67.5,
   78.75, 90, 78.75, 67.5, 56.25, 45, 33.7, 22.5, 11.25, 0, -22.5);
  cHourToElevation2: array[0..23] of Single =
  (-0.375, -0.375, 0.375, 0.375, 0.375, 0.375, 0.1875, 0.1875, 0.1875, 0.1875,  
     0.1875, 0.1875, 0.1875, 0.1875, -0.1875, -0.1875, -0.1875, -0.1875, -0.1875,
     -0.1875, -0.1875, -0.1875, -0.375, -0.375);
var
  ts:Single;
  fts:Single;
  i:integer;
  color:TColor;
begin
  HH:=Round(HH);
  if HH<0 then HH:=0;
  if HH>23 then HH:=23;
  if MM<0 then MM:=0;
  if MM>=60 then
  begin
    MM:=0;
    HH:=HH+1;
    if HH>23 then HH:=0;
  end;
  FSunElevation := cHourToElevation1[Round(HH)] + cHourToElevation2[Round(HH)]*MM;

  ts := DegToRadian(90 - FSunElevation);
  // Mix base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := IntPower(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  //fade stars if required
  if SunElevation>-40 then ts:=PowerInteger(1-(SunElevation+40)/90,11)else ts:=1;
  color := RGB2Color(round(ts * 255), round(ts * 255), round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then for i:=0 to Stars.Count-1 do stars[i].Color:=color;


  if esoRotateOnTwelveHours in ExtendedOptions then // spining around blue orb
  begin
    if (HH>=14) and (FMorning) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=false;
    end;

    if (HH>=2) and (HH<14) and (not FMorning) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=true;
    end;
  end;
  StructureChanged;
end;

procedure TGLEarthSkyDome.PreCalculate;
var
  ts: Single;
  fts: Single;
  i: integer;
  color: TColor;
begin
  ts := DegToRadian(90 - SunElevation);
  // Precompose base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := PowerInteger(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  //fade stars if required
  if SunElevation>-40 then
    ts := PowerInteger(1 - (SunElevation+40) / 90, 11)
  else
    ts := 1;
  color := RGB2Color(round(ts * 255), round(ts * 255), round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then
    for i := 0 to Stars.Count - 1 do
      stars[i].Color := color;

  if esoRotateOnTwelveHours in ExtendedOptions then
  begin
    if SunElevation = 90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        stars[i].RA := Stars[i].RA + 180;
    end
    else if SunElevation = -90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        stars[i].RA := Stars[i].RA + 180;
    end;
  end;

  StructureChanged;
end;

function TGLEarthSkyDome.CalculateColor(const theta, cosGamma: Single):
  TColorVector;
var
  t: Single;
begin
  t := PI / 2 - theta;
  // mix to get haze/sky
  VectorLerp(FCurSkyColor, FCurHazeColor, ClampValue(exp(FCurHazeTurbid * t), 0,
    1), Result);
  // then mix sky with sun
  VectorLerp(Result, FCurSunColor, ClampValue(exp(FCurSunSkyTurbid * cosGamma *
    (1 + t)) * 1.1, 0, 1), Result);
end;

procedure TGLEarthSkyDome.RenderDome;
var
  ts: Single;
  steps: Integer;
  sunPos: TAffineVector;
  sinTable, cosTable: PFloatArray;

  // coordinates system note: X is forward, Y is left and Z is up
  // always rendered as sphere of radius 1

  function CalculateCosGamma(const p: TVector): Single;
  begin
    Result := 1 - VectorAngleCosine(PAffineVector(@p)^, sunPos);
  end;

  procedure RenderDeepBand(stop: Single);
  var
    i: Integer;
    r, thetaStart: Single;
    vertex1: TVector;
    color: TColorVector;
  begin
    r := 0;
    vertex1.W := 1;
    // triangle fan with south pole
    gl.Begin_(GL_TRIANGLE_FAN);
    color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
    gl.Color4fv(DeepColor.AsAddress);
    gl.Vertex3f(0, 0, -1);
    SinCosine(DegToRadian(stop), vertex1.Z, r);
    thetaStart := DegToRadian(90 - stop);
    for i := 0 to steps - 1 do
    begin
      vertex1.X := r * cosTable[i];
      vertex1.Y := r * sinTable[i];
      color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
      gl.Color4fv(@color);
      gl.Vertex4fv(@vertex1);
    end;
    gl.End_;
  end;

  procedure RenderBand(start, stop: Single);
  var
    i: Integer;
    r, r2, thetaStart, thetaStop: Single;
    vertex1, vertex2: TVector;
    color: TColorVector;
  begin
    vertex1.W := 1;
    if stop = 90 then
    begin
      // triangle fan with north pole
      gl.Begin_(GL_TRIANGLE_FAN);
      color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
      gl.Color4fv(@color);
      gl.Vertex4fv(@ZHmgPoint);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      thetaStart := DegToRadian(90 - start);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        gl.Color4fv(@color);
        gl.Vertex4fv(@vertex1);
      end;
      gl.End_;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      gl.Begin_(GL_TRIANGLE_STRIP);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      thetaStart := DegToRadian(90 - start);
      thetaStop := DegToRadian(90 - stop);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        gl.Color4fv(@color);
        gl.Vertex4fv(@vertex1);
        vertex2.X := r2 * cosTable[i];
        vertex2.Y := r2 * sinTable[i];
        color := CalculateColor(thetaStop, CalculateCosGamma(vertex2));
        gl.Color4fv(@color);
        gl.Vertex4fv(@vertex2);
      end;
      gl.End_;
    end;
  end;

var
  n, i, sdiv2: Integer;
  t, t2, p, fs: Single;
begin
  ts := DegToRadian(90 - SunElevation);
  SetVector(sunPos, sin(ts), 0, cos(ts));
  // prepare sin/cos LUT, with a higher sampling around 0Ѝ
  n := Slices div 2;
  steps := 2 * n + 1;
  GetMem(sinTable, steps * SizeOf(Single));
  GetMem(cosTable, steps * SizeOf(Single));
  for i := 1 to n do
  begin
    p := (1 - Sqrt(Cos((i / n) * cPIdiv2))) * PI;
    SinCosine(p, sinTable[n + i], cosTable[n + i]);
    sinTable[n - i] := -sinTable[n + i];
    cosTable[n - i] := cosTable[n + i];
  end;
  // these are defined by hand for precision issue: the dome must wrap exactly
  sinTable[n] := 0;
  cosTable[n] := 1;
  sinTable[0] := 0;
  cosTable[0] := -1;
  sinTable[steps - 1] := 0;
  cosTable[steps - 1] := -1;
  fs := SunElevation / 90;
  // start render
  t := 0;
  sdiv2 := Stacks div 2;
  for n := 0 to Stacks - 1 do
  begin
    if fs > 0 then
    begin
      if n < sdiv2 then
        t2 := fs - fs * Sqr((sdiv2 - n) / sdiv2)
      else
        t2 := fs + Sqr((n - sdiv2) / (sdiv2 - 1)) * (1 - fs);
    end
    else
      t2 := (n + 1) / Stacks;
    RenderBand(Lerp(1, 90, t), Lerp(1, 90, t2));
    t := t2;
  end;
  RenderDeepBand(1);
  FreeMem(sinTable);
  FreeMem(cosTable);
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TGLSkyDome, TGLEarthSkyDome]);

end.
