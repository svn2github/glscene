//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{   Skydome object

}
unit VXS.Skydome;

interface

{$I VXScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,
  FMX.Graphics,

  VXS.Scene,
  VXS.VectorGeometry,
  VXS.Graphics,
  VXS.CrossPlatform,
  VXS.VectorTypes,
  VXS.Color,
  VXS.RenderContextInfo;

type

  TVXSkyDomeBand = class(TCollectionItem)
  private
    FStartAngle: Single;
    FStopAngle: Single;
    FStartColor: TVXColor;
    FStopColor: TVXColor;
    FSlices: Integer;
    FStacks: Integer;
  protected
    function GetDisplayName: string; override;
    procedure SetStartAngle(const val: Single);
    procedure SetStartColor(const val: TVXColor);
    procedure SetStopAngle(const val: Single);
    procedure SetStopColor(const val: TVXColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChange(sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo);
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StartColor: TVXColor read FStartColor write SetStartColor;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property StopColor: TVXColor read FStopColor write SetStopColor;
    property Slices: Integer read FSlices write SetSlices default 12;
    property Stacks: Integer read FStacks write SetStacks default 1;
  end;

  TVXSkyDomeBands = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXSkyDomeBand);
    function GetItems(index: Integer): TVXSkyDomeBand;
  public
    constructor Create(AOwner: TComponent);
    function Add: TVXSkyDomeBand;
    function FindItemID(ID: Integer): TVXSkyDomeBand;
    property Items[index: Integer]: TVXSkyDomeBand read GetItems write SetItems;
    default;
    procedure NotifyChange;
    procedure BuildList(var rci: TVXRenderContextInfo);
  end;

  TVXSkyDomeStar = class(TCollectionItem)
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
      { Right Ascension, in degrees. }
    property RA: Single read FRA write FRA;
    { Declination, in degrees. }
    property Dec: Single read FDec write FDec;
    { Absolute magnitude. }
    property Magnitude: Single read FMagnitude write FMagnitude;
    { Color of the star. }
    property Color: TColor read FColor write FColor;
  end;

  TVXSkyDomeStars = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVXSkyDomeStar);
    function GetItems(index: Integer): TVXSkyDomeStar;
    procedure PrecomputeCartesianCoordinates;
  public
    constructor Create(AOwner: TComponent);
    function Add: TVXSkyDomeStar;
    function FindItemID(ID: Integer): TVXSkyDomeStar;
    property Items[index: Integer]: TVXSkyDomeStar read GetItems write SetItems; default;
    procedure BuildList(var rci: TVXRenderContextInfo; twinkle: Boolean);
    { Adds nb random stars of the given color.
       Stars are homogenously scattered on the complete sphere, not only the band defined or visible dome. }
    procedure AddRandomStars(const nb: Integer; const color: TColor; const limitToTopDome: Boolean = False); overload;
    procedure AddRandomStars(const nb: Integer; const ColorMin, ColorMax:TVector3b; const Magnitude_min, Magnitude_max: Single;const limitToTopDome: Boolean = False); overload;
    { Load a 'stars' file, which is made of TVXStarRecord.
       Not that '.stars' files should already be sorted by magnitude and color. }
    procedure LoadStarsFile(const starsFileName: string);
  end;

  TVXSkyDomeOption = (sdoTwinkle);
  TVXSkyDomeOptions = set of TVXSkyDomeOption;

    { Renders a sky dome always centered on the camera.
       If you use this object make sure it is rendered *first*, as it ignores
       depth buffering and overwrites everything. All children of a skydome
       are rendered in the skydome's coordinate system.
       The skydome is described by "bands", each "band" is an horizontal cut
       of a sphere, and you can have as many bands as you wish.
       Estimated CPU cost (K7-500, GeForce SDR, default bands):
        800x600 fullscreen filled: 4.5 ms (220 FPS, worst case)
        Geometry cost (0% fill): 0.7 ms (1300 FPS, best case) }
  TVXSkyDome = class(TVXCameraInvariantObject)
  private
    FOptions: TVXSkyDomeOptions;
    FBands: TVXSkyDomeBands;
    FStars: TVXSkyDomeStars;
  protected
    procedure SetBands(const val: TVXSkyDomeBands);
    procedure SetStars(const val: TVXSkyDomeStars);
    procedure SetOptions(const val: TVXSkyDomeOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TVXRenderContextInfo); override;
  published
    property Bands: TVXSkyDomeBands read FBands write SetBands;
    property Stars: TVXSkyDomeStars read FStars write SetStars;
    property Options: TVXSkyDomeOptions read FOptions write SetOptions default [];
  end;

  TEarthSkydomeOption = (esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest);
  TEarthSkydomeOptions = set of TEarthSkydomeOption;

  { Render a skydome like what can be seen on earth.
     Color is based on sun position and turbidity, to "mimic" atmospheric
     Rayleigh and Mie scatterings. The colors can be adjusted to render
     weird/extra-terrestrial atmospheres too.
     The default slices/stacks values make for an average quality rendering,
     for a very clean rendering, use 64/64 (more is overkill in most cases).
     The complexity is quite high though, making a T&L 3D board a necessity
     for using TVXEarthSkyDome. }
  TVXEarthSkyDome = class(TVXSkyDome)
  private
    FSunElevation: Single;
    FTurbidity: Single;
    FCurSunColor, FCurSkyColor, FCurHazeColor: TColorVector;
    FCurHazeTurbid, FCurSunSkyTurbid: Single;
    FSunZenithColor: TVXColor;
    FSunDawnColor: TVXColor;
    FHazeColor: TVXColor;
    FSkyColor: TVXColor;
    FNightColor: TVXColor;
    FDeepColor: TVXColor;
    FSlices, FStacks: Integer;
    FExtendedOptions: TEarthSkydomeOptions;
    FMorning: boolean;
  protected
    procedure Loaded; override;
    procedure SetSunElevation(const val: Single);
    procedure SetTurbidity(const val: Single);
    procedure SetSunZenithColor(const val: TVXColor);
    procedure SetSunDawnColor(const val: TVXColor);
    procedure SetHazeColor(const val: TVXColor);
    procedure SetSkyColor(const val: TVXColor);
    procedure SetNightColor(const val: TVXColor);
    procedure SetDeepColor(const val: TVXColor);
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
    procedure BuildList(var rci: TVXRenderContextInfo); override;
    procedure SetSunAtTime(HH, MM: Single);
  published
      { Elevation of the sun, measured in degrees. }
    property SunElevation: Single read FSunElevation write SetSunElevation;
    { Expresses the purity of air.  Value range is from 1 (pure athmosphere) to 120 (very nebulous) }
    property Turbidity: Single read FTurbidity write SetTurbidity;
    property SunZenithColor: TVXColor read FSunZenithColor write SetSunZenithColor;
    property SunDawnColor: TVXColor read FSunDawnColor write SetSunDawnColor;
    property HazeColor: TVXColor read FHazeColor write SetHazeColor;
    property SkyColor: TVXColor read FSkyColor write SetSkyColor;
    property NightColor: TVXColor read FNightColor write SetNightColor;
    property DeepColor: TVXColor read FDeepColor write SetDeepColor;
    property ExtendedOptions: TEarthSkydomeOptions read FExtendedOptions write FExtendedOptions;
    property Slices: Integer read FSlices write SetSlices default 24;
    property Stacks: Integer read FStacks write SetStacks default 48;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  VXS.Context,
  VXS.StarRecord,
  VXS.State;

// ------------------
// ------------------ TVXSkyDomeBand ------------------
// ------------------

// Create
//

constructor TVXSkyDomeBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStartColor := TVXColor.Create(Self);
  FStartColor.Initialize(clrBlue);
  FStartColor.OnNotifyChange := OnColorChange;
  FStopColor := TVXColor.Create(Self);
  FStopColor.Initialize(clrBlue);
  FStopColor.OnNotifyChange := OnColorChange;
  FSlices := 12;
  FStacks := 1;
end;

// Destroy
//

destructor TVXSkyDomeBand.Destroy;
begin
  FStartColor.Free;
  FStopColor.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXSkyDomeBand.Assign(Source: TPersistent);
begin
  if Source is TVXSkyDomeBand then
  begin
    FStartAngle := TVXSkyDomeBand(Source).FStartAngle;
    FStopAngle := TVXSkyDomeBand(Source).FStopAngle;
    FStartColor.Assign(TVXSkyDomeBand(Source).FStartColor);
    FStopColor.Assign(TVXSkyDomeBand(Source).FStopColor);
    FSlices := TVXSkyDomeBand(Source).FSlices;
    FStacks := TVXSkyDomeBand(Source).FStacks;
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TVXSkyDomeBand.GetDisplayName: string;
begin
  Result := Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

// SetStartAngle
//

procedure TVXSkyDomeBand.SetStartAngle(const val: Single);
begin
  FStartAngle := ClampValue(val, -90, 90);
  if FStartAngle > FStopAngle then FStopAngle := FStartAngle;
  TVXSkyDomeBands(Collection).NotifyChange;
end;

// SetStartColor
//

procedure TVXSkyDomeBand.SetStartColor(const val: TVXColor);
begin
  FStartColor.Assign(val);
end;

// SetStopAngle
//

procedure TVXSkyDomeBand.SetStopAngle(const val: Single);
begin
  FStopAngle := ClampValue(val, -90, 90);
  if FStopAngle < FStartAngle then
    FStartAngle := FStopAngle;
  TVXSkyDomeBands(Collection).NotifyChange;
end;

// SetStopColor
//

procedure TVXSkyDomeBand.SetStopColor(const val: TVXColor);
begin
  FStopColor.Assign(val);
end;

// SetSlices
//

procedure TVXSkyDomeBand.SetSlices(const val: Integer);
begin
  if val < 3 then
    FSlices := 3
  else
    FSlices := val;
  TVXSkyDomeBands(Collection).NotifyChange;
end;

// SetStacks
//

procedure TVXSkyDomeBand.SetStacks(const val: Integer);
begin
  if val < 1 then
    FStacks := 1
  else
    FStacks := val;
  TVXSkyDomeBands(Collection).NotifyChange;
end;

// OnColorChange
//

procedure TVXSkyDomeBand.OnColorChange(sender: TObject);
begin
  TVXSkyDomeBands(Collection).NotifyChange;
end;

// BuildList
//

procedure TVXSkyDomeBand.BuildList(var rci: TVXRenderContextInfo);

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
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@colStart);
      glVertex3f(0, 0, -1);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(stop), vertex1.Z, r);
      glColor4fv(@colStop);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else if stop = 90 then
    begin
      // triangle fan with north pole
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@colStop);
      glVertex3fv(@ZHmgPoint);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      glColor4fv(@colStart);
      for i := Slices downto 0 do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      glBegin(GL_TRIANGLE_STRIP);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glColor4fv(@colStart);
        glVertex4fv(@vertex1);
        SinCosine(i * f, r2, vertex2.Y, vertex2.X);
        glColor4fv(@colStop);
        glVertex4fv(@vertex2);
      end;
      glEnd;
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
// ------------------ TVXSkyDomeBands ------------------
// ------------------

constructor TVXSkyDomeBands.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVXSkyDomeBand);
end;

function TVXSkyDomeBands.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVXSkyDomeBands.SetItems(index: Integer; const val: TVXSkyDomeBand);
begin
  inherited Items[index] := val;
end;

function TVXSkyDomeBands.GetItems(index: Integer): TVXSkyDomeBand;
begin
  Result := TVXSkyDomeBand(inherited Items[index]);
end;

function TVXSkyDomeBands.Add: TVXSkyDomeBand;
begin
  Result := (inherited Add) as TVXSkyDomeBand;
end;

function TVXSkyDomeBands.FindItemID(ID: Integer): TVXSkyDomeBand;
begin
  Result := (inherited FindItemID(ID)) as TVXSkyDomeBand;
end;

procedure TVXSkyDomeBands.NotifyChange;
begin
  if Assigned(owner) and (owner is TVXBaseSceneObject) then TVXBaseSceneObject(owner).StructureChanged;
end;

// BuildList
//

procedure TVXSkyDomeBands.BuildList(var rci: TVXRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TVXSkyDomeStar ------------------
// ------------------

// Create
//

constructor TVXSkyDomeStar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TVXSkyDomeStar.Destroy;
begin
  inherited Destroy;
end;

// Assign
//

procedure TVXSkyDomeStar.Assign(Source: TPersistent);
begin
  if Source is TVXSkyDomeStar then
  begin
    FRA := TVXSkyDomeStar(Source).FRA;
    FDec := TVXSkyDomeStar(Source).FDec;
    FMagnitude := TVXSkyDomeStar(Source).FMagnitude;
    FColor := TVXSkyDomeStar(Source).FColor;
    SetVector(FCacheCoord, TVXSkyDomeStar(Source).FCacheCoord);
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TVXSkyDomeStar.GetDisplayName: string;
begin
  Result := Format('RA: %5.1f / Dec: %5.1f', [RA, Dec]);
end;

// ------------------
// ------------------ TVXSkyDomeStars ------------------
// ------------------

// Create
//

constructor TVXSkyDomeStars.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVXSkyDomeStar);
end;

// GetOwner
//

function TVXSkyDomeStars.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// SetItems
//

procedure TVXSkyDomeStars.SetItems(index: Integer; const val: TVXSkyDomeStar);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TVXSkyDomeStars.GetItems(index: Integer): TVXSkyDomeStar;
begin
  Result := TVXSkyDomeStar(inherited Items[index]);
end;

// Add
//

function TVXSkyDomeStars.Add: TVXSkyDomeStar;
begin
  Result := (inherited Add) as TVXSkyDomeStar;
end;

// FindItemID
//

function TVXSkyDomeStars.FindItemID(ID: Integer): TVXSkyDomeStar;
begin
  Result := (inherited FindItemID(ID)) as TVXSkyDomeStar;
end;

// PrecomputeCartesianCoordinates
//

procedure TVXSkyDomeStars.PrecomputeCartesianCoordinates;
var
  i: Integer;
  star: TVXSkyDomeStar;
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

// BuildList
//

procedure TVXSkyDomeStars.BuildList(var rci: TVXRenderContextInfo; twinkle:
  Boolean);
var
  i, n: Integer;
  star: TVXSkyDomeStar;
  lastColor: TColor;
  lastPointSize10, pointSize10: Integer;
  color, twinkleColor: TColorVector;

  procedure DoTwinkle;
  begin
    if (n and 63) = 0 then
    begin
      twinkleColor := VectorScale(color, Random * 0.6 + 0.4);
      glColor3fv(@twinkleColor.X);
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

  rci.VXStates.Enable(stPointSmooth);
  rci.VXStates.Enable(stAlphaTest);
  rci.VXStates.SetAlphaFunction(cfNotEqual, 0.0);
  rci.VXStates.Enable(stBlend);
  rci.VXStates.SetBlendFunc(bfSrcAlpha, bfOne);

  glBegin(GL_POINTS);
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    pointSize10 := Round((4.5 - star.Magnitude) * 10);
    if pointSize10 <> lastPointSize10 then
    begin
      if pointSize10 > 15 then
      begin
        glEnd;
        lastPointSize10 := pointSize10;
        rci.VXStates.PointSize := pointSize10 * 0.1;
        glBegin(GL_POINTS);
      end
      else if lastPointSize10 <> 15 then
      begin
        glEnd;
        lastPointSize10 := 15;
        rci.VXStates.PointSize := 1.5;
        glBegin(GL_POINTS);
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
        glColor3fv(@color.X);
      lastColor := star.FColor;
    end
    else if twinkle then
      DoTwinkle;
    glVertex3fv(@star.FCacheCoord.X);
  end;
  glEnd;

  // restore default AlphaFunc
  rci.VXStates.SetAlphaFunction(cfGreater, 0);
end;

procedure TVXSkyDomeStars.AddRandomStars(const nb: Integer; const color: TColor;
  const limitToTopDome: Boolean = False);
var
  i: Integer;
  coord: TAffineVector;
  star: TVXSkyDomeStar;
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

// AddRandomStars
//

procedure TVXSkyDomeStars.AddRandomStars(const nb: Integer; const ColorMin,
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
  star: TVXSkyDomeStar;

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
    star.Color := RGB(RandomTT(ColorMin.X, ColorMax.X),
      RandomTT(ColorMin.Y, ColorMax.Y),
      RandomTT(ColorMin.Z, ColorMax.Z));
    // pick a magnitude
    star.Magnitude := Magnitude_min + Random * (Magnitude_max - Magnitude_min);
  end;
end;

// LoadStarsFile
//

procedure TVXSkyDomeStars.LoadStarsFile(const starsFileName: string);
var
  fs: TFileStream;
  sr: TVXStarRecord;
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
// ------------------ TVXSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TVXSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FBands := TVXSkyDomeBands.Create(Self);
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
  FStars := TVXSkyDomeStars.Create(Self);
end;

// Destroy
//

destructor TVXSkyDome.Destroy;
begin
  FStars.Free;
  FBands.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXSkyDome.Assign(Source: TPersistent);
begin
  if Source is TVXSkyDome then
  begin
    FBands.Assign(TVXSkyDome(Source).FBands);
    FStars.Assign(TVXSkyDome(Source).FStars);
  end;
  inherited;
end;

// SetBands
//

procedure TVXSkyDome.SetBands(const val: TVXSkyDomeBands);
begin
  FBands.Assign(val);
  StructureChanged;
end;

// SetStars
//

procedure TVXSkyDome.SetStars(const val: TVXSkyDomeStars);
begin
  FStars.Assign(val);
  StructureChanged;
end;

// SetOptions
//

procedure TVXSkyDome.SetOptions(const val: TVXSkyDomeOptions);
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

// BuildList
//

procedure TVXSkyDome.BuildList(var rci: TVXRenderContextInfo);
var
  f: Single;
begin
  // setup states
  rci.VXStates.Disable(stLighting); // 8
  rci.VXStates.Disable(stDepthTest);
  rci.VXStates.Disable(stFog);
  rci.VXStates.Disable(stCullFace);
  rci.VXStates.Disable(stBlend); // 2
  rci.VXStates.DepthWriteMask := 0;
  rci.VXStates.PolygonMode := pmFill;

  f := rci.rcci.farClippingDistance * 0.90;
  glScalef(f, f, f);

  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));
end;

// ------------------
// ------------------ TVXEarthSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TVXEarthSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMorning:=true;
  Bands.Clear;
  FSunElevation := 75;
  FTurbidity := 15;
  FSunZenithColor := TVXColor.CreateInitialized(Self, clrWhite, OnColorChanged);
  FSunDawnColor := TVXColor.CreateInitialized(Self, Vectormake(1, 0.5, 0, 0),OnColorChanged);
  FHazeColor := TVXColor.CreateInitialized(Self, VectorMake(0.9, 0.95, 1, 0),OnColorChanged);
  FSkyColor := TVXColor.CreateInitialized(Self, VectorMake(0.45, 0.6, 0.9, 0),OnColorChanged);
  FNightColor := TVXColor.CreateInitialized(Self, clrTransparent,OnColorChanged);
  FDeepColor := TVXColor.CreateInitialized(Self, VectorMake(0, 0.2, 0.4, 0));
  FStacks := 24;
  FSlices := 48;
  PreCalculate;
end;

// Destroy
//

destructor TVXEarthSkyDome.Destroy;
begin
  FSunZenithColor.Free;
  FSunDawnColor.Free;
  FHazeColor.Free;
  FSkyColor.Free;
  FNightColor.Free;
  FDeepColor.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVXEarthSkyDome.Assign(Source: TPersistent);
begin
  if Source is TVXSkyDome then
  begin
    FSunElevation := TVXEarthSkyDome(Source).SunElevation;
    FTurbidity := TVXEarthSkyDome(Source).Turbidity;
    FSunZenithColor.Assign(TVXEarthSkyDome(Source).FSunZenithColor);
    FSunDawnColor.Assign(TVXEarthSkyDome(Source).FSunDawnColor);
    FHazeColor.Assign(TVXEarthSkyDome(Source).FHazeColor);
    FSkyColor.Assign(TVXEarthSkyDome(Source).FSkyColor);
    FNightColor.Assign(TVXEarthSkyDome(Source).FNightColor);
    FSlices := TVXEarthSkyDome(Source).FSlices;
    FStacks := TVXEarthSkyDome(Source).FStacks;
    PreCalculate;
  end;
  inherited;
end;

// Loaded
//

procedure TVXEarthSkyDome.Loaded;
begin
  inherited;
  PreCalculate;
end;

// SetSunElevation
//

procedure TVXEarthSkyDome.SetSunElevation(const val: Single);
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

// SetTurbidity
//

procedure TVXEarthSkyDome.SetTurbidity(const val: Single);
begin
  FTurbidity := ClampValue(val, 1, 120);
  PreCalculate;
end;

// SetSunZenithColor
//

procedure TVXEarthSkyDome.SetSunZenithColor(const val: TVXColor);
begin
  FSunZenithColor.Assign(val);
  PreCalculate;
end;

// SetSunDawnColor
//

procedure TVXEarthSkyDome.SetSunDawnColor(const val: TVXColor);
begin
  FSunDawnColor.Assign(val);
  PreCalculate;
end;

// SetHazeColor
//

procedure TVXEarthSkyDome.SetHazeColor(const val: TVXColor);
begin
  FHazeColor.Assign(val);
  PreCalculate;
end;

// SetSkyColor
//

procedure TVXEarthSkyDome.SetSkyColor(const val: TVXColor);
begin
  FSkyColor.Assign(val);
  PreCalculate;
end;

// SetNightColor
//

procedure TVXEarthSkyDome.SetNightColor(const val: TVXColor);
begin
  FNightColor.Assign(val);
  PreCalculate;
end;

// SetDeepColor
//

procedure TVXEarthSkyDome.SetDeepColor(const val: TVXColor);
begin
  FDeepColor.Assign(val);
  PreCalculate;
end;

// SetSlices
//

procedure TVXEarthSkyDome.SetSlices(const val: Integer);
begin
  if val>6 then FSlices:=val else FSlices:=6;
  StructureChanged;
end;

// SetStacks
//

procedure TVXEarthSkyDome.SetStacks(const val: Integer);
begin
  if val>1 then FStacks:=val else FStacks:=1;
  StructureChanged;
end;

// BuildList
//

procedure TVXEarthSkyDome.BuildList(var rci: TVXRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.VxStates do
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
    DepthWriteMask := 0;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.95;
  glScalef(f, f, f);

  RenderDome;
  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));

  // restore
  rci.VXStates.DepthWriteMask := GLboolean(True);
end;

// OnColorChanged
//

procedure TVXEarthSkyDome.OnColorChanged(Sender: TObject);
begin
  PreCalculate;
end;

procedure TVXEarthSkyDome.SetSunAtTime(HH, MM: Single);
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
  color := RGB(round(ts * 255), round(ts * 255), round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then for i:=0 to Stars.Count-1 do stars[i].Color:=color;


  if esoRotateOnTwelveHours in ExtendedOptions then // spining around blue orb
  begin
    if (HH>=14) and (FMorning=true) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=false;
    end;

    if (HH>=2) and (HH<14) and (FMorning=false) then
    begin
      roll(180);
      for i:=0 to Stars.Count-1 do stars[i].RA:=Stars[i].RA+180;
      FMorning:=true;
    end;
  end;
  StructureChanged;
end;






// PreCalculate
//

procedure TVXEarthSkyDome.PreCalculate;
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
  color := RGB(round(ts * 255), round(ts * 255), round(ts * 255));
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

// CalculateColor
//

function TVXEarthSkyDome.CalculateColor(const theta, cosGamma: Single):
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

// SetSunElevation
//

procedure TVXEarthSkyDome.RenderDome;
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
    glBegin(GL_TRIANGLE_FAN);
    color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
    glColor4fv(DeepColor.AsAddress);
    glVertex3f(0, 0, -1);
    SinCosine(DegToRadian(stop), vertex1.Z, r);
    thetaStart := DegToRadian(90 - stop);
    for i := 0 to steps - 1 do
    begin
      vertex1.X := r * cosTable[i];
      vertex1.Y := r * sinTable[i];
      color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
      glColor4fv(@color);
      glVertex4fv(@vertex1);
    end;
    glEnd;
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
      glBegin(GL_TRIANGLE_FAN);
      color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
      glColor4fv(@color);
      glVertex4fv(@ZHmgPoint);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      thetaStart := DegToRadian(90 - start);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        glColor4fv(@color);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      glBegin(GL_TRIANGLE_STRIP);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      thetaStart := DegToRadian(90 - start);
      thetaStop := DegToRadian(90 - stop);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        glColor4fv(@color);
        glVertex4fv(@vertex1);
        vertex2.X := r2 * cosTable[i];
        vertex2.Y := r2 * sinTable[i];
        color := CalculateColor(thetaStop, CalculateCosGamma(vertex2));
        glColor4fv(@color);
        glVertex4fv(@vertex2);
      end;
      glEnd;
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
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  RegisterClasses([TVXSkyDome, TVXEarthSkyDome]);

end.
