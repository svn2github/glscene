//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{   Skydome object 
  
}
unit VKS.Skydome;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils, System.UITypes,
  FMX.Graphics,
   
  VKS.Scene, VKS.VectorGeometry, VKS.Graphics, VKS.CrossPlatform,
  VKS.VectorTypes, VKS.Color, VKS.RenderContextInfo;

type

  // TVKSkyDomeBand
  //
  TVKSkyDomeBand = class(TCollectionItem)
  private
    
    FStartAngle: Single;
    FStopAngle: Single;
    FStartColor: TVKColor;
    FStopColor: TVKColor;
    FSlices: Integer;
    FStacks: Integer;

  protected
    
    function GetDisplayName: string; override;
    procedure SetStartAngle(const val: Single);
    procedure SetStartColor(const val: TVKColor);
    procedure SetStopAngle(const val: Single);
    procedure SetStopColor(const val: TVKColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChange(sender: TObject);

  public
    
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo);

  published
    
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StartColor: TVKColor read FStartColor write SetStartColor;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property StopColor: TVKColor read FStopColor write SetStopColor;
    property Slices: Integer read FSlices write SetSlices default 12;
    property Stacks: Integer read FStacks write SetStacks default 1;
  end;

  // TVKSkyDomeBands
  //
  TVKSkyDomeBands = class(TCollection)
  protected
    
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKSkyDomeBand);
    function GetItems(index: Integer): TVKSkyDomeBand;

  public
    
    constructor Create(AOwner: TComponent);
    function Add: TVKSkyDomeBand;
    function FindItemID(ID: Integer): TVKSkyDomeBand;
    property Items[index: Integer]: TVKSkyDomeBand read GetItems write SetItems;
    default;

    procedure NotifyChange;
    procedure BuildList(var rci: TVKRenderContextInfo);
  end;

  // TVKSkyDomeStar
  //
  TVKSkyDomeStar = class(TCollectionItem)
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

  // TVKSkyDomeStars
  //
  TVKSkyDomeStars = class(TCollection)
  protected
    
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TVKSkyDomeStar);
    function GetItems(index: Integer): TVKSkyDomeStar;

    procedure PrecomputeCartesianCoordinates;

  public
    
    constructor Create(AOwner: TComponent);

    function Add: TVKSkyDomeStar;
    function FindItemID(ID: Integer): TVKSkyDomeStar;
    property Items[index: Integer]: TVKSkyDomeStar read GetItems write SetItems;
    default;

    procedure BuildList(var rci: TVKRenderContextInfo; twinkle: Boolean);

    { Adds nb random stars of the given color. 
       Stars are homogenously scattered on the complete sphere, not only the band defined or visible dome. }
    procedure AddRandomStars(const nb: Integer; const color: TColor; const limitToTopDome: Boolean = False); overload;
    procedure AddRandomStars(const nb: Integer; const ColorMin, ColorMax:TVector3b; const Magnitude_min, Magnitude_max: Single;const limitToTopDome: Boolean = False); overload;

    { Load a 'stars' file, which is made of TVKStarRecord. 
       Not that '.stars' files should already be sorted by magnitude and color. }
    procedure LoadStarsFile(const starsFileName: string);
  end;

  // TVKSkyDomeOption
  //
  TVKSkyDomeOption = (sdoTwinkle);
  TVKSkyDomeOptions = set of TVKSkyDomeOption;

  // TVKSkyDome
  //
    { Renders a sky dome always centered on the camera. 
       If you use this object make sure it is rendered *first*, as it ignores
       depth buffering and overwrites everything. All children of a skydome
       are rendered in the skydome's coordinate system. 
       The skydome is described by "bands", each "band" is an horizontal cut
       of a sphere, and you can have as many bands as you wish. 
       Estimated CPU cost (K7-500, GeForce SDR, default bands): 
        800x600 fullscreen filled: 4.5 ms (220 FPS, worst case)
        Geometry cost (0% fill): 0.7 ms (1300 FPS, best case)
         }
  TVKSkyDome = class(TVKCameraInvariantObject)
  private
    
    FOptions: TVKSkyDomeOptions;
    FBands: TVKSkyDomeBands;
    FStars: TVKSkyDomeStars;

  protected
    
    procedure SetBands(const val: TVKSkyDomeBands);
    procedure SetStars(const val: TVKSkyDomeStars);
    procedure SetOptions(const val: TVKSkyDomeOptions);

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BuildList(var rci: TVKRenderContextInfo); override;

  published
    
    property Bands: TVKSkyDomeBands read FBands write SetBands;
    property Stars: TVKSkyDomeStars read FStars write SetStars;
    property Options: TVKSkyDomeOptions read FOptions write SetOptions default [];
  end;

  TEarthSkydomeOption = (esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest);
  TEarthSkydomeOptions = set of TEarthSkydomeOption;

  // TVKEarthSkyDome
  //
  { Render a skydome like what can be seen on earth. 
     Color is based on sun position and turbidity, to "mimic" atmospheric
     Rayleigh and Mie scatterings. The colors can be adjusted to render
     weird/extra-terrestrial atmospheres too. 
     The default slices/stacks values make for an average quality rendering,
     for a very clean rendering, use 64/64 (more is overkill in most cases).
     The complexity is quite high though, making a T&L 3D board a necessity
     for using TVKEarthSkyDome. }
  TVKEarthSkyDome = class(TVKSkyDome)
  private
    
    FSunElevation: Single;
    FTurbidity: Single;
    FCurSunColor, FCurSkyColor, FCurHazeColor: TColorVector;
    FCurHazeTurbid, FCurSunSkyTurbid: Single;
    FSunZenithColor: TVKColor;
    FSunDawnColor: TVKColor;
    FHazeColor: TVKColor;
    FSkyColor: TVKColor;
    FNightColor: TVKColor;
    FDeepColor: TVKColor;
    FSlices, FStacks: Integer;
    FExtendedOptions: TEarthSkydomeOptions;
    FMorning: boolean;
  protected
    
    procedure Loaded; override;

    procedure SetSunElevation(const val: Single);
    procedure SetTurbidity(const val: Single);
    procedure SetSunZenithColor(const val: TVKColor);
    procedure SetSunDawnColor(const val: TVKColor);
    procedure SetHazeColor(const val: TVKColor);
    procedure SetSkyColor(const val: TVKColor);
    procedure SetNightColor(const val: TVKColor);
    procedure SetDeepColor(const val: TVKColor);
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

    procedure BuildList(var rci: TVKRenderContextInfo); override;

    procedure SetSunAtTime(HH, MM: Single);

  published
    
      { Elevation of the sun, measured in degrees. }
    property SunElevation: Single read FSunElevation write SetSunElevation;
    { Expresses the purity of air.  Value range is from 1 (pure athmosphere) to 120 (very nebulous) }
    property Turbidity: Single read FTurbidity write SetTurbidity;

    property SunZenithColor: TVKColor read FSunZenithColor write SetSunZenithColor;
    property SunDawnColor: TVKColor read FSunDawnColor write SetSunDawnColor;
    property HazeColor: TVKColor read FHazeColor write SetHazeColor;
    property SkyColor: TVKColor read FSkyColor write SetSkyColor;
    property NightColor: TVKColor read FNightColor write SetNightColor;
    property DeepColor: TVKColor read FDeepColor write SetDeepColor;
    property ExtendedOptions: TEarthSkydomeOptions read FExtendedOptions write FExtendedOptions;
    property Slices: Integer read FSlices write SetSlices default 24;
    property Stacks: Integer read FStacks write SetStacks default 48;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  Winapi.OpenGL, Winapi.OpenGLext, 
  VKS.Context,
  VKS.StarRecord,
  VKS.State;

// ------------------
// ------------------ TVKSkyDomeBand ------------------
// ------------------

// Create
//

constructor TVKSkyDomeBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStartColor := TVKColor.Create(Self);
  FStartColor.Initialize(clrBlue);
  FStartColor.OnNotifyChange := OnColorChange;
  FStopColor := TVKColor.Create(Self);
  FStopColor.Initialize(clrBlue);
  FStopColor.OnNotifyChange := OnColorChange;
  FSlices := 12;
  FStacks := 1;
end;

// Destroy
//

destructor TVKSkyDomeBand.Destroy;
begin
  FStartColor.Free;
  FStopColor.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKSkyDomeBand.Assign(Source: TPersistent);
begin
  if Source is TVKSkyDomeBand then
  begin
    FStartAngle := TVKSkyDomeBand(Source).FStartAngle;
    FStopAngle := TVKSkyDomeBand(Source).FStopAngle;
    FStartColor.Assign(TVKSkyDomeBand(Source).FStartColor);
    FStopColor.Assign(TVKSkyDomeBand(Source).FStopColor);
    FSlices := TVKSkyDomeBand(Source).FSlices;
    FStacks := TVKSkyDomeBand(Source).FStacks;
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TVKSkyDomeBand.GetDisplayName: string;
begin
  Result := Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

// SetStartAngle
//

procedure TVKSkyDomeBand.SetStartAngle(const val: Single);
begin
  FStartAngle := ClampValue(val, -90, 90);
  if FStartAngle > FStopAngle then FStopAngle := FStartAngle;
  TVKSkyDomeBands(Collection).NotifyChange;
end;

// SetStartColor
//

procedure TVKSkyDomeBand.SetStartColor(const val: TVKColor);
begin
  FStartColor.Assign(val);
end;

// SetStopAngle
//

procedure TVKSkyDomeBand.SetStopAngle(const val: Single);
begin
  FStopAngle := ClampValue(val, -90, 90);
  if FStopAngle < FStartAngle then
    FStartAngle := FStopAngle;
  TVKSkyDomeBands(Collection).NotifyChange;
end;

// SetStopColor
//

procedure TVKSkyDomeBand.SetStopColor(const val: TVKColor);
begin
  FStopColor.Assign(val);
end;

// SetSlices
//

procedure TVKSkyDomeBand.SetSlices(const val: Integer);
begin
  if val < 3 then
    FSlices := 3
  else
    FSlices := val;
  TVKSkyDomeBands(Collection).NotifyChange;
end;

// SetStacks
//

procedure TVKSkyDomeBand.SetStacks(const val: Integer);
begin
  if val < 1 then
    FStacks := 1
  else
    FStacks := val;
  TVKSkyDomeBands(Collection).NotifyChange;
end;

// OnColorChange
//

procedure TVKSkyDomeBand.OnColorChange(sender: TObject);
begin
  TVKSkyDomeBands(Collection).NotifyChange;
end;

// BuildList
//

procedure TVKSkyDomeBand.BuildList(var rci: TVKRenderContextInfo);

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
// ------------------ TVKSkyDomeBands ------------------
// ------------------

constructor TVKSkyDomeBands.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVKSkyDomeBand);
end;

function TVKSkyDomeBands.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TVKSkyDomeBands.SetItems(index: Integer; const val: TVKSkyDomeBand);
begin
  inherited Items[index] := val;
end;

function TVKSkyDomeBands.GetItems(index: Integer): TVKSkyDomeBand;
begin
  Result := TVKSkyDomeBand(inherited Items[index]);
end;

function TVKSkyDomeBands.Add: TVKSkyDomeBand;
begin
  Result := (inherited Add) as TVKSkyDomeBand;
end;

function TVKSkyDomeBands.FindItemID(ID: Integer): TVKSkyDomeBand;
begin
  Result := (inherited FindItemID(ID)) as TVKSkyDomeBand;
end;

procedure TVKSkyDomeBands.NotifyChange;
begin
  if Assigned(owner) and (owner is TVKBaseSceneObject) then TVKBaseSceneObject(owner).StructureChanged;
end;

// BuildList
//

procedure TVKSkyDomeBands.BuildList(var rci: TVKRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TVKSkyDomeStar ------------------
// ------------------

// Create
//

constructor TVKSkyDomeStar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

// Destroy
//

destructor TVKSkyDomeStar.Destroy;
begin
  inherited Destroy;
end;

// Assign
//

procedure TVKSkyDomeStar.Assign(Source: TPersistent);
begin
  if Source is TVKSkyDomeStar then
  begin
    FRA := TVKSkyDomeStar(Source).FRA;
    FDec := TVKSkyDomeStar(Source).FDec;
    FMagnitude := TVKSkyDomeStar(Source).FMagnitude;
    FColor := TVKSkyDomeStar(Source).FColor;
    SetVector(FCacheCoord, TVKSkyDomeStar(Source).FCacheCoord);
  end;
  inherited Destroy;
end;

// GetDisplayName
//

function TVKSkyDomeStar.GetDisplayName: string;
begin
  Result := Format('RA: %5.1f / Dec: %5.1f', [RA, Dec]);
end;

// ------------------
// ------------------ TVKSkyDomeStars ------------------
// ------------------

// Create
//

constructor TVKSkyDomeStars.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TVKSkyDomeStar);
end;

// GetOwner
//

function TVKSkyDomeStars.GetOwner: TPersistent;
begin
  Result := Owner;
end;

// SetItems
//

procedure TVKSkyDomeStars.SetItems(index: Integer; const val: TVKSkyDomeStar);
begin
  inherited Items[index] := val;
end;

// GetItems
//

function TVKSkyDomeStars.GetItems(index: Integer): TVKSkyDomeStar;
begin
  Result := TVKSkyDomeStar(inherited Items[index]);
end;

// Add
//

function TVKSkyDomeStars.Add: TVKSkyDomeStar;
begin
  Result := (inherited Add) as TVKSkyDomeStar;
end;

// FindItemID
//

function TVKSkyDomeStars.FindItemID(ID: Integer): TVKSkyDomeStar;
begin
  Result := (inherited FindItemID(ID)) as TVKSkyDomeStar;
end;

// PrecomputeCartesianCoordinates
//

procedure TVKSkyDomeStars.PrecomputeCartesianCoordinates;
var
  i: Integer;
  star: TVKSkyDomeStar;
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

procedure TVKSkyDomeStars.BuildList(var rci: TVKRenderContextInfo; twinkle:
  Boolean);
var
  i, n: Integer;
  star: TVKSkyDomeStar;
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

  rci.VKStates.Enable(stPointSmooth);
  rci.VKStates.Enable(stAlphaTest);
  rci.VKStates.SetAlphaFunction(cfNotEqual, 0.0);
  rci.VKStates.Enable(stBlend);
  rci.VKStates.SetBlendFunc(bfSrcAlpha, bfOne);

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
        rci.VKStates.PointSize := pointSize10 * 0.1;
        glBegin(GL_POINTS);
      end
      else if lastPointSize10 <> 15 then
      begin
        glEnd;
        lastPointSize10 := 15;
        rci.VKStates.PointSize := 1.5;
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

  // restore default GLScene AlphaFunc
  rci.VKStates.SetAlphaFunction(cfGreater, 0);
end;

// AddRandomStars
//

procedure TVKSkyDomeStars.AddRandomStars(const nb: Integer; const color: TColor;
  const limitToTopDome: Boolean = False);
var
  i: Integer;
  coord: TAffineVector;
  star: TVKSkyDomeStar;
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

procedure TVKSkyDomeStars.AddRandomStars(const nb: Integer; const ColorMin,
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
  star: TVKSkyDomeStar;

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

procedure TVKSkyDomeStars.LoadStarsFile(const starsFileName: string);
var
  fs: TFileStream;
  sr: TVKStarRecord;
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
// ------------------ TVKSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TVKSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FBands := TVKSkyDomeBands.Create(Self);
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
  FStars := TVKSkyDomeStars.Create(Self);
end;

// Destroy
//

destructor TVKSkyDome.Destroy;
begin
  FStars.Free;
  FBands.Free;
  inherited Destroy;
end;

// Assign
//

procedure TVKSkyDome.Assign(Source: TPersistent);
begin
  if Source is TVKSkyDome then
  begin
    FBands.Assign(TVKSkyDome(Source).FBands);
    FStars.Assign(TVKSkyDome(Source).FStars);
  end;
  inherited;
end;

// SetBands
//

procedure TVKSkyDome.SetBands(const val: TVKSkyDomeBands);
begin
  FBands.Assign(val);
  StructureChanged;
end;

// SetStars
//

procedure TVKSkyDome.SetStars(const val: TVKSkyDomeStars);
begin
  FStars.Assign(val);
  StructureChanged;
end;

// SetOptions
//

procedure TVKSkyDome.SetOptions(const val: TVKSkyDomeOptions);
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

procedure TVKSkyDome.BuildList(var rci: TVKRenderContextInfo);
var
  f: Single;
begin
  // setup states
  rci.VKStates.Disable(stLighting); // 8
  rci.VKStates.Disable(stDepthTest);
  rci.VKStates.Disable(stFog);
  rci.VKStates.Disable(stCullFace);
  rci.VKStates.Disable(stBlend); // 2
  rci.VKStates.DepthWriteMask := 0;
  rci.VKStates.PolygonMode := pmFill;

  f := rci.rcci.farClippingDistance * 0.90;
  glScalef(f, f, f);

  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));
end;

// ------------------
// ------------------ TVKEarthSkyDome ------------------
// ------------------

// CreateOwned
//

constructor TVKEarthSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMorning:=true;
  Bands.Clear;
  FSunElevation := 75;
  FTurbidity := 15;
  FSunZenithColor := TVKColor.CreateInitialized(Self, clrWhite, OnColorChanged);
  FSunDawnColor := TVKColor.CreateInitialized(Self, Vectormake(1, 0.5, 0, 0),OnColorChanged);
  FHazeColor := TVKColor.CreateInitialized(Self, VectorMake(0.9, 0.95, 1, 0),OnColorChanged);
  FSkyColor := TVKColor.CreateInitialized(Self, VectorMake(0.45, 0.6, 0.9, 0),OnColorChanged);
  FNightColor := TVKColor.CreateInitialized(Self, clrTransparent,OnColorChanged);
  FDeepColor := TVKColor.CreateInitialized(Self, VectorMake(0, 0.2, 0.4, 0));
  FStacks := 24;
  FSlices := 48;
  PreCalculate;
end;

// Destroy
//

destructor TVKEarthSkyDome.Destroy;
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

procedure TVKEarthSkyDome.Assign(Source: TPersistent);
begin
  if Source is TVKSkyDome then
  begin
    FSunElevation := TVKEarthSkyDome(Source).SunElevation;
    FTurbidity := TVKEarthSkyDome(Source).Turbidity;
    FSunZenithColor.Assign(TVKEarthSkyDome(Source).FSunZenithColor);
    FSunDawnColor.Assign(TVKEarthSkyDome(Source).FSunDawnColor);
    FHazeColor.Assign(TVKEarthSkyDome(Source).FHazeColor);
    FSkyColor.Assign(TVKEarthSkyDome(Source).FSkyColor);
    FNightColor.Assign(TVKEarthSkyDome(Source).FNightColor);
    FSlices := TVKEarthSkyDome(Source).FSlices;
    FStacks := TVKEarthSkyDome(Source).FStacks;
    PreCalculate;
  end;
  inherited;
end;

// Loaded
//

procedure TVKEarthSkyDome.Loaded;
begin
  inherited;
  PreCalculate;
end;

// SetSunElevation
//

procedure TVKEarthSkyDome.SetSunElevation(const val: Single);
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

procedure TVKEarthSkyDome.SetTurbidity(const val: Single);
begin
  FTurbidity := ClampValue(val, 1, 120);
  PreCalculate;
end;

// SetSunZenithColor
//

procedure TVKEarthSkyDome.SetSunZenithColor(const val: TVKColor);
begin
  FSunZenithColor.Assign(val);
  PreCalculate;
end;

// SetSunDawnColor
//

procedure TVKEarthSkyDome.SetSunDawnColor(const val: TVKColor);
begin
  FSunDawnColor.Assign(val);
  PreCalculate;
end;

// SetHazeColor
//

procedure TVKEarthSkyDome.SetHazeColor(const val: TVKColor);
begin
  FHazeColor.Assign(val);
  PreCalculate;
end;

// SetSkyColor
//

procedure TVKEarthSkyDome.SetSkyColor(const val: TVKColor);
begin
  FSkyColor.Assign(val);
  PreCalculate;
end;

// SetNightColor
//

procedure TVKEarthSkyDome.SetNightColor(const val: TVKColor);
begin
  FNightColor.Assign(val);
  PreCalculate;
end;

// SetDeepColor
//

procedure TVKEarthSkyDome.SetDeepColor(const val: TVKColor);
begin
  FDeepColor.Assign(val);
  PreCalculate;
end;

// SetSlices
//

procedure TVKEarthSkyDome.SetSlices(const val: Integer);
begin
  if val>6 then FSlices:=val else FSlices:=6;
  StructureChanged;
end;

// SetStacks
//

procedure TVKEarthSkyDome.SetStacks(const val: Integer);
begin
  if val>1 then FStacks:=val else FStacks:=1;
  StructureChanged;
end;

// BuildList
//

procedure TVKEarthSkyDome.BuildList(var rci: TVKRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.VKStates do
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
  rci.VKStates.DepthWriteMask := GLboolean(True);
end;

// OnColorChanged
//

procedure TVKEarthSkyDome.OnColorChanged(Sender: TObject);
begin
  PreCalculate;
end;

procedure TVKEarthSkyDome.SetSunAtTime(HH, MM: Single);
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

procedure TVKEarthSkyDome.PreCalculate;
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

function TVKEarthSkyDome.CalculateColor(const theta, cosGamma: Single):
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

procedure TVKEarthSkyDome.RenderDome;
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

  RegisterClasses([TVKSkyDome, TVKEarthSkyDome]);

end.
