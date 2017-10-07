{ -----------------------------------------------------------------------------
  Unit Name: cUsersSettings
  Author:    Aaron Hochwimmer
  Purpose:   Stores the user's settings
  $Id: cUsersSettings.pas,v 1.25 2004/07/08 09:54:53 hochwimmera Exp $
  ----------------------------------------------------------------------------- }
unit cUsersSettings;

interface

uses
  System.Sysutils, System.Win.Registry,
  Vcl.Forms, Vcl.Graphics,

   
  GLTexture, GLVectorgeometry, GLColor,

  // gldata units
  cColourSpectrum, cUtilities;

const
  CREGISTRYKEY = '\Software\glData';

type
  TColourScaleOptions = class(TObject)
  private
    fAlpha: double;
    fRegPath: string;
    fContinuous: boolean;
    fVisible: boolean;
    fBorder: boolean;
    fPoints: integer;
    fScaleFormat: string;
    fScaleSteps: boolean;
    fShowContourPoints: boolean;
    fColourScaleType: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    property Alpha: double read fAlpha write fAlpha;
    property RegPath: string read fRegPath write fRegPath;
    property Border: boolean read fBorder write fBorder;
    property Continuous: boolean read fContinuous write fContinuous;
    property Points: integer read fPoints write fPoints;
    property ScaleSteps: boolean read fScaleSteps write fScaleSteps;
    property Scaleformat: string read fScaleFormat write fScaleFormat;
    property ShowContourPoints: boolean read fShowContourPoints
      write fShowContourPoints;
    property ColourScaleType: integer read fColourScaleType
      write fColourScaleType;
    property Visible: boolean read fVisible write fVisible;
  end;

  TVectorFieldOptions = class(TObject)
  private
    fBoxLineAA: boolean;
    fBoxLineColour: TColor;
    fBoxLinePattern: integer;
    fBoxLineSmooth: boolean;

    fColPalette: TColourSpectrum;
    fMaxArrowHeadHeight: double;
    fMaxArrowHeadRadius: double;
    fMaxArrowLength: double;
    fMaxArrowRadius: double;
    fMinArrowHeadHeight: double;
    fMinArrowHeadRadius: double;
    fMinArrowLength: double;
    fMinArrowRadius: double;
    fRegPath: string;
    fSlices: integer;
    fStacks: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;

    property BoxLineAA: boolean read fBoxLineAA write fBoxLineAA;
    property BoxLineColour: TColor read fBoxLineColour write fBoxLineColour;
    property BoxLinePattern: integer read fBoxLinePattern write fBoxLinePattern;
    property BoxLineSmooth: boolean read fBoxLineSmooth write fBoxLineSmooth;

    property ColPalette: TColourSpectrum read fColPalette write fColPalette;
    property MaxArrowHeadHeight: double read fMaxArrowHeadHeight
      write fMaxArrowHeadHeight;
    property MaxArrowHeadRadius: double read fMaxArrowHeadRadius
      write fMaxArrowHeadRadius;
    property MaxArrowLength: double read fMaxArrowLength write fMaxArrowLength;
    property MaxArrowRadius: double read fMaxArrowRadius write fMaxArrowRadius;
    property MinArrowHeadHeight: double read fMinArrowHeadHeight
      write fMinArrowHeadHeight;
    property MinArrowHeadRadius: double read fMinArrowHeadRadius
      write fMinArrowHeadRadius;
    property MinArrowLength: double read fMinArrowLength write fMinArrowLength;
    property MinArrowRadius: double read fMinArrowRadius write fMinArrowRadius;
    property Slices: integer read fSlices write fSlices;
    property Stacks: integer read fStacks write fStacks;
    property RegPath: string read fRegPath write fRegPath;
  end;

  TGridOptions = class(TObject)
  private
    fAlpha: double;
    fColourMode: integer;
    fColPalette: TColourSpectrum;
    fColsBlank: TGLColor;
    // when initially loading a grid it is shown/hidden based on CreateVisible
    fCreateVisible: boolean;

    fDefaultDir: string;
    fglCol_Bottom: TGLColor;
    fglCol_Top: TGLColor;

    fModified: boolean;
    fName: string;

    fPolygonMode: integer;
    fPromptTexture: boolean;
    fRegPath: string;
    fSilentImport: boolean;
    fSilentLoad: boolean;
    fTileX: integer;
    fTileY: integer;
    fTwoSidedMesh: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    function GetSpectrum(dRatio, dValue: double): TColorVector;

    property Alpha: double read fAlpha write fAlpha;
    property ColourMode: integer read fColourMode write fColourMode;
    property ColPalette: TColourSpectrum read fColPalette write fColPalette;
    property ColsBlank: TGLColor read fColsBlank write fColsBlank;
    property CreateVisible: boolean read fCreateVisible write fCreateVisible;

    property Modified: boolean read fModified write fModified;
    property DefaultDir: string read fDefaultDir write fDefaultDir;
    property glCol_Bottom: TGLColor read fglCol_Bottom write fglCol_Bottom;
    property glCol_Top: TGLColor read fglCol_Top write fglCol_Top;
    property Name: string read fName write fName;
    property PolygonMode: integer read fPolygonMode write fPolygonMode;
    property PromptTexture: boolean read fPromptTexture write fPromptTexture;
    property RegPath: string read fRegPath write fRegPath;
    property SilentImport: boolean read fSilentImport write fSilentImport;
    property SilentLoad: boolean read fSilentLoad write fSilentLoad;
    property TileX: integer read fTileX write fTileX;
    property TileY: integer read fTileY write fTileY;
    property TwoSidedMesh: boolean read fTwoSidedMesh write fTwoSidedMesh;
  end;

  TGLDataUserSettings = class(TObject)
  private
    fAntialiasing: integer;

    fGeoColPalette: TColourSpectrum;
    fGeoModified: boolean;
    fArcInfoGrid: TGridOptions;
    fSurferGrid: TGridOptions;

    fColPalette: TColourSpectrum;
    fColourScaleOptions: TColourScaleOptions;
    fAutoFocusGrids: boolean;
    fAutoFocusPoints: boolean;
    fAutoProcess: boolean;
    faValue: double;
    fBackGroundColour: TColor;
    fCameraDepthOfView: double;
    fCameraFocalLength: double;
    fCameraNearPlaneBias: double;
    fCameraStyle: integer;
    fDisplayAxes: boolean;
    fDisplayLines: boolean;
    fDisplayMarkers: boolean;
    fDisplayPipe: boolean;
    fDisplayPoints: boolean;
    fInvertMouseWheel: boolean;
    fLighting: boolean;
    fLighting2: boolean;
    fLineColour: TColor;
    fLineMode: integer;

    fMarkerBoxLineAA: boolean;
    fMarkerBoxLineColour: TColor;
    fMarkerBoxLinePattern: integer;
    fMarkerBoxLineSmooth: boolean;

    fMarkerColour: TColor;
    fMarkerRadius: double;
    fMarkerWireFrame: boolean;
    fPointColourNull: TGLColor;
    fPointStyle: integer;
    fRenderNullPoints: boolean;
    fPipeRadius: double;
    fScaleHUD: boolean;
    fFocusStatus: boolean;
    fCameraStatus: boolean;
    fScaleX: double;
    fScaleY: double;
    fScaleZ: double;
    fShadeModel: integer; // 0 = default, 1 = flat, 2 = smooth

    fTwoSideLighting: boolean;

    // vector field properties
    fVectorFieldOptions: TVectorFieldOptions;

  public
    constructor Create;
    destructor Destroy; override;
    function GetScalarSpectrum(dRatio, dValue: double): TColorVector;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;

    // 0 = colpalette
    procedure UpdatePalette(iMode: integer);

    property AntiAliasing: integer read fAntialiasing write fAntialiasing;

    property ColPalette: TColourSpectrum read fColPalette write fColPalette;
    property geocolpalette: TColourSpectrum read fGeoColPalette
      write fGeoColPalette;
    property GeoModified: boolean read fGeoModified write fGeoModified;

    property VectorFieldOptions: TVectorFieldOptions read fVectorFieldOptions
      write fVectorFieldOptions;

    property ColourScaleOptions: TColourScaleOptions read fColourScaleOptions
      write fColourScaleOptions;
    property ArcInfoGrid: TGridOptions read fArcInfoGrid write fArcInfoGrid;
    property AutoFocusGrids: boolean read fAutoFocusGrids write fAutoFocusGrids;
    property AutoFocusPoints: boolean read fAutoFocusPoints
      write fAutoFocusPoints;
    property AutoProcess: boolean read fAutoProcess write fAutoProcess;
    property aValue: double read faValue write faValue;
    property BackGroundColour: TColor read fBackGroundColour
      write fBackGroundColour;

    property CameraDepthOfView: double read fCameraDepthOfView
      write fCameraDepthOfView;
    property CameraFocalLength: double read fCameraFocalLength
      write fCameraFocalLength;
    property CameraNearPlaneBias: double read fCameraNearPlaneBias
      write fCameraNearPlaneBias;
    property CameraStyle: integer read fCameraStyle write fCameraStyle;

    property DisplayAxes: boolean read fDisplayAxes write fDisplayAxes;
    property DisplayLines: boolean read fDisplayLines write fDisplayLines;
    property DisplayMarkers: boolean read fDisplayMarkers write fDisplayMarkers;
    property DisplayPipe: boolean read fDisplayPipe write fDisplayPipe;
    property DisplayPoints: boolean read fDisplayPoints write fDisplayPoints;
    property InvertMouseWheel: boolean read fInvertMouseWheel
      write fInvertMouseWheel;
    property FocusStatus: boolean read fFocusStatus write fFocusStatus;
    property CameraStatus: boolean read fCameraStatus write fCameraStatus;

    property Lighting: boolean read fLighting write fLighting;
    property Lighting2: boolean read fLighting2 write fLighting2;

    property LineColour: TColor read fLineColour write fLineColour;
    property LineMode: integer read fLineMode write fLineMode;

    property MarkerBoxLineAA: boolean read fMarkerBoxLineAA
      write fMarkerBoxLineAA;
    property MarkerBoxLineColour: TColor read fMarkerBoxLineColour
      write fMarkerBoxLineColour;
    property MarkerBoxLinePattern: integer read fMarkerBoxLinePattern
      write fMarkerBoxLinePattern;
    property MarkerBoxLineSmooth: boolean read fMarkerBoxLineSmooth
      write fMarkerBoxLineSmooth;

    property MarkerColour: TColor read fMarkerColour write fMarkerColour;
    property MarkerRadius: double read fMarkerRadius write fMarkerRadius;
    property MarkerWireFrame: boolean read fMarkerWireFrame
      write fMarkerWireFrame;
    property PointColourNull: TGLColor read fPointColourNull
      write fPointColourNull;
    property PointStyle: integer read fPointStyle write fPointStyle;
    property PipeRadius: double read fPipeRadius write fPipeRadius;

    property RenderNullPoints: boolean read fRenderNullPoints
      write fRenderNullPoints;
    property ScaleHUD: boolean read fScaleHUD write fScaleHUD;
    property ScaleX: double read fScaleX write fScaleX;
    property ScaleY: double read fScaleY write fScaleY;
    property ScaleZ: double read fScaleZ write fScaleZ;

    property ShadeModel: integer read fShadeModel write fShadeModel;

    property SurferGrid: TGridOptions read fSurferGrid write fSurferGrid;
    property TwoSideLighting: boolean read fTwoSideLighting
      write fTwoSideLighting;
  end;

function ReadRegBool(myReg: TRegistry; sEntry: string;
  bDefault: boolean): boolean;
function ReadRegColour(myReg: TRegistry; sEntry: string; dCol: TColor): TColor;
function ReadRegFloat(myReg: TRegistry; sEntry: string;
  dDefault: double): double;
function ReadRegInteger(myReg: TRegistry; sEntry: string;
  iDefault: integer): integer;
function ReadRegString(myReg: TRegistry; sEntry, sDefault: string): string;

implementation

// ----- ReadRegBool -----------------------------------------------------------
function ReadRegBool(myReg: TRegistry; sEntry: string;
  bDefault: boolean): boolean;

begin
  if myReg.ValueExists(sEntry) then
    result := myReg.ReadBool(sEntry)
  else
    result := bDefault;
end;

// ----- ReadRegColour ---------------------------------------------------------
function ReadRegColour(myReg: TRegistry; sEntry: string; dCol: TColor): TColor;

begin
  if myReg.ValueExists(sEntry) then
    result := HexToColor(myReg.ReadString(sEntry))
  else
    result := dCol;
end;

// ----- ReadRegFloat ----------------------------------------------------------
function ReadRegFloat(myReg: TRegistry; sEntry: string;
  dDefault: double): double;

begin
  if myReg.ValueExists(sEntry) then
    result := myReg.ReadFloat(sEntry)
  else
    result := dDefault;
end;

// ----- ReadRegInteger --------------------------------------------------------
function ReadRegInteger(myReg: TRegistry; sEntry: string;
  iDefault: integer): integer;

begin
  if myReg.ValueExists(sEntry) then
    result := myReg.ReadInteger(sEntry)
  else
    result := iDefault;
end;

// ----- ReadRegString ---------------------------------------------------------
{ ** reads the registry string defined by sEntry. Returns sDefault if not present
  Note myReg is assumed to be opened on the correct key }
function ReadRegString(myReg: TRegistry; sEntry, sDefault: string): string;

begin
  if myReg.ValueExists(sEntry) then
    result := myReg.ReadString(sEntry)
  else
    result := sDefault;
end;

// =============================================================================
// TColourScaleOptions Implementation
// =============================================================================
constructor TColourScaleOptions.Create;

begin
  inherited Create;
end;

// ----- TColourScaleOptions.Destroy -------------------------------------------
destructor TColourScaleOptions.Destroy;

begin
  inherited Destroy;
end;

// ----- TColourScaleOptions.LoadFromRegistry ----------------------------------
procedure TColourScaleOptions.LoadFromRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.CreateKey(RegPath);
  myReg.OpenKey(RegPath, false);
  fVisible := ReadRegBool(myReg, 'visible', false);
  fPoints := ReadRegInteger(myReg, 'points', 10);
  fScaleSteps := ReadRegBool(myReg, 'scalesteps', true);
  fContinuous := ReadRegBool(myReg, 'continuous', true);
  fBorder := ReadRegBool(myReg, 'border', true);
  fScaleFormat := ReadRegString(myReg, 'scaleformat', '%5.2g');
  fShowContourPoints := ReadRegBool(myReg, 'showcontourpoints', false);
  fAlpha := ReadRegFloat(myReg, 'alpha', 1.0);
  fColourScaleType := ReadRegInteger(myReg, 'scaletype', 0);
  myReg.CloseKey;
  myReg.Free;
end;

// ----- TColourScaleOptions.SaveToRegistry ------------------------------------
procedure TColourScaleOptions.SaveToRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.OpenKey(RegPath, false);
  myReg.WriteBool('visible', fVisible);
  myReg.WriteInteger('points', fPoints);
  myReg.WriteBool('scalesteps', fScaleSteps);
  myReg.WriteBool('continuous', fContinuous);
  myReg.WriteBool('border', fBorder);
  myReg.WriteBool('showcontourpoints', fShowContourPoints);
  myReg.WriteString('scaleformat', fScaleFormat);
  myReg.WriteFloat('alpha', fAlpha);
  myReg.WriteInteger('scaletype', fColourScaleType);
  myReg.CloseKey;
  myReg.Free;
end;

// =============================================================================
// TVectorFieldOptions Implementation
// =============================================================================
// ----- TVectorFieldOptions.Create --------------------------------------------
constructor TVectorFieldOptions.Create;

begin
  inherited Create;
  fColPalette := TColourSpectrum.Create;
  LoadFromRegistry;
end;

// ----- TVectorFieldOptions.Destroy -------------------------------------------
destructor TVectorFieldOptions.Destroy;

begin
  fColPalette.Free;
  inherited Destroy;
end;

// ----- TVectorFieldOptions.LoadFromRegistry ----------------------------------
procedure TVectorFieldOptions.LoadFromRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.CreateKey(RegPath);
  myReg.OpenKey(RegPath, false);

  fBoxLineAA := ReadRegBool(myReg, 'boxlineAA', false);
  fBoxLineColour := ReadRegColour(myReg, 'boxlinecolour', clGray);
  fBoxLinePattern := ReadRegInteger(myReg, 'boxlinepattern', 65535);
  fBoxLineSmooth := ReadRegBool(myReg, 'boxlinesmooth', false);

  fMinArrowLength := ReadRegFloat(myReg, 'minarrowlength', 0.0);
  fMaxArrowLength := ReadRegFloat(myReg, 'maxarrowlength', 1.0);
  fMinArrowRadius := ReadRegFloat(myReg, 'minarrowradius', 0.0);
  fMaxArrowRadius := ReadRegFloat(myReg, 'maxarrowradius', 0.1);
  fMinArrowHeadRadius := ReadRegFloat(myReg, 'minarrowheadradius', 0.0);
  fMaxArrowHeadRadius := ReadRegFloat(myReg, 'maxarrowheadradius', 0.2);
  fMinArrowHeadHeight := ReadRegFloat(myReg, 'minarrowheadheight', 0.0);
  fMaxArrowHeadHeight := ReadRegFloat(myReg, 'maxarrowheadheight', 0.5);
  fSlices := ReadRegInteger(myReg, 'slices', 4);
  fStacks := ReadRegInteger(myReg, 'stacks', 1);

  ColPalette.SpectrumFile := ReadRegString(myReg, 'clr', '');
  ColPalette.SingleColour := ReadRegColour(myReg, 'coloursingle', clGreen);
  ColPalette.MinColour := ReadRegColour(myReg, 'colourmin', clBlue);
  ColPalette.MaxColour := ReadRegColour(myReg, 'colourmax', clFuchsia);
  ColPalette.SpectrumMode := ReadRegInteger(myReg, 'spectrummode', 2);

  myReg.CloseKey;
  myReg.Free;
end;

// ----- TVectorFieldOptions.SaveToRegistry ------------------------------------
procedure TVectorFieldOptions.SaveToRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.OpenKey(RegPath, false);

  myReg.WriteBool('boxlineAA', fBoxLineAA);
  myReg.WriteString('boxlinecolour', ColorToHex(fBoxLineColour));
  myReg.WriteBool('boxlinesmooth', fBoxLineSmooth);
  myReg.WriteInteger('boxlinepattern', fBoxLinePattern);

  myReg.WriteFloat('minarrowlength', fMinArrowLength);
  myReg.WriteFloat('maxarrowlength', fMaxArrowLength);
  myReg.WriteFloat('minarrowradius', fMinArrowRadius);
  myReg.WriteFloat('minarrowradius', fMinArrowRadius);
  myReg.WriteFloat('minarrowheadradius', fMinArrowHeadRadius);
  myReg.WriteFloat('maxarrowheadradius', fMaxArrowHeadRadius);
  myReg.WriteFloat('minarrowheadheight', fMinArrowHeadHeight);
  myReg.WriteFloat('maxarrowheadheight', fMaxArrowHeadHeight);
  myReg.WriteInteger('slices', fSlices);
  myReg.WriteInteger('stacks', fStacks);

  myReg.WriteString('clr', ColPalette.SpectrumFile);
  myReg.WriteString('coloursingle', ColorToHex(ColPalette.SingleColour));
  myReg.WriteString('colourmin', ColorToHex(ColPalette.MinColour));
  myReg.WriteString('colourmax', ColorToHex(ColPalette.MaxColour));
  myReg.WriteInteger('spectrummode', ColPalette.SpectrumMode);

  myReg.CloseKey;
  myReg.Free;
end;

// =============================================================================
// TGridOptions Implementation
// =============================================================================
// ----- TGridOptions.Create ---------------------------------------------------
constructor TGridOptions.Create;

begin
  inherited Create;
  fColPalette := TColourSpectrum.Create;
  fglCol_Bottom := TGLColor.Create(nil);
  fglCol_Top := TGLColor.Create(nil);
  fColsBlank := TGLColor.Create(nil);

  fModified := true;
end;

// ----- TGridOptions.Destroy --------------------------------------------------
destructor TGridOptions.Destroy;

begin
  fColPalette.Free;

  fglCol_Bottom.Free;
  fglCol_Top.Free;

  fColsBlank.Free;
  inherited Destroy;
end;

// ----- TGridOptions.LoadFromRegistry -----------------------------------------
procedure TGridOptions.LoadFromRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.CreateKey(RegPath);
  myReg.OpenKey(RegPath, false);

  fAlpha := ReadRegFloat(myReg, 'alpha', 1.0);
  fCreateVisible := ReadRegBool(myReg, 'createvisible', true);

  fTileX := ReadRegInteger(myReg, 'tilex', 1);
  fTileY := ReadRegInteger(myReg, 'tiley', 1);
  fPromptTexture := ReadRegBool(myReg, 'prompttexture', true);
  fColourMode := ReadRegInteger(myReg, 'colourmode', 1);
  fTwoSidedMesh := ReadRegBool(myReg, 'twosidedmesh', true);
  fDefaultDir := ReadRegString(myReg, 'defaultdir',
    ExtractFilePath(ParamStr(0)) + 'samples\' + fName + '\');
  fSilentImport := ReadRegBool(myReg, 'silentimport', false);
  fSilentLoad := ReadRegBool(myReg, 'silentload', true);

  ColPalette.SpectrumFile := ReadRegString(myReg, 'clr', '');
  ColPalette.SingleColour := ReadRegColour(myReg, 'coloursingle', clGreen);
  ColPalette.MinColour := ReadRegColour(myReg, 'colourmin', clBlue);
  ColPalette.MaxColour := ReadRegColour(myReg, 'colourmax', clFuchsia);
  ColPalette.SpectrumMode := ReadRegInteger(myReg, 'spectrummode', 2);

  fColsBlank.AsWinColor := ReadRegColour(myReg, 'colourblank', clSilver);
  fPolygonMode := ReadRegInteger(myReg, 'polygonmode', 0);
  myReg.CloseKey;
  myReg.Free;
end;

// ----- TGridOptions.SaveToRegistry -------------------------------------------
procedure TGridOptions.SaveToRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.OpenKey(RegPath, false);

  myReg.WriteFloat('alpha', fAlpha);
  myReg.WriteBool('createvisible', fCreateVisible);

  myReg.WriteInteger('tilex', fTileX);
  myReg.WriteInteger('tiley', fTileY);
  myReg.WriteBool('prompttexture', fPromptTexture);

  myReg.WriteInteger('colourmode', fColourMode);
  myReg.WriteBool('twosidedmesh', fTwoSidedMesh);

  myReg.WriteString('defaultdir', fDefaultDir);
  myReg.WriteBool('silentimport', fSilentImport);
  myReg.WriteBool('silentload', fSilentLoad);

  myReg.WriteString('clr', ColPalette.SpectrumFile);
  myReg.WriteString('coloursingle', ColorToHex(ColPalette.SingleColour));
  myReg.WriteString('colourmin', ColorToHex(ColPalette.MinColour));
  myReg.WriteString('colourmax', ColorToHex(ColPalette.MaxColour));
  myReg.WriteString('colourblank', ColorToHex(fColsBlank.AsWinColor));
  myReg.WriteInteger('spectrummode', ColPalette.SpectrumMode);

  myReg.WriteInteger('polygonmode', fPolygonMode);
  myReg.CloseKey;
  myReg.Free;
end;

// ----- TGridOptions.GetSpectrum ----------------------------------------------
function TGridOptions.GetSpectrum(dRatio, dValue: double): TColorVector;

begin
  case ColPalette.SpectrumMode of
    // single colour
    0:
      result := ColPalette.GetColourVector(dValue, true);
    // simple minimum/maximum
    1:
      result := ColPalette.GetColourVector(dValue, true);
    // rainbow/inverse rainbow
    2, 3:
      result := ColPalette.GetColourVector(dValue,
        (ColPalette.SpectrumMode = 2));
    // palettee
    4, 5:
      result := ColPalette.GetColourVector(dValue,
        (ColPalette.SpectrumMode = 4));
  end;
end;

// =============================================================================
// ----- TGLDataUserSettings.Create --------------------------------------------
constructor TGLDataUserSettings.Create;

begin
  inherited Create;

  SurferGrid := TGridOptions.Create;
  SurferGrid.Name := 'surfer';
  SurferGrid.RegPath := CREGISTRYKEY + '\grids\surfer';

  ArcInfoGrid := TGridOptions.Create;
  ArcInfoGrid.Name := 'arcinfo';
  ArcInfoGrid.RegPath := CREGISTRYKEY + '\grids\arcinfo';

  ColourScaleOptions := TColourScaleOptions.Create;
  ColourScaleOptions.RegPath := CREGISTRYKEY + '\display\colourscale';

  PointColourNull := TGLColor.Create(nil);

  ColPalette := TColourSpectrum.Create;

  // geothermal stuff should be put in a different class
  geocolpalette := TColourSpectrum.Create;
  fGeoModified := true;

  VectorFieldOptions := TVectorFieldOptions.Create;
  VectorFieldOptions.RegPath := CREGISTRYKEY + '\vectorfield';
end;

// ----- TGLDataUserSettings.Destroy -------------------------------------------
destructor TGLDataUserSettings.Destroy;

begin
  VectorFieldOptions.Free;

  SurferGrid.Free;
  ArcInfoGrid.Free;
  ColourScaleOptions.Free;
  PointColourNull.Free;
  ColPalette.Free;
  geocolpalette.Free;
  inherited Destroy;
end;

// ----- TGLDataUserSettings.GetScalarSpectrum -------------------------------
function TGLDataUserSettings.GetScalarSpectrum(dRatio, dValue: double)
  : TColorVector;

begin
  case ColPalette.SpectrumMode of
    // single colour
    0:
      result := ColPalette.GetColourVector(dValue, true);
    // simple minimum/maximum
    1:
      result := ColPalette.GetColourVector(dValue, true);
    // rainbow and inverse rainbow spectrum
    2, 3:
      result := ColPalette.GetColourVector(dValue,
        (ColPalette.SpectrumMode = 2));
    // using a palette
    4, 5:
      result := ColPalette.GetColourVector(dValue,
        (ColPalette.SpectrumMode = 4))
  end;
end;

// ----- TGLDataUserSettings.LoadFromRegistry ----------------------------------
procedure TGLDataUserSettings.LoadFromRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  myReg.OpenKey(CREGISTRYKEY + '\general', true);
  fAutoProcess := ReadRegBool(myReg, 'autoprocess', true);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\interface', true);

  fFocusStatus := ReadRegBool(myReg, 'focusstatus', true);
  fCameraStatus := ReadRegBool(myReg, 'camerastatus', true);
  fInvertMouseWheel := ReadRegBool(myReg, 'invertmousewheel', false);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\coordinates', true);
  faValue := ReadRegFloat(myReg, 'aValue', 1.0);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display', true);
  fAntialiasing := ReadRegInteger(myReg, 'antialiasing', 1); // default
  fBackGroundColour := ReadRegColour(myReg, 'backgroundcolour', clWhite);
  fTwoSideLighting := ReadRegBool(myReg, 'twosidelighting', false);
  fShadeModel := ReadRegInteger(myReg, 'shademodel', 0); // default
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display\axes', true);
  fDisplayAxes := ReadRegBool(myReg, 'displayaxes', true);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display\camera', true);
  fCameraStyle := ReadRegInteger(myReg, 'camerastyle', 2);
  fCameraFocalLength := ReadRegFloat(myReg, 'focallength', 50.0);
  fCameraDepthOfView := ReadRegFloat(myReg, 'depthofview', 100.0);
  fCameraNearPlaneBias := ReadRegFloat(myReg, 'nearplanebias', 1.0);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display\lighting', true);
  fLighting := ReadRegBool(myReg, 'enabled', true);
  fLighting2 := ReadRegBool(myReg, 'enabled2', true);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\data\markers', true);
  fDisplayMarkers := ReadRegBool(myReg, 'displaymarkers', true);
  fDisplayPoints := ReadRegBool(myReg, 'displaypoints', true);

  fMarkerBoxLineAA := ReadRegBool(myReg, 'markerboxlineaa', false);
  fMarkerBoxLineColour := ReadRegColour(myReg, 'markerboxlinecolour', clGray);
  fMarkerBoxLinePattern := ReadRegInteger(myReg, 'markerboxlinepattern', 65535);
  fMarkerBoxLineSmooth := ReadRegBool(myReg, 'markerboxlinesmooth', false);

  fMarkerRadius := ReadRegFloat(myReg, 'radius', 0.01);
  fMarkerColour := ReadRegColour(myReg, 'markercolour', clRed);
  fMarkerWireFrame := ReadRegBool(myReg, 'wireframe', false);
  fAutoFocusPoints := ReadRegBool(myReg, 'autofocus', true);

  // obtain spectrum file first !
  ColPalette.SpectrumFile := ReadRegString(myReg, 'clr', '');
  ColPalette.SingleColour := ReadRegColour(myReg, 'pointcoloursingle', clGreen);
  ColPalette.MinColour := ReadRegColour(myReg, 'pointcolourmin', clBlue);
  ColPalette.MaxColour := ReadRegColour(myReg, 'pointcolourmax', clRed);
  ColPalette.SpectrumMode := ReadRegInteger(myReg, 'pointspectrummode', 2);
  fPointColourNull.AsWinColor := ReadRegColour(myReg,
    'pointcolournull', clBlack);
  fRenderNullPoints := ReadRegBool(myReg, 'rendernullpoints', true);
  fPointStyle := ReadRegInteger(myReg, 'pointstyle', 3);
  myReg.CloseKey;

  // geothermal grid spectrum file
  myReg.OpenKey(CREGISTRYKEY + '\geothermal', true);
  geocolpalette.SpectrumFile := ReadRegString(myReg, 'clr', '');
  geocolpalette.SingleColour := ReadRegColour(myReg,
    'pointcoloursingle', clGreen);
  geocolpalette.MinColour := ReadRegColour(myReg, 'pointcolourmin', clBlue);
  geocolpalette.MaxColour := ReadRegColour(myReg, 'pointcolourmax', clRed);
  geocolpalette.SpectrumMode := ReadRegInteger(myReg, 'pointspectrummode', 2);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\data\lines', true);
  fDisplayLines := ReadRegBool(myReg, 'displaylines', true);
  fLineColour := ReadRegColour(myReg, 'linecolour', clBlue);
  fLineMode := ReadRegInteger(myReg, 'linemode', 1);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\data\pipe', true);
  fDisplayPipe := ReadRegBool(myReg, 'displaypipe', false);
  fPipeRadius := ReadRegFloat(myReg, 'radius', 0.01);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display\scale', true);
  fScaleX := ReadRegFloat(myReg, 'scaleX', 1.0);
  fScaleY := ReadRegFloat(myReg, 'scaleY', 1.0);
  fScaleZ := ReadRegFloat(myReg, 'scaleZ', 1.0);
  fScaleHUD := ReadRegBool(myReg, 'scaleHUD', true);
  myReg.CloseKey;

  myReg.OpenKey(CREGISTRYKEY + '\display\scale', true);
  fAutoFocusGrids := ReadRegBool(myReg, 'autofocus', true);
  myReg.CloseKey;
  myReg.Free;

  ArcInfoGrid.LoadFromRegistry;
  SurferGrid.LoadFromRegistry;
  ColourScaleOptions.LoadFromRegistry;
  VectorFieldOptions.LoadFromRegistry;
end;

// ----- TGLDataUserSettings.SaveToRegistry ------------------------------------
procedure TGLDataUserSettings.SaveToRegistry;

var
  myReg: TRegistry;

begin
  myReg := TRegistry.Create;
  with myReg do
  begin
    // general options
    OpenKey(CREGISTRYKEY + '\general', true);
    WriteBool('autoprocess', fAutoProcess);
    CloseKey;
    // interface options
    OpenKey(CREGISTRYKEY + '\interface', true);
    WriteBool('invertmousewheel', fInvertMouseWheel);
    WriteBool('focusstatus', fFocusStatus);
    WriteBool('camerastatus', fCameraStatus);
    CloseKey;
    // co-ordinates options
    OpenKey(CREGISTRYKEY + '\coordinates', true);
    WriteFloat('aValue', faValue);
    CloseKey;
    // display options
    OpenKey(CREGISTRYKEY + '\display', true);
    WriteInteger('antialiasing', fAntialiasing);
    WriteString('backgroundcolour', ColorToHex(fBackGroundColour));
    WriteBool('twosidelighting', fTwoSideLighting);
    WriteInteger('shademodel', fShadeModel);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\display\axes', true);
    WriteBool('displayaxes', fDisplayAxes);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\display\camera', true);
    WriteInteger('camerastyle', fCameraStyle);
    WriteFloat('focallength', fCameraFocalLength);
    WriteFloat('depthofview', fCameraDepthOfView);
    WriteFloat('nearplanebias', fCameraNearPlaneBias);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\display\lighting', true);
    WriteBool('enabled', fLighting);
    WriteBool('enabled2', fLighting2);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\display\scale', true);
    WriteFloat('scaleX', fScaleX);
    WriteFloat('scaleY', fScaleY);
    WriteFloat('scaleZ', fScaleZ);
    WriteBool('scalehud', fScaleHUD);
    CloseKey;
    // data options
    OpenKey(CREGISTRYKEY + '\data\markers', true);

    WriteBool('markerboxlineaa', fMarkerBoxLineAA);
    WriteString('markerboxlinecolour', ColorToHex(fMarkerBoxLineColour));
    WriteInteger('markerboxlinepattern', fMarkerBoxLinePattern);
    WriteBool('markerboxlinesmooth', fMarkerBoxLineSmooth);

    WriteBool('displaymarkers', fDisplayMarkers);
    WriteBool('displaypoints', fDisplayPoints);
    WriteFloat('radius', fMarkerRadius);
    WriteString('markercolour', ColorToHex(fMarkerColour));
    WriteBool('wireframe', fMarkerWireFrame);
    WriteBool('autofocus', fAutoFocusPoints);
    WriteBool('rendernullpoints', fRenderNullPoints);
    WriteString('pointcolournull', ColorToHex(fPointColourNull.AsWinColor));
    WriteString('pointcoloursingle', ColorToHex(ColPalette.SingleColour));
    WriteString('pointcolourmin', ColorToHex(ColPalette.MinColour));
    WriteString('pointcolourmax', ColorToHex(ColPalette.MaxColour));
    WriteInteger('pointspectrummode', ColPalette.SpectrumMode);
    WriteInteger('pointstyle', fPointStyle);
    WriteString('clr', ColPalette.SpectrumFile);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\data\lines', true);
    WriteBool('displaylines', fDisplayLines);
    WriteString('linecolour', ColorToHex(fLineColour));
    WriteInteger('linemode', fLineMode);
    CloseKey;
    OpenKey(CREGISTRYKEY + '\data\pipe', true);
    WriteBool('displaypipe', fDisplayPipe);
    WriteFloat('radius', fPipeRadius);
    CloseKey;
    // grid options
    OpenKey(CREGISTRYKEY + '\grids', true);
    WriteBool('autofocus', fAutoFocusGrids);
    CloseKey;
    // geothermal options
    OpenKey(CREGISTRYKEY + '\geothermal', true);
    WriteString('pointcoloursingle', ColorToHex(geocolpalette.SingleColour));
    WriteString('pointcolourmin', ColorToHex(geocolpalette.MinColour));
    WriteString('pointcolourmax', ColorToHex(geocolpalette.MaxColour));
    WriteInteger('pointspectrummode', geocolpalette.SpectrumMode);
    WriteString('clr', geocolpalette.SpectrumFile);
    CloseKey;
    Free;
  end;

  ArcInfoGrid.SaveToRegistry;
  SurferGrid.SaveToRegistry;
  ColourScaleOptions.SaveToRegistry;
  VectorFieldOptions.SaveToRegistry;
end;

// 0 = colpalette
// ----- TGLDataUserSettings.UpdatePalette -------------------------------------
procedure TGLDataUserSettings.UpdatePalette(iMode: integer);

begin
  case iMode of
    // for scalar points
    0:
      begin
        ColPalette.Continuous := ColourScaleOptions.Continuous;
        ColPalette.Border := ColourScaleOptions.Border;
        ColPalette.ScaleWithContours := ColourScaleOptions.ShowContourPoints;
        ColPalette.LabelFormat := ColourScaleOptions.Scaleformat;
        ColPalette.CLRLegend := not ColourScaleOptions.ScaleSteps;
        ColPalette.LabelCount := ColourScaleOptions.Points;
        ColPalette.MakePalette;
      end;
    // vector points
    1:
      begin
        VectorFieldOptions.ColPalette.Continuous :=
          ColourScaleOptions.Continuous;
        VectorFieldOptions.ColPalette.Border := ColourScaleOptions.Border;
        VectorFieldOptions.ColPalette.ScaleWithContours :=
          ColourScaleOptions.ShowContourPoints;
        VectorFieldOptions.ColPalette.LabelFormat :=
          ColourScaleOptions.Scaleformat;
        VectorFieldOptions.ColPalette.CLRLegend :=
          not ColourScaleOptions.ScaleSteps;
        VectorFieldOptions.ColPalette.LabelCount := ColourScaleOptions.Points;
        VectorFieldOptions.ColPalette.MakePalette;
      end;
    // surfer grid
    2:
      begin
        SurferGrid.ColPalette.Continuous := ColourScaleOptions.Continuous;
        SurferGrid.ColPalette.Border := ColourScaleOptions.Border;
        SurferGrid.ColPalette.ScaleWithContours :=
          ColourScaleOptions.ShowContourPoints;
        SurferGrid.ColPalette.LabelFormat := ColourScaleOptions.Scaleformat;
        SurferGrid.ColPalette.CLRLegend := not ColourScaleOptions.ScaleSteps;
        SurferGrid.ColPalette.LabelCount := ColourScaleOptions.Points;
        SurferGrid.ColPalette.MakePalette;
      end;
    // arcinfo grid
    3:
      begin
        ArcInfoGrid.ColPalette.Continuous := ColourScaleOptions.Continuous;
        ArcInfoGrid.ColPalette.Border := ColourScaleOptions.Border;
        ArcInfoGrid.ColPalette.ScaleWithContours :=
          ColourScaleOptions.ShowContourPoints;
        ArcInfoGrid.ColPalette.LabelFormat := ColourScaleOptions.Scaleformat;
        ArcInfoGrid.ColPalette.CLRLegend := not ColourScaleOptions.ScaleSteps;
        ArcInfoGrid.ColPalette.LabelCount := ColourScaleOptions.Points;
        ArcInfoGrid.ColPalette.MakePalette;
      end;
    // geothermal
    4:
      begin
        geocolpalette.Continuous := ColourScaleOptions.Continuous;
        geocolpalette.Border := ColourScaleOptions.Border;
        geocolpalette.ScaleWithContours := ColourScaleOptions.ShowContourPoints;
        geocolpalette.LabelFormat := ColourScaleOptions.Scaleformat;
        geocolpalette.CLRLegend := not ColourScaleOptions.ScaleSteps;
        geocolpalette.LabelCount := ColourScaleOptions.Points;
        geocolpalette.MakePalette;
      end;
  end;
end;

// =============================================================================
end.
