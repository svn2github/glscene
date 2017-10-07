{
  @abstract(reads a surfer .clr file and provides rendering tools on a TBitMap
  level. Adapted by source code provided by Phil Scadden.)
  @author(Aaron Hochwimer <aaron@graphic-edge.co.nz>)
  @created(June 27, 2003)
  @lastmod(July 9, 2003)
  $Id: cColourSpectrum.pas,v 1.11 2004/07/08 09:54:53 hochwimmera Exp $
}
unit cColourSpectrum;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
   
  GLVectorGeometry,
  GLvectorTypes,
  GLTexture,
  GLColor;

type
  TRGBArray = array [0 .. 32767] of TRGBTriple;
  pRGBArray = ^TRGBArray;

  TColourSpectrumEntry = packed record
    colour: TRGBTriple;
    percentvalue: double;
  end;

  pColourSpectrumEntry = ^TColourSpectrumEntry;

  TriVertex = packed record
    x, y: DWORD;
    Red, Green, Blue, Alpha: Word;
  end;

  TColourSpectrum = class(TObject)
  private
    fBorder: boolean;
    fCLRLegend: boolean;
    fContinuous: boolean;
    fEntries: TList;
    fGLCol_Bottom: TGLColor;
    fGLCol_Top: TGLColor;
    fInvertScale: boolean;
    fInterval: double;
    fLabelCount: integer;
    fLabelFormat: string;
    fMaxColour: TColor;
    fMaxValue: double;
    fMinColour: TColor;
    fMinValue: double;
    fPixelsPerPercent: integer;
    fScalePalette: TBitmap;
    fScaleWithContours: boolean;
    fSingleColour: TColor;
    fSpectrumFile: string;
    fSpectrumMode: integer;
    procedure Add(dPercent: double; Red, Green, Blue: byte);
    procedure ClearEntries;
    procedure DrawBorder;
    function GetCount: integer;
    function GetEntry(iIndex: integer): TColourSpectrumEntry;
    procedure GFillRect(const canvas: TCanvas;
      start_colour, end_colour: TRGBTriple;
      rLeft, rTop, rRight, rBottom: integer);
    function MakeRGB(r, g, b: byte): TRGBTriple;
    procedure WriteLabel(dValue: double; iLeft, iTop: integer);
    procedure GenerateLegend(iLeft, iTop: integer);
  protected
    procedure SetCLRLegend(bLegend: boolean);
    procedure SetEntry(Index: integer; value: TColourSpectrumEntry);
    procedure SetSpectrumFile(sFile: string);
    procedure SetSpectrumMode(iMode: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetColours(const dValue: double; const bForward: boolean;
      var col1, col2: TColor; var dRatio: double);
    function GetColourVector(const dValue: double; const bForward: boolean)
      : TColorVector;
    procedure MakePalette;
    procedure ReadCLRFile;

    property PixelsPerPercent: integer read fPixelsPerPercent
      write fPixelsPerPercent;
    property LabelFormat: string read fLabelFormat write fLabelFormat;
    property Border: boolean read fBorder write fBorder;
    property Continuous: boolean read fContinuous write fContinuous;
    property Count: integer read GetCount;
    property Entries: TList read fEntries write fEntries;
    property ScaleWithContours: boolean read fScaleWithContours
      write fScaleWithContours;
    property glCol_Bottom: TGLColor read fGLCol_Bottom write fGLCol_Bottom;
    property glCol_Top: TGLColor read fGLCol_Top write fGLCol_Top;
    property Interval: double read fInterval write fInterval;
    property SingleColour: TColor read fSingleColour write fSingleColour;
    property MinColour: TColor read fMinColour write fMinColour;
    property MaxColour: TColor read fMaxColour write fMaxColour;
    property SpectrumMode: integer read fSpectrumMode write SetSpectrumMode;
    property SpectrumFile: string read fSpectrumFile write SetSpectrumFile;
    property MinValue: double read fMinValue write fMinValue;
    property MaxValue: double read fMaxValue write fMaxValue;
    property Scalepalette: TBitmap read fScalePalette write fScalePalette;
    property InvertScale: boolean read fInvertScale write fInvertScale;

    property CLRLegend: boolean read fCLRLegend write SetCLRLegend;
    property LabelCount: integer read fLabelCount write fLabelCount;

    property Entry[iIndex: integer]: TColourSpectrumEntry read GetEntry
      write SetEntry;
  end;

  // Access a Windows API. May be possbile to use G32?
function GradientFill(DC: hDC; pVertex: Pointer; dwNumVertex: DWORD;
  pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWORD; stdcall;
  external 'msimg32.dll';

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
implementation

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------

procedure TColourSpectrum.Add(dPercent: double; Red, Green, Blue: byte);

var
  Entry: pColourSpectrumEntry;

begin
  New(Entry);
  with Entry^ do
  begin
    colour := MakeRGB(Red, Green, Blue);
    percentvalue := dPercent;
  end;
  Entries.Add(Entry);
end;

procedure TColourSpectrum.ClearEntries;
var
  i: integer;

begin
  for i := Entries.Count - 1 downto 0 do
    Dispose(pColourSpectrumEntry(Entries.Items[i]));
  Entries.Clear;
end;

procedure TColourSpectrum.DrawBorder;

begin
  if Assigned(Scalepalette) then
  begin
    with Scalepalette.canvas do
    begin
      Pen.Color := clBlack;
      Pen.Mode := pmcopy;
      Brush.Color := clWhite;
      Rectangle(0, 1, Scalepalette.Width - 1, Scalepalette.Height - 1);
    end;
  end;
end;

function TColourSpectrum.GetCount: integer;

begin
  result := Entries.Count;
end;

function TColourSpectrum.GetEntry(iIndex: integer): TColourSpectrumEntry;

begin
  result := pColourSpectrumEntry(Entries.Items[iIndex])^;
end;

procedure TColourSpectrum.GFillRect(const canvas: TCanvas;
  start_colour, end_colour: TRGBTriple; rLeft, rTop, rRight, rBottom: integer);

var
  vert: array [0 .. 1] of TriVertex;
  gRect: GRADIENT_RECT;

begin
  with vert[0] do
  begin
    x := rLeft;
    y := rTop;
    Red := start_colour.rgbtred shl 8;
    Green := start_colour.rgbtgreen shl 8;
    Blue := start_colour.rgbtblue shl 8;
    Alpha := $0000;
  end;

  with vert[1] do
  begin
    x := rRight;
    y := rBottom;
    Red := end_colour.rgbtred shl 8;
    Green := end_colour.rgbtgreen shl 8;
    Blue := end_colour.rgbtblue shl 8;
    Alpha := $0000;
  end;
  gRect.UpperLeft := 0;
  gRect.LowerRight := 1;
  GradientFill(canvas.Handle, @vert[0], 2, @gRect, 1, GRADIENT_FILL_RECT_V);
end;

function TColourSpectrum.MakeRGB(r, g, b: byte): TRGBTriple;

begin
  result.rgbtblue := b;
  result.rgbtgreen := g;
  result.rgbtred := r;
end;

// ----- TColourSpectrum.SetCLRLegend ------------------------------------------
procedure TColourSpectrum.SetCLRLegend(bLegend: boolean);

begin
  fCLRLegend := bLegend;
end;

procedure TColourSpectrum.SetEntry(Index: integer; value: TColourSpectrumEntry);

begin
  with pColourSpectrumEntry(Entries.Items[Index])^ do
  begin
    percentvalue := value.percentvalue;
    colour := value.colour;
  end;
end;

procedure TColourSpectrum.SetSpectrumFile(sFile: string);

begin
  fSpectrumFile := sFile;
end;

procedure TColourSpectrum.SetSpectrumMode(iMode: integer);

var
  bR, bG, bB: byte;

begin
  fSpectrumMode := iMode;
  case iMode of
    // single colour
    0:
      begin
        InvertScale := false;
        ClearEntries;
        bR := GetRValue(SingleColour);
        bG := GetGValue(SingleColour);
        bB := GetBValue(SingleColour);
        Add(0.0, bR, bG, bB);
        Add(100.0, bR, bG, bB);
      end;
    // min/max colour
    1:
      begin
        InvertScale := false;
        ClearEntries;
        Add(0.0, GetRValue(MinColour), GetGValue(MinColour),
          GetBValue(MinColour));
        Add(100.0, GetRValue(MaxColour), GetGValue(MaxColour),
          GetBValue(MaxColour));
      end;
    // rainbow and inverse rainbow
    2, 3:
      begin
        InvertScale := (iMode = 3);
        ClearEntries;
        Add(0.0, 0, 0, 255);
        Add(25.0, 0, 255, 255);
        Add(50.0, 0, 255, 0);
        Add(75.0, 255, 255, 0);
        Add(100.0, 255, 0, 0);
      end;
    // palette
    4:
      begin
        InvertScale := false;
        ReadCLRFile;
      end;
    // inverse palette
    5:
      begin
        InvertScale := true;
        ReadCLRFile;
      end;
  end;
end;


constructor TColourSpectrum.Create;

begin
  inherited Create;

  fEntries := TList.Create;
  fGLCol_Bottom := TGLColor.Create(nil);
  fGLCol_Top := TGLColor.Create(nil);

  fSingleColour := clGreen;
  fMinColour := clBlue;
  fMaxColour := clRed;

  Scalepalette := TBitmap.Create;
  Scalepalette.PixelFormat := pf24bit;

  fBorder := true;
  fContinuous := true;
  fScaleWithContours := true;

  fLabelFormat := '%5.1g';
  fPixelsPerPercent := 4;

  fInvertScale := false;

  fCLRLegend := false; // set from code
  fLabelCount := 2; // number of labels >=2

  SpectrumMode := 2; // safe
end;

destructor TColourSpectrum.Destroy;

begin
  fEntries.Free;
  fGLCol_Bottom.Free;
  fGLCol_Top.Free;

  if Assigned(Scalepalette) then
    Scalepalette.Free;

  inherited Destroy;
end;

procedure TColourSpectrum.GetColours(const dValue: double;
  const bForward: boolean; var col1, col2: TColor; var dRatio: double);

var
  i, ibot, iTop: integer;
  dBot, dPercentage: double;

begin
  Interval := MaxValue - MinValue;

  // clamp bottom colour
  if (dValue <= MinValue) then
  begin
    if bForward then
      iTop := 0
    else
      iTop := GetCount - 1;
    col1 := RGB(pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtred,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtgreen,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtblue);
    col2 := col1;
    // clamp top colour
  end
  else if (dValue >= MaxValue) then
  begin
    if bForward then
      iTop := GetCount - 1
    else
      iTop := 0;
    col1 := RGB(pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtred,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtgreen,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtblue);
    col2 := col1;
  end
  else
  begin
    dPercentage := 100.0 * (dValue - MinValue) / Interval;
    if (not bForward) then
      dPercentage := 100.0 - dPercentage;

    // miss the 100.0 entry (from count-2)
    for i := Entries.Count - 2 downto 0 do
    begin
      if (pColourSpectrumEntry(Entries.Items[i])^.percentvalue < dPercentage)
      then
      begin
        ibot := i;
        iTop := i + 1;
        break;
      end;
    end;
    col1 := RGB(pColourSpectrumEntry(Entries.Items[ibot])^.colour.rgbtred,
      pColourSpectrumEntry(Entries.Items[ibot])^.colour.rgbtgreen,
      pColourSpectrumEntry(Entries.Items[ibot])^.colour.rgbtblue);
    col2 := RGB(pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtred,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtgreen,
      pColourSpectrumEntry(Entries.Items[iTop])^.colour.rgbtblue);

    dBot := pColourSpectrumEntry(Entries.Items[ibot])^.percentvalue;
    dRatio := (dPercentage - dBot) / (pColourSpectrumEntry(Entries.Items[iTop])
      ^.percentvalue - dBot);
  end;
end;

function TColourSpectrum.GetColourVector(const dValue: double;
  const bForward: boolean): TColorVector;

var
  rstar: double;
  col1, col2: TColor;

begin
  GetColours(dValue, bForward, col1, col2, rstar);
  glCol_Bottom.AsWinColor := col1;
  glCol_Top.AsWinColor := col2;
  VectorLerp(glCol_Bottom.Color, glCol_Top.Color, rstar, result);
end;

procedure TColourSpectrum.WriteLabel(dValue: double; iLeft, iTop: integer);

var
  sLegendString: string;
  csize: TSize;

begin
  Scalepalette.canvas.Brush.Color := clWhite;
  sLegendString := Format(LabelFormat, [dValue]);
  csize := Scalepalette.canvas.TextExtent(sLegendString);
  Scalepalette.canvas.TextOut(iLeft, iTop - (csize.cy div 2), sLegendString)
end;

procedure TColourSpectrum.GenerateLegend(iLeft, iTop: integer);

var
  i, iIndex, iHeight, n, iPos: integer;
  percent_bot, percent_top, val_top, val_bot: double;

begin
  if not Assigned(Scalepalette) then
    exit;

  iPos := iTop;

  if InvertScale then
    iIndex := 0
  else
    iIndex := n - 1;

  if CLRLegend then
  begin
    n := Count;
    with pColourSpectrumEntry(Entries.Items[iIndex])^ do
    begin
      percent_top := percentvalue;
    end;
  end
  else
  begin
    n := LabelCount; // minimum of 2
    if InvertScale then
      percent_top := 0
    else
      percent_top := 100;
  end;


  // writes the first label
  // WriteLabel(val_top,iLeft,iPos);

  if InvertScale then
  begin
    for i := 0 to n - 1 do
    begin
      if CLRLegend then
      begin
        with pColourSpectrumEntry(Entries.Items[i])^ do
        begin
          percent_bot := percentvalue;
          iHeight := -Round(PixelsPerPercent * (percent_top - percent_bot));
          val_bot := MaxValue - 0.01 * percentvalue * Interval;
        end;
      end
      else
      begin
        percent_bot := ((n - 1 - i) / (n - 1)) * 100;
        iHeight := -Round(PixelsPerPercent * (percent_top - percent_bot));
        val_bot := MaxValue - 0.01 * percent_bot * Interval;
      end;

      iPos := iPos + iHeight;
      WriteLabel(val_bot, iLeft, iPos);
      percent_top := percent_bot;
    end;
  end
  else
  begin
    for i := n - 1 downto 0 do
    begin
      if CLRLegend then
      begin
        with pColourSpectrumEntry(Entries.Items[i])^ do
        begin
          percent_bot := percentvalue;
          iHeight := Round(PixelsPerPercent * (percent_top - percent_bot));
          val_bot := MinValue + 0.01 * percentvalue * Interval;
        end;
      end
      else
      begin
        percent_bot := i / (n - 1) * 100;
        iHeight := Round(PixelsPerPercent * (percent_top - percent_bot));
        val_bot := MinValue + 0.01 * percent_bot * Interval;
      end;
      iPos := iPos + iHeight;
      WriteLabel(val_bot, iLeft, iPos);

      percent_top := percent_bot;
    end;
  end;
end;

procedure TColourSpectrum.MakePalette;

const
  iLeft = 5;
  iTop = 12;

var
  BoxSize: integer;
  i, iBoxPos, iDepth, iHeight, n: integer;
  percent_bot, percent_top, val_bot, val_top: double;
  colour_bot, colour_top: TRGBTriple;
  r: TRect;
  iIndex: integer;

begin
  n := Count;
  if (n = 0) then
    exit;

  Interval := (MaxValue - MinValue);

  if Assigned(Scalepalette) then
    Scalepalette.Free;

  Scalepalette := TBitmap.Create;
  with Scalepalette do
  begin
    Transparent := true;
    PixelFormat := pf24bit;
  end;
  iBoxPos := 12;
  BoxSize := 20;
  Scalepalette.Width := 60;
  Scalepalette.Height := (100 * PixelsPerPercent) + 25;

  Scalepalette.canvas.Font.Height := 12;
  Scalepalette.canvas.Font.Name := 'Tahoma';

  if Border then
    DrawBorder;

  Scalepalette.canvas.Pen.Color := clBlack;
  Scalepalette.canvas.Pen.Mode := pmcopy;
  iDepth := PixelsPerPercent * 100 + iBoxPos + 1;
  Scalepalette.canvas.Rectangle(iLeft, iTop - 1, BoxSize, iDepth + 1);

  if InvertScale then
    iIndex := 0
  else
    iIndex := n - 1;

  with pColourSpectrumEntry(Entries.Items[iIndex])^ do
  begin
    colour_top := colour;
    percent_top := percentvalue;
    if InvertScale then
      val_top := MaxValue - 0.01 * percent_top * Interval
    else
      val_top := MinValue + 0.01 * percent_top * Interval;
  end;

  // inverted scale.
  if InvertScale then
  begin
    for i := 1 to n - 1 do
    begin
      with pColourSpectrumEntry(Entries.Items[i])^ do
      begin
        colour_bot := colour;
        percent_bot := percentvalue;
        if i < n - 1 then
          iHeight := -Round(PixelsPerPercent * (percent_top - percentvalue))
        else
          iHeight := (iDepth - iBoxPos);

        val_bot := MaxValue - 0.01 * percentvalue * Interval;
      end;
      Scalepalette.canvas.Brush.Color := clNone;
      if Continuous then
      begin

        if ScaleWithContours and (i < n - 1) then
          GFillRect(Scalepalette.canvas, colour_top, colour_bot, iLeft + 1,
            iBoxPos, BoxSize - 1, iBoxPos + iHeight - 1)
        else
          GFillRect(Scalepalette.canvas, colour_top, colour_bot, iLeft + 1,
            iBoxPos, BoxSize - 1, iBoxPos + iHeight);
      end
      else
      begin
        Scalepalette.canvas.Brush.Color :=
          RGB(colour_bot.rgbtred, colour_bot.rgbtgreen, colour_bot.rgbtblue);
        r.Left := iLeft + 1;
        r.Top := iBoxPos;
        r.Right := BoxSize - 1;
        r.Bottom := iBoxPos + iHeight;
        if ScaleWithContours and (i < n - 1) then
          r.Bottom := r.Bottom - 1;
        Scalepalette.canvas.FillRect(r);
      end;
      iBoxPos := iBoxPos + iHeight;
      colour_top := colour_bot;
      percent_top := percent_bot;
    end;
    GenerateLegend(BoxSize + 4, 12);
  end
  else
  begin
    for i := n - 2 downto 0 do
    begin
      with pColourSpectrumEntry(Entries.Items[i])^ do
      begin
        colour_bot := colour;
        percent_bot := percentvalue;
        if i > 0 then
          iHeight := Round(PixelsPerPercent * (percent_top - percentvalue))
        else
          iHeight := iDepth - iBoxPos;

        val_bot := MinValue + 0.01 * percentvalue * Interval;
      end;
      Scalepalette.canvas.Brush.Color := clNone;
      if Continuous then
      begin

        if ScaleWithContours and (i > 0) then
          GFillRect(Scalepalette.canvas, colour_top, colour_bot, iLeft + 1,
            iBoxPos, BoxSize - 1, iBoxPos + iHeight - 1)
        else
          GFillRect(Scalepalette.canvas, colour_top, colour_bot, iLeft + 1,
            iBoxPos, BoxSize - 1, iBoxPos + iHeight);
      end
      else
      begin
        Scalepalette.canvas.Brush.Color :=
          RGB(colour_bot.rgbtred, colour_bot.rgbtgreen, colour_bot.rgbtblue);
        r.Left := iLeft + 1;
        r.Top := iBoxPos;
        r.Right := BoxSize - 1;
        r.Bottom := iBoxPos + iHeight;
        if ScaleWithContours and (i > 0) then
          r.Bottom := r.Bottom - 1;
        Scalepalette.canvas.FillRect(r);
      end;
      iBoxPos := iBoxPos + iHeight;
      colour_top := colour_bot;
      percent_top := percent_bot;
    end;
    GenerateLegend(BoxSize + 4, 12);
  end;
end;

procedure TColourSpectrum.ReadCLRFile;

var
  f: TextFile;
  dPercent: double;
  red1, green1, blue1: byte;
  sHeader: string;

begin
  red1 := GetRValue(SingleColour);
  green1 := GetGValue(SingleColour);
  blue1 := GetBValue(SingleColour);

  if FileExists(SpectrumFile) then
  begin
    AssignFile(f, SpectrumFile);
    Reset(f);
    ReadLn(f, sHeader);
    if (Pos('ColorMap 1 1', sHeader) = 1) then
    begin
      ClearEntries;
      while not eof(f) do
      begin
        ReadLn(f, dPercent, red1, green1, blue1);
        Add(dPercent, red1, green1, blue1);
      end;
    end
    else
    begin
      ClearEntries;
      Add(0.0, red1, green1, blue1);
      Add(100.0, red1, green1, blue1);
    end;
    CloseFile(f);
  end
  else
  begin
    ClearEntries;
    Add(0.0, red1, green1, blue1);
    Add(100.0, red1, green1, blue1);
  end;
end;

// =============================================================================
end.
