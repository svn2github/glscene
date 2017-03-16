{ -------------------------------------------------------------------------------
  Unit Name: cGLgridimportfn
  ------------------------------------------------------------------------------- }
unit cGLGridImportFn;

interface

uses
  System.Classes, System.SysUtils, {GraphicEx,} Vcl.Graphics,
  Vcl.Dialogs, Vcl.ExtDlgs, Vcl.Forms, Vcl.Controls,
   
  GLGraph, GLVectorGeometry, GLObjects, GLTexture, GLVectorTypes,
  GLColor, GLMaterial, GLState,
  // glData
  cGridImportFn, cUsersSettings, frmSurferImport;

type
  TGLContourGridData = class(TContourGridData)
  private
    fAlpha: double;
    fBaseMapPath: string;
    fEnableBaseMap: boolean;
    fDummyCube: TGLDummyCube;
    fExactBlankSub: boolean;
    fGrid: TGLHeightField;
    fGridName: string;
    fGridOptions: TGridOptions;
    fOpenTexture: TOpenPictureDialog;
    fScaleX: double;
    fScaleY: double;
    fScaleZ: double;
    fTileX: integer;
    fTileY: integer;
  protected
    function GetColourMode: integer;
    procedure SetColourMode(iMode: integer);
    function GetPolyGonMode: integer;
    procedure SetPolygonMode(iMode: integer);
    function GetTriangleCount: integer;
    function GetVisible: boolean;
    function GetTwoSided: boolean;
    procedure SetTwoSided(bTwoSided: boolean);
    procedure Render(const x, y: Single; var z: Single; var color: TColorVector;
      var texPoint: TTexPoint);
    procedure SetVisible(bVisible: boolean);
    procedure SetAlpha(dAlpha: double);
    procedure SetScaleX(dScale: double);
    procedure SetScaleY(dScale: double);
    procedure SetScaleZ(dScale: double);
    procedure SetTileX(iTile: integer);
    procedure SetTileY(iTile: integer);
    procedure SetBaseMapPath(sBaseMapPath: string);
    procedure SetEnableBaseMap(bEnableBaseMap: boolean);
  public
    constructor Create(aDummyCube: TGLDummyCube);
    destructor Destroy; override;
    procedure ConstructGrid;
    // 0 = surfer, 1 = arcinfo
    // blank sub is here to allow grids to be (reloaded)....
    procedure Import(sFileName: TFileName; dBlankSub: double; iType: integer;
      var bOK: boolean);
    procedure SilentImport(sFileName: string; dBlankSub: double; iType: integer;
      var bOK: boolean);
    property Alpha: double read fAlpha write SetAlpha;
    property BaseMapPath: string read fBaseMapPath write SetBaseMapPath;
    property EnableBaseMap: boolean read fEnableBaseMap write SetEnableBaseMap;
    property DummyCube: TGLDummyCube read fDummyCube write fDummyCube;
    property ExactBlankSub: boolean read fExactBlankSub write fExactBlankSub;
    property Grid: TGLHeightField read fGrid write fGrid;
    property GridName: string read fGridName write fGridName;
    property GridOptions: TGridOptions read fGridOptions write fGridOptions;
    property OpenTexture: TOpenPictureDialog read fOpenTexture
      write fOpenTexture;
    property ColourMode: integer read GetColourMode write SetColourMode;
    property PolygonMode: integer read GetPolyGonMode write SetPolygonMode;
    property TriangleCount: integer read GetTriangleCount;
    property Visible: boolean read GetVisible write SetVisible;
    property ScaleX: double read fScaleX write SetScaleX;
    property ScaleY: double read fScaleY write SetScaleY;
    property ScaleZ: double read fScaleZ write SetScaleZ;
    property TileX: integer read fTileX write SetTileX;
    property TileY: integer read fTileY write SetTileY;
    property TwoSided: boolean read GetTwoSided write SetTwoSided;
  end;

implementation

// ----- TGLContourGridData.SetBaseMapPath -------------------------------------
procedure TGLContourGridData.SetBaseMapPath(sBaseMapPath: string);

begin
  if (fBaseMapPath <> sBaseMapPath) then
  begin
    fBaseMapPath := sBaseMapPath;
    if FileExists(fBaseMapPath) then
    begin
      Grid.Material.Texture.Image.LoadFromFile(fBaseMapPath);
      Grid.StructureChanged;
    end
    else
      EnableBaseMap := false;
  end;
end;

// ----- TGLContourGridData.SetEnableBaseMap -----------------------------------
procedure TGLContourGridData.SetEnableBaseMap(bEnableBaseMap: boolean);

begin
  fEnableBaseMap := bEnableBaseMap;
  Grid.Material.Texture.Disabled := not fEnableBaseMap;
  if fEnableBaseMap then
    Grid.Options := Grid.Options + [hfoTextureCoordinates]
  else
    Grid.Options := Grid.Options - [hfoTextureCoordinates];
end;

// ----- TGLContourGridData.SetScaleX ------------------------------------------
procedure TGLContourGridData.SetScaleX(dScale: double);

begin
  fScaleX := dScale;
  Grid.Scale.x := dScale;
  Grid.Position.x := xlo * dScale;
end;

// ----- TGLContourGridData.SetScaley ------------------------------------------
procedure TGLContourGridData.SetScaleY(dScale: double);

begin
  fScaleY := dScale;
  Grid.Scale.y := dScale;
  Grid.Position.y := ylo * dScale;
end;

// ----- TGLContourGridData.SetScalez ------------------------------------------
procedure TGLContourGridData.SetScaleZ(dScale: double);

begin
  fScaleZ := dScale;
  Grid.Scale.z := dScale;
  Grid.Position.z := zlo * dScale;
end;

// ----- TGLContourGridData.SetTileX -------------------------------------------
procedure TGLContourGridData.SetTileX(iTile: integer);

begin
  fTileX := iTile;
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.SetTileY -------------------------------------------
procedure TGLContourGridData.SetTileY(iTile: integer);

begin
  fTileY := iTile;
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.GetTwoSided ----------------------------------------
function TGLContourGridData.GetTwoSided: boolean;

begin
  result := (hfoTwoSided in Grid.Options);
end;

// ----- TGLContourGridData.SetTwoSided ----------------------------------------
procedure TGLContourGridData.SetTwoSided(bTwoSided: boolean);

begin
  if bTwoSided then
    Grid.Options := Grid.Options + [hfoTwoSided]
  else
    Grid.Options := Grid.Options - [hfoTwoSided];
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.GetColourMode --------------------------------------
function TGLContourGridData.GetColourMode: integer;

begin
  if (Grid.ColorMode = hfcmAmbient) then
    result := 0
  else if (Grid.ColorMode = hfcmAmbientAndDiffuse) then
    result := 1
  else if (Grid.ColorMode = hfcmDiffuse) then
    result := 2
  else if (Grid.ColorMode = hfcmEmission) then
    result := 3
  else if (Grid.ColorMode = hfcmNone) then
    result := 4
  else
    result := 0;
end;

// ----- TGLContourGridData.SetColourMode --------------------------------------
procedure TGLContourGridData.SetColourMode(iMode: integer);

begin
  case iMode of
    0:
      Grid.ColorMode := hfcmAmbient;
    1:
      Grid.ColorMode := hfcmAmbientAndDiffuse;
    2:
      Grid.ColorMode := hfcmDiffuse;
    3:
      Grid.ColorMode := hfcmEmission;
    4:
      Grid.ColorMode := hfcmNone;
  end;
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.GetPolyGonMode -------------------------------------
function TGLContourGridData.GetPolyGonMode: integer;

begin
  if (Grid.Material.PolygonMode = pmFill) then
    result := 0
  else if (Grid.Material.PolygonMode = pmLines) then
    result := 1
  else
    result := 2;
end;

// ----- TGLContourGridData.SetPolygonMode -------------------------------------
procedure TGLContourGridData.SetPolygonMode(iMode: integer);

begin
  case iMode of
    0:
      Grid.Material.PolygonMode := pmFill;
    1:
      Grid.Material.PolygonMode := pmLines;
    2:
      Grid.Material.PolygonMode := pmPoints;
  end;
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.GetTriangleCount -----------------------------------
function TGLContourGridData.GetTriangleCount: integer;

begin
  result := Grid.TriangleCount;
end;

// ----- TGLContourGridData.GetVisible -----------------------------------------
function TGLContourGridData.GetVisible: boolean;

begin
  result := Grid.Visible;
end;

// ----- TGLContourGridData.SetVisible -----------------------------------------
procedure TGLContourGridData.SetVisible(bVisible: boolean);

begin
  Grid.Visible := bVisible;
end;

// ----- TGLContourGridData.SetAlpha -------------------------------------------
procedure TGLContourGridData.SetAlpha(dAlpha: double);

begin
  fAlpha := dAlpha;
  Grid.StructureChanged;
end;

// ----- TGLContourGridData.Render ---------------------------------------------
procedure TGLContourGridData.Render(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);

const
  SMALL = 1E-4;

var
  val: Single;

begin
  val := Nodes[Round(y / Dy), Round(x / Dx)];
  if ExactBlankSub then
  begin
    if (val = blankval) then
    begin
      z := blanksub - zlo;
      if not Grid.Material.Texture.Disabled then
      begin
        texPoint.S := x * TileX / xrange;
        texPoint.T := y * TileY / yrange;
      end
      else
        color := GridOptions.ColsBlank.color; // blank colour;
    end
    else
    begin
      z := val - zlo;
      if not Grid.Material.Texture.Disabled then
      begin
        texPoint.S := x * TileX / xrange;
        texPoint.T := y * TileY / yrange;
      end
      else
        color := GridOptions.GetSpectrum((val - zlo) / zrange, val);
    end;
  end
  else
  begin
    if (val >= (blankval - SMALL)) then
    begin
      z := blanksub - zlo;
      if not Grid.Material.Texture.Disabled then
      begin
        texPoint.S := x * TileX / xrange;
        texPoint.T := y * TileY / yrange;
      end
      else
        color := GridOptions.ColsBlank.color; // blank colour;
    end
    else
    begin
      z := val - zlo;
      if not Grid.Material.Texture.Disabled then
      begin
        texPoint.S := x * TileX / xrange;
        texPoint.T := y * TileY / yrange;
      end
      else
        color := GridOptions.GetSpectrum((val - zlo) / zrange, val);
    end;
  end;
  color.V[3] := fAlpha;
end;

// ----- TGLContourGridData.Create ---------------------------------------------
constructor TGLContourGridData.Create(aDummyCube: TGLDummyCube);

begin
  inherited Create;
  fAlpha := 1.0;
  fBaseMapPath := '';
  fDummyCube := aDummyCube; // assigned externally
  fExactBlankSub := false;
  fGridName := '';
  fTileX := 1;
  fTileY := 1;

  Grid := TGLHeightField(DummyCube.AddNewChild(TGLHeightField));
  Grid.Material.BlendingMode := bmTransparency;
  OpenTexture := TOpenPictureDialog.Create(nil);
  OpenTexture.Title := 'Select Basemap Texture'
end;

// ----- TGLContourGridData.Destroy --------------------------------------------
destructor TGLContourGridData.Destroy;

begin
  fGrid.free;
  OpenTexture.free;
  inherited Destroy;
end;

// ----- TGLContourGridData.ConstructGrid --------------------------------------
procedure TGLContourGridData.ConstructGrid;

begin
  with Grid do
  begin
    XSamplingScale.Min := 0;
    XSamplingScale.Max := xrange + 0.01 * Dx;
    XSamplingScale.Origin := 0;
    XSamplingScale.Step := Dx;
    Position.x := xlo;

    YSamplingScale.Min := 0.0;
    YSamplingScale.Origin := 0.0;
    YSamplingScale.Max := yrange + 0.01 * Dy;
    YSamplingScale.Step := Dy;
    Position.y := ylo;
    Position.z := zlo;
    OnGetHeight := self.Render;
  end;
end;

// ----- TGLContourGridData.Import ---------------------------------------------
procedure TGLContourGridData.Import(sFileName: TFileName; dBlankSub: double;
  iType: integer; var bOK: boolean);

var
  bLoad: boolean;
  sf: TformSurferImport;

begin
  bOK := false;
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;

  case iType of
    0:
      bLoad := LoadSurferGrid(sFileName) = 0;
    1:
      bLoad := LoadARCINFOASCII(sFileName) = 0;
  else
    bLoad := false;
  end;
  // assign the blank value substitution
  blanksub := dBlankSub;

  // load data if successful
  if bLoad then
  begin
    sf := TformSurferImport.Create(nil);
    with sf do
    begin
      case iType of
        0:
          Caption := 'Surfer Grid Import Settings';
        1:
          Caption := 'ARC/INFO ASCII Grid Import Settings';
      end;
      ebMinX.Text := Format('%g', [xlo]);
      ebMinY.Text := Format('%g', [ylo]);
      ebMinZ.Text := Format('%g', [zlo]);
      ebMaxX.Text := Format('%g', [xhi]);
      ebMaxY.Text := Format('%g', [yhi]);
      ebMaxZ.Text := Format('%g', [zhi]);
      ebSpacingX.Text := Format('%g', [Dx]);
      ebSpacingY.Text := Format('%g', [Dy]);
      ebNoX.Text := IntToStr(Nx);
      ebNoY.Text := IntToStr(Ny);
      ebTotalNo.Text := IntToStr(Nx * Ny);

      if (nblanks > 0) then
      begin
        pnlBlankedNodes.Visible := true;
        ebNoBlanks.Text := IntToStr(nblanks);
        geBlankedValue.Value := blanksub;
      end
      else
      // fix to bug reported by Richard Beitelmair
      begin
        pnlBlankedNodes.Visible := false;
        ebNoBlanks.Text := '0';
        geBlankedValue.Value := blanksub;
      end;
    end;

    Screen.Cursor := crDefault;
    if (sf.ShowModal = mrOK) then
    begin
      Application.ProcessMessages;
      Screen.Cursor := crHourGlass;

      // palette scaling is by data limits - have manual settings at a later stage
      if zlo < GridOptions.colpalette.MinValue then
        GridOptions.colpalette.MinValue := zlo;
      if zhi > GridOptions.colpalette.MaxValue then
        GridOptions.colpalette.MaxValue := zhi;

      if GridOptions.PromptTexture then
      begin
        if OpenTexture.Execute then
        begin
          try
            BaseMapPath := OpenTexture.FileName;
            EnableBaseMap := true;
          except
            EnableBaseMap := false;
          end;
        end
        else
          EnableBaseMap := false;
      end
      else
        EnableBaseMap := false;

      case iType of
        0:
          ExactBlankSub := false;
        1:
          ExactBlankSub := true;
      end;
      blanksub := sf.geBlankedValue.Value;
      GridName := sf.ebGridName.Text;

      ConstructGrid;
      bOK := true;
      Screen.Cursor := crDefault;
    end;
    sf.Release;
  end
  else
  begin
    case iType of
      0:
        MessageDlg('"' + sFileName +
          '" does not seem to be a valid Surfer grid file!', mtError,
          [mbOK], 0);
      1:
        MessageDlg('"' + sFileName +
          '" does not seem to be a valid ArcInfo ASCII grid file!', mtError,
          [mbOK], 0);
    end;
  end;
end;

// ----- TGLContourGridData.SilentImport ---------------------------------------
procedure TGLContourGridData.SilentImport(sFileName: string; dBlankSub: double;
  iType: integer; var bOK: boolean);

var
  bLoad: boolean;

begin
  bOK := false;
  case iType of
    0:
      bLoad := LoadSurferGrid(sFileName) = 0;
    1:
      bLoad := LoadARCINFOASCII(sFileName) = 0;
  else
    bLoad := false;
  end;

  if bLoad then
  begin
    blanksub := dBlankSub;
    // palette scaling is by data limits - have manual settings at a later stage
    if zlo < GridOptions.colpalette.MinValue then
      GridOptions.colpalette.MinValue := zlo;
    if zhi > GridOptions.colpalette.MaxValue then
      GridOptions.colpalette.MaxValue := zhi;

    EnableBaseMap := false; // override after import
    case iType of
      0:
        ExactBlankSub := false;
      1:
        ExactBlankSub := true;
    end;
    ConstructGrid;
    bOK := true;
  end;
end;

// =============================================================================
end.
