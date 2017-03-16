{ -------------------------------------------------------------------------------
  Unit Name: cGLSimulationGrids
  Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)

  Purpose:   visual classes for rendering TOUGH/MULKOM/TETRAD grids with
  GLScene.

  Updates: TETRAD Grid Support
  ------------------------------------------------------------------------------- }
unit cGLSimulationGrids;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Graphics,
   
  GLScene,
  GLobjects,
  GLGeomobjects,
  GLVectorFileObjects,
  GLVectorTypes,
  GLVectorGeometry,
  GLTexture,
  GLGraphics,
  GLExtrusion,
  GLMaterial,
  GLColor,
  // glData
  cSimulationGrids,
  cUtilities,
  cColourSpectrum,
  cIsoSurfaceMC;

type
  TGLGridLayer = class;
  TGLGridLayers = class;
  TGLSimulationGrid = class;

  TGLBlock = class(TGridBlock)
  private
    fBlock: TGLExtrusionSolid;
    fDummyCube: TGLDummyCube;
    fLayer: TGLGridLayer;
    fIsVisible: boolean;
    procedure SetParamColour(iMode: integer);
  protected
    procedure ConstructBlock;
    function GetScaleX: single;
    function GetScaleY: single;
    function GetScaleZ: single;
    function GetShowBlock: boolean;
    procedure SetShowBlock(bValue: boolean);
  public
    constructor Create(aLayer: TGLGridLayer; aDummyCube: TGLDummyCube);
    destructor Destroy; override;
    procedure ColourBlock;
    procedure RenderBlock;
    property Block: TGLExtrusionSolid read fBlock;
    property Layer: TGLGridLayer read fLayer;
    property ScaleX: single read GetScaleX;
    property ScaleY: single read GetScaleY;
    property ScaleZ: single read GetScaleZ;
    property ShowBlock: boolean read GetShowBlock write SetShowBlock;
  end;

  TGLGridLayer = class(TGridLayer)
  private
    fDummyCube: TGLDummyCube;
    fBlockBottom: boolean;
    fBlockTop: boolean;
    fBlockOutside: boolean;
    fVertexPoints: TGLPoints;
    fAlpha: single;

    procedure UpdateBlockSection(part: TExtrusionSolidPart; bValue: boolean);
    function GetMaxProperty(iProp: integer; dMax, dStart: double;
      bMax: boolean): double;
    function GetMinProperty(iProp: integer; dMin, dStart: double;
      bMin: boolean): double;
    function GetMaxPorosity: double;
    function GetMinPorosity: double;
    function GetMaxPermeabilityX: double;
    function GetMinPermeabilityX: double;
    function GetMaxPermeabilityY: double;
    function GetMinPermeabilityY: double;
    function GetMaxPermeabilityZ: double;
    function GetMinPermeabilityZ: double;
    function GetMaxPressure: double;
    function GetMinPressure: double;
    function GetMaxTemperature: double;
    function GetMinTemperature: double;
    function GetMaxSaturation: double;
    function GetMinSaturation: double;
    procedure SetBlockBottom(bValue: boolean);
    procedure SetBlockOutside(bValue: boolean);
    procedure SetBlockTop(bValue: boolean);
    procedure SetAlpha(dAlpha: single);
  public
    constructor Create(aParent: TGLGridLayers);
    destructor Destroy; override;
    procedure AddBlock(sBlockName: string;
      dLocationE, dLocationN: double); override;
    procedure AddTETRADBlock(sBlockName: string;
      dLocationE, dLocationN, dElevation, dTHickness, dX, dY: double;
      iRow, iCol: integer; bActive: boolean); override;

    procedure ClearBlocks; override;
    procedure HideAllBlocks;
    procedure HideBlockByName(sBlock: string);
    procedure RenderAllBlocks;
    procedure RenderVertices;
    procedure ColourAllBlocks;
    procedure ShowAllBlocks;
    procedure ResetAllBlocks; // resets visibility based on constraints
    procedure ShowBlockByName(sBlock: string);
    property MaxPorosity: double read GetMaxPorosity; // move to TGridLayer?
    property MinPorosity: double read GetMinPorosity;
    property MaxPermeabilityX: double read GetMaxPermeabilityX;
    property MinPermeabilityX: double read GetMinPermeabilityX;
    property MaxPermeabilityY: double read GetMaxPermeabilityY;
    property MinPermeabilityY: double read GetMinPermeabilityY;
    property MaxPermeabilityZ: double read GetMaxPermeabilityZ;
    property MinPermeabilityZ: double read GetMinPermeabilityZ;
    property MaxPressure: double read GetMaxPressure;
    property MinPressure: double read GetMinPressure;
    property MaxSaturation: double read GetMaxSaturation;
    property MinSaturation: double read GetMinSaturation;
    property MaxTemperature: double read GetMaxTemperature;
    property MinTemperature: double read GetMinTemperature;

    property BlockBottom: boolean read fBlockBottom write SetBlockBottom;
    property BlockOutside: boolean read fBlockOutside write SetBlockOutside;
    property BlockTop: boolean read fBlockTop write SetBlockTop;
    property Alpha: single read fAlpha write SetAlpha;
    property VertexPoints: TGLPoints read fVertexPoints;
  end;

  TGLGridLayers = class(TGridLayers)
  private
    fGrid: TGLSimulationGrid;
    fColourMode: integer;
    fMaxPaletteValue: double;
    fMinPaletteValue: double;
    fManualLimits: boolean;

    fShowInActiveBlocks: boolean;

    fRowList: TStringlist;
    fColumnList: TStringlist;

    function GetGLLayer(iIndex: integer): TGLGridLayer;

    function GetMinPropertyVis(iProp: integer; dStart: double): double;
    function GetMaxPropertyVis(iProp: integer; dStart: double): double;

    function GetMinPorosityVis: double;
    function GetMaxPorosityVis: double;
    function GetMinPermeabilityXVis: double;
    function GetMaxPermeabilityXVis: double;
    function GetMinPermeabilityyVis: double;
    function GetMaxPermeabilityyVis: double;
    function GetMinPermeabilityzVis: double;
    function GetMaxPermeabilityzVis: double;

    function GetMinPressureVis: double;
    function GetMaxPressureVis: double;
    function GetMinTemperatureVis: double;
    function GetMaxTemperatureVis: double;
    function GetMinSaturationVis: double;
    function GetMaxSaturationVis: double;

    procedure SetColourMode(iMode: integer);
    procedure SetShowInActiveBlocks(bShow: boolean);
    procedure SetMaxPaletteValue(dValue: double);
    procedure SetMinPaletteValue(dValue: double);
    procedure SetManualLimits(bValue: boolean);
  public
    constructor Create(aGrid: TGLSimulationGrid);
    destructor Destroy; override;
    procedure AddLayer(sLayerName, sLayerType: string;
      dElevation, dTHickness: single); override;
    procedure AddTETRADLayer(sLayerName: string);
    procedure ClearLayers; override;
    function GetGLLayerByName(sLayer: string): TGLGridLayer;
    procedure RenderAll;
    procedure RenderVertices;
    procedure DeleteLayer(iIndex: integer); override;
    procedure ResetAllBlocks;

    property MinPorosityVis: double read GetMinPorosityVis;
    property MaxPorosityVis: double read GetMaxPorosityVis;
    property MinPermeabilityXVis: double read GetMinPermeabilityXVis;
    property MaxPermeabilityXVis: double read GetMaxPermeabilityXVis;
    property MinPermeabilityYVis: double read GetMinPermeabilityyVis;
    property MaxPermeabilityYVis: double read GetMaxPermeabilityyVis;
    property MinPermeabilityZVis: double read GetMinPermeabilityzVis;
    property MaxPermeabilityZVis: double read GetMaxPermeabilityzVis;

    property MinPressureVis: double read GetMinPressureVis;
    property MaxPressureVis: double read GetMaxPressureVis;
    property MinTemperatureVis: double read GetMinTemperatureVis;
    property MaxTemperatureVis: double read GetMaxTemperatureVis;
    property MinSaturationVis: double read GetMinSaturationVis;
    property MaxSaturationVis: double read GetMaxSaturationVis;

    property ColourMode: integer read fColourMode write SetColourMode;
    property GLLayer[iIndex: integer]: TGLGridLayer read GetGLLayer;
    property Grid: TGLSimulationGrid read fGrid;

    property ShowInActiveBlocks: boolean read fShowInActiveBlocks
      write SetShowInActiveBlocks;

    property RowList: TStringlist read fRowList write fRowList;
    property ColumnList: TStringlist read fColumnList write fColumnList;

    property MaxPaletteValue: double read fMaxPaletteValue
      write SetMaxPaletteValue;
    property MinPaletteValue: double read fMinPaletteValue
      write SetMinPaletteValue;
    property ManualLimits: boolean read fManualLimits write SetManualLimits;
  end;

  TGLSimulationGrid = class(TSimulationGrid)
  private
    fColpalette: TColourSpectrum;
    fFreeForm: TGLFreeForm;
    fDummyCube: TGLDummyCube;
    fGridLayers: TGLGridLayers;
    fScaleX: single;
    fScaleY: single;
    fScaleZ: single;
    fIsoArray: array of array of array of single;
    fIsoMode: integer;
    fIsoNX: integer;
    fIsoNY: integer;
    fIsoNZ: integer;
    fXMin: double;
    fXMax: double;
    fYMin: double;
    fYMax: double;
    fZMin: double;
    fZMax: double;
  protected
    procedure SetScaleX(dValue: single);
    procedure SetScaleY(dValue: single);
    procedure SetScaleZ(dValue: single);
  public
    constructor CreateWithDummy(aDummyCube: TGLDummyCube);
    destructor Destroy; override;
    function ObtainVal(x, y, z: double): single;

    function ObtainValue(x, y, z: integer; var bNull: boolean): single;
    procedure EvaluateGridBounds;
    {:Generate Isosurface using Matching Cube algorithm}
    procedure GenerateIsoSurfaceMC(isovalue: double);
    procedure SetupIsoArray;

    property ColPalette: TColourSpectrum read fColpalette write fColpalette;
    property DummyCube: TGLDummyCube read fDummyCube;
    property FreeForm: TGLFreeForm read fFreeForm write fFreeForm;
    property GridLayers: TGLGridLayers read fGridLayers;
    property IsoMode: integer read fIsoMode write fIsoMode;
    property IsoNX: integer read fIsoNX write fIsoNX;
    property IsoNY: integer read fIsoNY write fIsoNY;
    property IsoNZ: integer read fIsoNZ write fIsoNZ;
    property ScaleX: single read fScaleX write SetScaleX;
    property ScaleY: single read fScaleY write SetScaleY;
    property ScaleZ: single read fScaleZ write SetScaleZ;

    property XMin: double read fXMin;
    property XMax: double read fXMax;
    property YMin: double read fYMin;
    property YMax: double read fYMax;
    property ZMin: double read fZMin;
    property ZMax: double read fZMax;

  end;

implementation

// =============================================================================
// TGLGridBlock Implementation
// =============================================================================
// ----- TGLGRidBlock.ConstructBlock -------------------------------------------
procedure TGLBlock.ConstructBlock;

var
  gv: TGridVertex;
  i: integer;
  dX, dY: double;

begin
  // top face
  with Block do
  begin
    Position.x := ScaleX * LocationE;
    Position.y := ScaleY * locationN;
    Position.z := ScaleZ * (Elevation - 0.5 * Thickness);

    // z position = top of layer
    Height := ScaleZ * Thickness;
    MinSmoothAngle := 0.0;
    Contours.Clear;

    // LineWidth :=TGLSimulationGrid(self.Layer.Parent.Grid).LineWidth;
    if TETRAD then
    begin
      Name := 'Block_' + BlockName + '_Layer_' + Layer.LayerName;
      dX := StrToFloat(self.Vertices.Strings[0]);
      dY := StrToFloat(self.Vertices.Strings[1]);
      // four nodes - TETRAD blocks are rectangular
      with Contours.Add.Nodes do
      begin
        // bottom left
        AddNode(self.ScaleX * -0.5 * dX, self.ScaleY * -0.5 * dY, 0);
        // bottom right
        AddNode(self.ScaleX * 0.5 * dX, self.ScaleY * -0.5 * dY, 0);
        // top right
        AddNode(self.ScaleX * 0.5 * dX, self.ScaleY * 0.5 * dY, 0);
        // top left
        AddNode(self.ScaleX * -0.5 * dX, self.ScaleY * 0.5 * dY, 0);
      end;
    end
    else
    begin
      Name := 'Block_' + BlockName + '_Layer_' + Layer.LayerName;
      with Contours.Add.Nodes do
      begin
        for i := Vertices.Count - 1 downto 0 do
        begin
          gv := TGLSimulationGrid(self.Layer.Parent.Grid)
            .GetVertexByLabel(self.Vertices.Strings[i]);
          AddNode(self.ScaleX * (gv.LocationE - self.LocationE),
            self.ScaleY * (gv.locationN - self.locationN), 0);
          // relative position?
        end;
      end;
    end;
    Contours.Items[0].SplineMode := lsmLines;
    StructureChanged;
  end;
end;

// ----- TGLGridBlock.GetScaleX ------------------------------------------------
function TGLBlock.GetScaleX: single;

begin
  result := TGLSimulationGrid(self.Layer.Parent.Grid).ScaleX;
end;

// ----- TGLGridBlock.GetScaleY ------------------------------------------------
function TGLBlock.GetScaleY: single;

begin
  result := TGLSimulationGrid(self.Layer.Parent.Grid).ScaleY;
end;

// ----- TGLGridBlock.GetScaleZ ------------------------------------------------
function TGLBlock.GetScaleZ: single;

begin
  result := TGLSimulationGrid(self.Layer.Parent.Grid).ScaleZ;
end;

// ----- TGLGridBlock.GetShowBlock ---------------------------------------------
function TGLBlock.GetShowBlock: boolean;

begin
  result := fIsVisible;
end;

// ----- TGLGridBlock.SetShowBlock ---------------------------------------------
procedure TGLBlock.SetShowBlock(bValue: boolean);

var
  sTemp: boolean;

begin
  sTemp := bValue;
  fIsVisible := bValue;

  if sTemp then
  begin
    if (self.InActive and (not TGLGridLayers(self.Layer.Parent)
      .ShowInActiveBlocks)) then
      sTemp := false;
    if (TGLGridLayers(self.Layer.Parent).RowList.IndexOf(IntToStr(self.Row))
      = -1) then
      sTemp := false;
    if (TGLGridLayers(self.Layer.Parent).ColumnList.IndexOf
      (IntToStr(self.Column)) = -1) then
      sTemp := false;
  end;

  Block.Visible := sTemp;
end;

// ----- TGLGridBlock.Create ---------------------------------------------------
constructor TGLBlock.Create(aLayer: TGLGridLayer; aDummyCube: TGLDummyCube);

begin
  inherited Create(TGridLayer(aLayer));
  fLayer := aLayer;
  fDummyCube := TGLDummyCube(aDummyCube.AddNewChild(TGLDummyCube));

  fBlock := TGLExtrusionSolid(fDummyCube.AddNewChild(TGLExtrusionSolid));
  with fBlock do
  begin
    Visible := false;
    if aLayer.BlockOutside then
      Parts := Parts + [espOutside];
    if aLayer.BlockTop then
      Parts := Parts + [espStopPolygon];
    if aLayer.BlockBottom then
      Parts := Parts + [espStartPolygon];

    if aLayer.Alpha < 1.0 then
      Material.BlendingMode := bmTransparency
    else
      Material.BlendingMode := bmOpaque;
    Material.MaterialOptions := Material.MaterialOptions + [moNoLighting];
  end;
end;

// ----- TGLGridBlock.Destroy --------------------------------------------------
destructor TGLBlock.Destroy;

begin
  fBlock.Free;
  fDummyCube.Free;
  inherited Destroy;
end;

// ----- TGLGridBlock.SetParamColour -------------------------------------------
procedure TGLBlock.SetParamColour(iMode: integer);

var
  cv: TColorVector;
  dVal: double;

begin
  case iMode of
    1:
      dVal := Porosity;
    2:
      dVal := PermeabilityX;
    3:
      dVal := PermeabilityY;
    4:
      dVal := PermeabilityZ;
    // need to add thermal condutivity, specific heat...
    5:
      dVal := Pr;
    6:
      dVal := Te;
    7:
      dVal := Sv;
  else
    dVal := Porosity;
  end;

  with TGLGridLayers(Layer.Parent).Grid.colpalette do
  begin
    begin
      case SpectrumMode of
        0:
          cv := GetColourVector(dVal, true);
        1:
          cv := GetColourVector(dVal, true);
        2, 3:
          cv := GetColourVector(dVal, (SpectrumMode = 2));
          // rainbow and inverse rainbow spectrum
        4, 5:
          cv := GetColourVector(dVal, (SpectrumMode = 4)); // using a palette
      end;
    end;
    with Block.Material do
    begin
      FrontProperties.Ambient.Color := cv;
      FrontProperties.Diffuse.Color := cv;
      FrontProperties.Emission.Color := cv;
      FrontProperties.Diffuse.Alpha := Layer.Alpha;
    end;
  end;
end;

// ----- TGLGridBlock.ColourBlock ----------------------------------------------
procedure TGLBlock.ColourBlock;

var
  rt: TRockType;
  col: TColor;

begin
  if self.InActive then
  begin
    with Block.Material do
    begin
      FrontProperties.Ambient.AsWinColor := clBlack;
      FrontProperties.Diffuse.AsWinColor := clBlack;
      FrontProperties.Emission.AsWinColor := clBlack;
      FrontProperties.Diffuse.Alpha := Layer.Alpha;
    end;
  end
  else
  begin

    case TGLGridLayers(Layer.Parent).ColourMode of
      // rock-type
      0:
        begin
          rt := self.Layer.Parent.Grid.GetRockTypeByName(RockTypeCode);
          if rt <> nil then
            col := HexToColor(rt.ColourCode)
          else
            col := clBlack; // should be a property

          with Block.Material do
          begin
            FrontProperties.Ambient.AsWinColor := col;
            FrontProperties.Diffuse.AsWinColor := col;
            FrontProperties.Emission.AsWinColor := col;
            FrontProperties.Diffuse.Alpha := Layer.Alpha;
          end;
        end;
      1, 2, 3, 4, 5, 6, 7:
        SetParamColour(TGLGridLayers(Layer.Parent).ColourMode);
    end;
  end;
end;

// ----- TGLGridBlock.RenderBlock ----------------------------------------------
procedure TGLBlock.RenderBlock;

begin
  ConstructBlock;
  ColourBlock;
end;

// =============================================================================
// TGLGridLayer Implementation
// =============================================================================
// ----- TGLGridLayer.HideAllBlocks --------------------------------------------
procedure TGLGridLayer.HideAllBlocks;

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
    TGLBlock(BlockList.Objects[i]).ShowBlock := false;
end;

// ----- TGLGridLayer.HideBlockByName ------------------------------------------
procedure TGLGridLayer.HideBlockByName(sBlock: string);

begin
  if (BlockList.IndexOf(sBlock) <> -1) then
    TGLBlock(BlockList.Objects[BlockList.IndexOf(sBlock)])
      .ShowBlock := false;
end;

// ----- TGLGridLayer.ShowAllBlocks --------------------------------------------
procedure TGLGridLayer.ShowAllBlocks;

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
    with TGLBlock(BlockList.Objects[i]) do
      ShowBlock := true;
end;

// ----- TGLGridLayer.ResetAllBlocks -------------------------------------------
procedure TGLGridLayer.ResetAllBlocks;

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
    with TGLBlock(BlockList.Objects[i]) do
      ShowBlock := ShowBlock;
end;

// ----- TGLGridLayer.ColourAllBlocks ------------------------------------------
procedure TGLGridLayer.ColourAllBlocks;

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
    TGLBlock(BlockList.Objects[i]).ColourBlock;
end;

// ----- TGLGridLayer.ShowBlockByName ------------------------------------------
procedure TGLGridLayer.ShowBlockByName(sBlock: string);

begin
  if (BlockList.IndexOf(sBlock) <> -1) then
    TGLBlock(BlockList.Objects[BlockList.IndexOf(sBlock)])
      .ShowBlock := true;
end;

// ----- TGLGridLayer.Create ---------------------------------------------------
constructor TGLGridLayer.Create(aParent: TGLGridLayers);

begin
  inherited Create(aParent);
  // dummy cube for this layer
  fDummyCube := TGLDummyCube(TGLSimulationGrid(Parent.Grid)
    .DummyCube.AddNewChild(TGLDummyCube));

  // add glpoints
  fVertexPoints := TGLPoints(fDummyCube.AddNewChild(TGLPoints));
  fBlockTop := true;
  fBlockOutside := true;
  fBlockBottom := true;
  fAlpha := 1.0;
end;

// ----- TGLGridLayer.Destroy --------------------------------------------------
destructor TGLGridLayer.Destroy;

begin
  fVertexPoints.Free;
  fDummyCube.Free;
  inherited Destroy;
end;

// ----- TGLGridLayer.AddBlock -------------------------------------------------
procedure TGLGridLayer.AddBlock(sBlockName: string;
  dLocationE, dLocationN: double);

var
  iIndex: integer;
  Block: TGLBlock;

begin
  // check to see if this block is already in the list...
  if (BlockList.IndexOf(sBlockName) = -1) then
  begin
    Block := TGLBlock.Create(self, fDummyCube);
    BlockList.AddObject(sBlockName, Block);
    iIndex := BlockList.IndexOf(sBlockName);
    with TGLBlock(BlockList.Objects[iIndex]) do
    begin
      BlockName := sBlockName;
      LocationE := dLocationE;
      locationN := dLocationN;
    end;
  end;
end;

procedure TGLGridLayer.AddTETRADBlock(sBlockName: string;
  dLocationE, dLocationN, dElevation, dTHickness, dX, dY: double;
  iRow, iCol: integer; bActive: boolean);

var
  iIndex: integer;
  Block: TGLBlock;

begin
  // check to see if this block is already in the list...
  if (BlockList.IndexOf(sBlockName) = -1) then
  begin
    Block := TGLBlock.Create(self, fDummyCube);
    BlockList.AddObject(sBlockName, Block);
    iIndex := BlockList.IndexOf(sBlockName);
    with TGLBlock(BlockList.Objects[iIndex]) do
    begin
      TETRAD := true;
      BlockName := sBlockName;
      LocationE := dLocationE;
      locationN := dLocationN;
      Elevation := dElevation;
      Thickness := dTHickness;
      Row := iRow;
      Column := iCol;
      Vertices.Add(FloatToStr(dX));
      Vertices.Add(FloatToStr(dY));
      InActive := not bActive;
    end;
  end;
end;

// ----- TGLGridLayer.ClearBlocks ----------------------------------------------
procedure TGLGridLayer.ClearBlocks;

begin
  while (BlockList.Count > 0) do
  begin
    TGLBlock(BlockList.Objects[0]).Free;
    BlockList.Delete(0);
  end;
  BlockList.Clear;
end;

// ----- TGLGridLayer.RenderVertices -------------------------------------------
procedure TGLGridLayer.RenderVertices;

var
  Grid: TGLSimulationGrid;
  i: integer;
  x, y: single;

begin
  Grid := TGLSimulationGrid(Parent.Grid);
  VertexPoints.Positions.Clear;
  VertexPoints.Colors.Add(clrBlack);

  for i := 0 to Grid.VertexCount - 1 do
  begin
    x := TGridVertex(Grid.Vertex[i]).LocationE;
    y := TGridVertex(Grid.Vertex[i]).locationN;
    // scale vertices here?
    VertexPoints.Positions.Add(x, y, Elevation);
  end;
end;

// ----- TGLGridLayer.RenderAllBlocks ------------------------------------------
procedure TGLGridLayer.RenderAllBlocks;

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
    TGLBlock(BlockList.Objects[i]).RenderBlock;
end;

// ----- TGLGridLayer.GetMaxProperty -------------------------------------------
function TGLGridLayer.GetMaxProperty(iProp: integer; dMax, dStart: double;
  bMax: boolean): double;

var
  i: integer;
  dValue: double;

begin
  result := dStart; // start value - min
  if (BlockList.Count > 0) then
  begin
    for i := 0 to BlockList.Count - 1 do
    begin
      with TGLBlock(BlockList.Objects[i]) do
      begin
        case iProp of
          0:
            dValue := Porosity;
          1:
            dValue := PermeabilityX;
          2:
            dValue := PermeabilityY;
          3:
            dValue := PermeabilityZ;
          4:
            dValue := Pr;
          5:
            dValue := Te;
          6:
            dValue := Sv;
        else
          dValue := Porosity;
        end;
        if (Block.Visible) and (dValue > result) then
          if bMax then
          begin
            if (dValue <= dMax) then
              result := dValue;
          end
          else
            result := dValue;
      end;
    end;
  end;
end;

// ----- TGLGridLayer.GetMinProperty -------------------------------------------
function TGLGridLayer.GetMinProperty(iProp: integer; dMin, dStart: double;
  bMin: boolean): double;

var
  i: integer;
  dValue: double;

begin
  result := dStart; // start value - min
  if (BlockList.Count > 0) then
  begin
    for i := 0 to BlockList.Count - 1 do
    begin
      with TGLBlock(BlockList.Objects[i]) do
      begin
        case iProp of
          0:
            dValue := Porosity;
          1:
            dValue := PermeabilityX;
          2:
            dValue := PermeabilityY;
          3:
            dValue := PermeabilityZ;
          4:
            dValue := Pr;
          5:
            dValue := Te;
          6:
            dValue := Sv;
        else
          dValue := Porosity;
        end;
        if (Block.Visible) and (dValue < result) then
          if bMin then
          begin
            if (dValue >= dMin) then
              result := dValue;
          end
          else
            result := dValue;
      end;
    end;
  end;
end;

// ----- TGridLayer.GetMaxPorosity ---------------------------------------------
function TGLGridLayer.GetMaxPorosity: double;

begin
  result := GetMaxProperty(0, 1.0, 0.0, true);
end;

// ----- TGridLayer.GetMinPorosity ---------------------------------------------
function TGLGridLayer.GetMinPorosity: double;

begin
  result := GetMinProperty(0, 0.0, 1.0, true);
end;

// ----- TGLGridLayer.GetMaxPermeabilityX --------------------------------------
function TGLGridLayer.GetMaxPermeabilityX: double;

begin
  result := GetMaxProperty(1, 0.0, -1.0, false);
end;

// ----- TGLGridLayer.GetMinPermeabilityX --------------------------------------
function TGLGridLayer.GetMinPermeabilityX: double;

begin
  result := GetMinProperty(1, 0.0, 1.0, false);
end;

// ----- TGLGridLayer.GetMaxPermeabilityy --------------------------------------
function TGLGridLayer.GetMaxPermeabilityY: double;

begin
  result := GetMaxProperty(2, 0.0, -1.0, false);
end;

// ----- TGLGridLayer.GetMinPermeabilityX --------------------------------------
function TGLGridLayer.GetMinPermeabilityY: double;

begin
  result := GetMinProperty(2, 0.0, 1.0, false);
end;

// ----- TGLGridLayer.GetMaxPermeabilityz --------------------------------------
function TGLGridLayer.GetMaxPermeabilityZ: double;

begin
  result := GetMaxProperty(3, 0.0, -1.0, false);
end;

// ----- TGLGridLayer.GetMinPermeabilityz --------------------------------------
function TGLGridLayer.GetMinPermeabilityZ: double;

begin
  result := GetMinProperty(3, 0.0, 1.0, false);
end;

// ----- TGLGridLayer.GetMaxPressure -------------------------------------------
function TGLGridLayer.GetMaxPressure: double;

begin
  result := GetMaxProperty(4, 0.0, 0.0, false);
end;

// ----- TGLGridLayer.GetMinPressure -------------------------------------------
function TGLGridLayer.GetMinPressure: double;

begin
  result := GetMinProperty(4, 0.0, 1E20, false);
end;

// ----- TGLGridLayer.GetMaxTemperature ----------------------------------------
function TGLGridLayer.GetMaxTemperature: double;

begin
  result := GetMaxProperty(5, 0.0, 0.0, false);
end;

// ----- TGLGridLayer.GetMinTemperature ----------------------------------------
function TGLGridLayer.GetMinTemperature: double;

begin
  result := GetMinProperty(5, 0.0, 1E5, false);
end;

// ----- TGLGridLayer.GetMaxSaturation -----------------------------------------
function TGLGridLayer.GetMaxSaturation: double;

begin
  result := GetMaxProperty(6, 1.0, 0.0, true);
end;

// ----- TGLGridLayer.GetMinSaturation -----------------------------------------
function TGLGridLayer.GetMinSaturation: double;

begin
  result := GetMinProperty(6, 0.0, 1.0, false);
end;

// ----- TGLGridLayer.UpdateBlockSection ---------------------------------------
procedure TGLGridLayer.UpdateBlockSection(part: TExtrusionSolidPart;
  bValue: boolean);

var
  i: integer;

begin
  for i := 0 to BlockList.Count - 1 do
  begin
    with TGLBlock(BlockList.Objects[i]) do
    begin
      if bValue then
        Block.Parts := Block.Parts + [part]
      else
        Block.Parts := Block.Parts - [part];
    end;
  end;
end;

// ----- TGLGridLayer.SetBlockBottom -------------------------------------------
procedure TGLGridLayer.SetBlockBottom(bValue: boolean);

begin
  if (fBlockBottom <> bValue) then
  begin
    fBlockBottom := bValue;
    UpdateBlockSection(espStartPolygon, bValue);
  end;
end;

// ----- TGLGridLayer.SetBlockOutside ------------------------------------------
procedure TGLGridLayer.SetBlockOutside(bValue: boolean);

begin
  if (fBlockOutside <> bValue) then
  begin
    fBlockOutside := bValue;
    UpdateBlockSection(espOutside, bValue);
  end;
end;

// ----- TGLGridLayer.SetBlockTop ----------------------------------------------
procedure TGLGridLayer.SetBlockTop(bValue: boolean);

begin
  if (fBlockTop <> bValue) then
  begin
    fBlockTop := bValue;
    UpdateBlockSection(espStopPolygon, bValue);
  end;
end;

// ----- TGLGridLayer.SetAlpha -------------------------------------------------
procedure TGLGridLayer.SetAlpha(dAlpha: single);

var
  i: integer;

begin
  if (dAlpha <> fAlpha) then
  begin
    fAlpha := dAlpha;
    for i := 0 to BlockList.Count - 1 do
    begin
      with TGLBlock(BlockList.Objects[i]) do
      begin
        if dAlpha >= 1.0 then
        begin
          Block.Material.BlendingMode := bmOpaque;
          Block.Material.FrontProperties.Diffuse.Alpha := 1.0;
        end
        else
        begin
          Block.Material.BlendingMode := bmTransparency;
          Block.Material.FrontProperties.Diffuse.Alpha := dAlpha;
        end;
      end;
    end;
  end;
end;

// =============================================================================
// TGLGridLayers Implementation
// =============================================================================
// ----- TGLGridLayers.SetColourMode -------------------------------------------
procedure TGLGridLayers.SetColourMode(iMode: integer);

var
  i: integer;

begin
  // if iMode <> fColourMode then
  // begin
  fColourMode := iMode;
  if ManualLimits then
  begin
    Grid.colpalette.MinValue := MinPaletteValue;
    Grid.colpalette.MaxValue := MaxPaletteValue;
  end
  else
  begin
    case iMode of
      1:
        begin
          Grid.colpalette.MinValue := MinPorosityVis;
          Grid.colpalette.MaxValue := MaxPorosityVis;
        end;
      2:
        begin
          Grid.colpalette.MinValue := MinPermeabilityXVis;
          Grid.colpalette.MaxValue := MaxPermeabilityXVis;
        end;
      3:
        begin
          Grid.colpalette.MinValue := MinPermeabilityYVis;
          Grid.colpalette.MaxValue := MaxPermeabilityYVis;
        end;
      4:
        begin
          Grid.colpalette.MinValue := MinPermeabilityZVis;
          Grid.colpalette.MaxValue := MaxPermeabilityZVis;
        end;
      5:
        begin
          Grid.colpalette.MinValue := MinPressureVis;
          Grid.colpalette.MaxValue := MaxPressureVis;
        end;
      6:
        begin
          Grid.colpalette.MinValue := MinTemperatureVis;
          Grid.colpalette.MaxValue := MaxTemperatureVis;
        end;
      7:
        begin
          Grid.colpalette.MinValue := MinSaturationVis;
          Grid.colpalette.MaxValue := MaxSaturationVis;
        end;
    end;
  end;

  for i := 0 to LayerList.Count - 1 do
    TGLGridLayer(LayerList.Objects[i]).ColourAllBlocks;
  // end;
end;

// ----- TGLGridLayers.SetShowActiveBlocks -------------------------------------
procedure TGLGridLayers.SetShowInActiveBlocks(bShow: boolean);

var
  i: integer;

begin
  if (bShow <> fShowInActiveBlocks) then
  begin
    fShowInActiveBlocks := bShow;
    for i := 0 to LayerList.Count - 1 do
      TGLGridLayer(LayerList.Objects[i]).ResetAllBlocks;
  end;
end;

procedure TGLGridLayers.ResetAllBlocks;

var
  i: integer;

begin
  for i := 0 to LayerList.Count - 1 do
    TGLGridLayer(LayerList.Objects[i]).ResetAllBlocks;
end;

// ----- TGLGridLayers.SetMaxPaletteValue --------------------------------------
procedure TGLGridLayers.SetMaxPaletteValue(dValue: double);

begin
  if (dValue <> fMaxPaletteValue) then
  begin
    fMaxPaletteValue := dValue;
    SetColourMode(fColourMode); // force redraw
  end;
end;

// ----- TGLGridLayers.SetMinPaletteValue --------------------------------------
procedure TGLGridLayers.SetMinPaletteValue(dValue: double);

begin
  if (dValue <> fMinPaletteValue) then
  begin
    fMinPaletteValue := dValue;
    SetColourMode(fColourMode); // force redraw
  end;
end;

// ----- TGLGridLayers.SetManualLimits -----------------------------------------
procedure TGLGridLayers.SetManualLimits(bValue: boolean);

begin
  if (bValue <> fManualLimits) then
  begin
    fManualLimits := bValue;
    SetColourMode(fColourMode); // force redraw
  end;
end;

// ----- TGLGridLayers.RenderAll -----------------------------------------------
procedure TGLGridLayers.RenderAll;

var
  i: integer;

begin
  for i := 0 to LayerList.Count - 1 do
    TGLGridLayer(LayerList.Objects[i]).RenderAllBlocks;
end;

// ----- TGLGridLayers.GetGLLayer ----------------------------------------------
function TGLGridLayers.GetGLLayer(iIndex: integer): TGLGridLayer;

begin
  if (iIndex >= 0) and (iIndex < LayerList.Count) then
    result := TGLGridLayer(LayerList.Objects[iIndex])
  else
    result := nil;
end;

// ----- TGLGridLayers.GetMaxPropertyVis ---------------------------------------
function TGLGridLayers.GetMaxPropertyVis(iProp: integer;
  dStart: double): double;

var
  i: integer;
  dValue: double;

begin
  result := dStart;
  for i := 0 to LayerList.Count - 1 do
  begin
    with TGLGridLayer(LayerList.Objects[i]) do
    begin
      case iProp of
        0:
          dValue := MaxPorosity;
        1:
          dValue := MaxPermeabilityX;
        2:
          dValue := MaxPermeabilityY;
        3:
          dValue := MaxPermeabilityZ;
        4:
          dValue := MaxPressure;
        5:
          dValue := MaxTemperature;
        6:
          dValue := MaxSaturation;
      else
        dValue := MaxPorosity;
      end;
      if (dValue > result) then
        result := dValue;
    end;
  end;
end;

// ----- TGLGridLayers.GetMinPropertyVis ---------------------------------------
function TGLGridLayers.GetMinPropertyVis(iProp: integer;
  dStart: double): double;

var
  i: integer;
  dValue: double;

begin
  result := dStart;
  for i := 0 to LayerList.Count - 1 do
  begin
    with TGLGridLayer(LayerList.Objects[i]) do
    begin
      case iProp of
        0:
          dValue := MinPorosity;
        1:
          dValue := MinPermeabilityX;
        2:
          dValue := MinPermeabilityY;
        3:
          dValue := MinPermeabilityZ;
        4:
          dValue := MinPressure;
        5:
          dValue := MinTemperature;
        6:
          dValue := MinSaturation;
      else
        dValue := MinPorosity;
      end;
      if (dValue < result) then
        result := dValue;
    end;
  end;
end;

// ----- TGLGridLayers.GetMaxPorosityVis ---------------------------------------
function TGLGridLayers.GetMaxPorosityVis: double;

begin
  result := GetMaxPropertyVis(0, 0.0);
end;

// ----- TGLGridLayers.GetMinPorosityVis ---------------------------------------
function TGLGridLayers.GetMinPorosityVis: double;

begin
  result := GetMinPropertyVis(0, 1.0);
end;

// ----- TGLGridLayers.GetMaxPermeabilityXVis ----------------------------------
function TGLGridLayers.GetMaxPermeabilityXVis: double;

begin
  result := GetMaxPropertyVis(1, -1.0);
end;

// ----- TGLGridLayers.GetMinPermeabilityXVis ----------------------------------
function TGLGridLayers.GetMinPermeabilityXVis: double;

begin
  result := GetMinPropertyVis(1, 1.0);
end;

// ----- TGLGridLayers.GetMaxPermeabilityyVis ----------------------------------
function TGLGridLayers.GetMaxPermeabilityyVis: double;

begin
  result := GetMaxPropertyVis(2, -1.0);
end;

// ----- TGLGridLayers.GetMinPermeabilityyVis ----------------------------------
function TGLGridLayers.GetMinPermeabilityyVis: double;

begin
  result := GetMinPropertyVis(2, 1.0);
end;

// ----- TGLGridLayers.GetMaxPermeabilityzVis ----------------------------------
function TGLGridLayers.GetMaxPermeabilityzVis: double;

begin
  result := GetMaxPropertyVis(3, -1.0);
end;

// ----- TGLGridLayers.GetMinPermeabilityzVis ----------------------------------
function TGLGridLayers.GetMinPermeabilityzVis: double;

begin
  result := GetMinPropertyVis(3, 1.0);
end;

// ----- TGLGridLayers.GetMinPressureVis ---------------------------------------
function TGLGridLayers.GetMinPressureVis: double;

begin
  result := GetMinPropertyVis(4, 1E10);
end;

// ----- TGLGridLayers.GetMaxPressureVis ---------------------------------------
function TGLGridLayers.GetMaxPressureVis: double;

begin
  result := GetMaxPropertyVis(4, 0.0);
end;

// ----- TGLGridLayers.GetMinTemperatureVis ------------------------------------
function TGLGridLayers.GetMinTemperatureVis: double;

begin
  result := GetMinPropertyVis(5, 1E5);
end;

// ----- TGLGridLayers.GetMaxTemperatureVis ------------------------------------
function TGLGridLayers.GetMaxTemperatureVis: double;

begin
  result := GetMaxPropertyVis(5, 0.0);
end;

// ----- TGLGridLayers.GetMinSaturationVis -------------------------------------
function TGLGridLayers.GetMinSaturationVis: double;

begin
  result := GetMinPropertyVis(6, 1.0);
end;

// ----- TGLGridLayers.GetMaxSaturationVis -------------------------------------
function TGLGridLayers.GetMaxSaturationVis: double;

begin
  result := GetMaxPropertyVis(6, 0.0);
end;

// ----- TGLGridLayers.Create --------------------------------------------------
constructor TGLGridLayers.Create(aGrid: TGLSimulationGrid);

begin
  inherited Create(aGrid); // passed in as a TSimulationGrid?
  fGrid := aGrid;
  fColourMode := 0;
  fManualLimits := true;
  fMaxPaletteValue := 1.0;
  fMinPaletteValue := 0.0;
  fShowInActiveBlocks := false;
  fRowList := TStringlist.Create;
  fColumnList := TStringlist.Create;
end;

// ----- TGLGridLayers.Destroy -------------------------------------------------
destructor TGLGridLayers.Destroy;

begin
  fRowList.Free;
  fColumnList.Free;
  inherited Destroy;
end;

// ----- TGLGRidLayers.AddLayer ------------------------------------------------
procedure TGLGridLayers.AddLayer(sLayerName, sLayerType: string;
  dElevation, dTHickness: single);

var
  iIndex: integer;
  Layer: TGLGridLayer;

begin
  // check to see if this layer is already in the list...
  if (LayerList.IndexOf(sLayerName) = -1) then
  begin
    Layer := TGLGridLayer.Create(self);
    LayerList.AddObject(sLayerName, Layer);
    iIndex := LayerList.IndexOf(sLayerName);
    with TGLGridLayer(LayerList.Objects[iIndex]) do
    begin
      LayerName := sLayerName;
      LayerType := sLayerType;
      Elevation := dElevation;
      Thickness := dTHickness;
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TGLGridLayers.AddTETRADLayer(sLayerName: string);

var
  iIndex: integer;
  Layer: TGLGridLayer;

begin
  // check to see if this layer is already in the list...
  if (LayerList.IndexOf(sLayerName) = -1) then
  begin
    Layer := TGLGridLayer.Create(self);
    LayerList.AddObject(sLayerName, Layer);
    iIndex := LayerList.IndexOf(sLayerName);
    with TGLGridLayer(LayerList.Objects[iIndex]) do
    begin
      LayerName := sLayerName;
    end;
  end;
end;

// ----- TGLGridLayers.ClearLayers ---------------------------------------------
procedure TGLGridLayers.ClearLayers;

begin
  while (LayerList.Count > 0) do
  begin
    TGLGridLayer(LayerList.Objects[0]).Free;
    LayerList.Delete(0);
  end;
  LayerList.Clear;
end;

// ----- TGLGridLayers.GetGLLayerByName ----------------------------------------
function TGLGridLayers.GetGLLayerByName(sLayer: string): TGLGridLayer;

begin
  result := GetGLLayer(LayerList.IndexOf(sLayer));
end;

// ----- TGLGridLayers.DeleteLayer ---------------------------------------------
procedure TGLGridLayers.DeleteLayer(iIndex: integer);

begin
  if (iIndex >= 0) and (iIndex < LayerList.Count) then
  begin
    TGLGridLayer(LayerList.Objects[iIndex]).Free;
    LayerList.Delete(iIndex);
  end;
end;

// ----- TGLGridLayers.RenderVertices ------------------------------------------
procedure TGLGridLayers.RenderVertices;

var
  i: integer;

begin
  for i := 0 to LayerList.Count - 1 do
    TGLGridLayer(LayerList.Objects[i]).RenderVertices;
end;

// =============================================================================
// TGLSimulationGrid Implementation
// =============================================================================
// ----- TGLSimulationGrid.SetScaleX -------------------------------------------
procedure TGLSimulationGrid.SetScaleX(dValue: single);

begin
  fScaleX := dValue; // propogate to visual objects or maybe not
end;

// ----- TGLSimulationGrid.SetScaleY -------------------------------------------
procedure TGLSimulationGrid.SetScaleY(dValue: single);

begin
  fScaleY := dValue; // propogate to visual objects or maybe not
end;

// ----- TGLSimulationGrid.SetScaleZ -------------------------------------------
procedure TGLSimulationGrid.SetScaleZ(dValue: single);
begin
  fScaleZ := dValue; // propogate to visual objects or maybe not
end;

// ----- TGLSimulationGRid.CreateWithDummy -------------------------------------
constructor TGLSimulationGrid.CreateWithDummy(aDummyCube: TGLDummyCube);

var
  mo: TMeshObject;

begin
  inherited Create;
  fGridLayers := TGLGridLayers.Create(self);
  fDummyCube := aDummyCube;

  fFreeForm := TGLFreeForm(fDummyCube.AddNewChild(TGLFreeForm));
  fFreeForm.Material.FaceCulling := fcNoCull;
  mo := TMeshObject.CreateOwned(fFreeForm.MeshObjects);

  fScaleX := 1.0;
  fScaleY := 1.0;
  fScaleZ := 1.0;

  fIsoNX := 30;
  fIsoNY := 30;
  fIsoNZ := 30;
  fIsoMode := 5;
end;

// ----- TGLSimulationGrid.Destroy ---------------------------------------------
destructor TGLSimulationGrid.Destroy;

begin
  fGridLayers.Free;
  fFreeForm.MeshObjects.Clear;
  fFreeForm.Free;
  inherited Destroy;
end;

// ----- TGLSimulationGrid.SetupIsoArray ---------------------------------------
procedure TGLSimulationGrid.SetupIsoArray;

var
  iRow, iCol: integer;
  i, j, k: integer;
  locE, locN, dex, dey, delev, dthick: double;
  x, y, z, xdelta, ydelta, zdelta: double;

begin

  SetLength(fIsoArray, fIsoNX + 1, fIsoNY + 1, fIsoNZ + 1);

  xdelta := (XMax - XMin) / fIsoNX;
  ydelta := (YMax - YMin) / fIsoNY;
  zdelta := (ZMax - ZMin) / fIsoNZ;

  z := ZMin;
  for k := 0 to fIsoNZ do
  begin
    x := XMin;
    For i := 0 To fIsoNX Do
    Begin
      y := YMin;
      For j := 0 To fIsoNY Do
      Begin
        fIsoArray[i, j, k] := ObtainVal(x, y, z);
        y := y + ydelta;
      End;
      x := x + xdelta;
    End;
    z := z + zdelta;
  End;
end;

// ----- TGLSimulationGrid.ObtainVal -----------------------------------------
function TGLSimulationGrid.ObtainValue(x, y, z: integer;
  var bNull: boolean): single;

begin
  result := fIsoArray[x, y, z];
  bNull := (fIsoArray[x, y, z] = -1);
end;

// ----- TGLSimulationGrid.ObtainVal -----------------------------------------
// obtains for TETRAD
function TGLSimulationGrid.ObtainVal(x, y, z: double): single;

var
  iRow, iCol: integer;
  i, j: integer;
  locE, locN, dex, dey, delev, dthick: double;

begin
  iRow := -1; // not found
  iCol := -1; // not found
  result := -1;

  if (GridLayers.LayerList.Count = 0) then
    exit;

  with TGLGridLayer(GridLayers.LayerList.Objects[0]) do
  begin
    for i := 0 to BlockList.Count - 1 do
    begin
      with TGLBlock(BlockList.Objects[i]) do
      begin
        locE := LocationE;
        locN := locationN;
        dex := StrToFloat(Vertices[0]);
        dey := StrToFloat(Vertices[1]);
        if (not InActive) and
          (((x >= locE - 0.5 * dex) and (x <= locE + 0.5 * dex)) and
          ((y >= locN - 0.5 * dey) and (y <= locN + 0.5 * dey))) then
        begin
          iRow := Row;
          iCol := Column;
          break;
        end;
      end;
    end;
  end;

  if (iRow = -1) or (iCol = -1) then
    exit;

  for i := 0 to GridLayers.LayerList.Count - 1 do
  begin
    with TGLGridLayer(GridLayers.LayerList.Objects[i]) do
    begin
      for j := 0 to BlockList.Count - 1 do
      begin

        if (iRow = TGLBlock(BlockList.Objects[j]).Row) and
          (iCol = TGLBlock(BlockList.Objects[j]).Column) then
        begin
          delev := TGLBlock(BlockList.Objects[j]).Elevation;
          dthick := TGLBlock(BlockList.Objects[j]).Thickness;

          if ((z >= delev - 0.5 * dthick) and (z <= delev + 0.5 * dthick)) then
          begin
            case IsoMode of
              5:
                result := TGLBlock(BlockList.Objects[j]).Te;
              6:
                result := TGLBlock(BlockList.Objects[j]).Pr;
              7:
                result := TGLBlock(BlockList.Objects[j]).Sv;
            end;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

// ----- TGLSimulationGrid.EvaluateGridBounds ----------------------------------
procedure TGLSimulationGrid.EvaluateGridBounds;

var
  i: integer;
  xl, xr, yl, yr, zl, zr: double;

begin
  // grab one layer (they are the same)
  fXMin := 1.7E+308;
  fXMax := 5.0E-324;
  fYMin := 1.7E+308;
  fYMax := 5.0E-324;
  fZMin := 1.7E+308;
  fZMax := 5.0E-324;

  if GridLayers.LayerList.Count > 0 then
  begin
    with TGLGridLayer(GridLayers.LayerList.Objects[0]) do
    begin
      for i := 0 to BlockList.Count - 1 do
      begin
        // xbounds:
        xl := TGLBlock(BlockList.Objects[i]).LocationE -
          StrToFloat(TGLBlock(BlockList.Objects[i]).Vertices[0]);
        xr := TGLBlock(BlockList.Objects[i]).LocationE +
          StrToFloat(TGLBlock(BlockList.Objects[i]).Vertices[0]);
        if (xl < fXMin) then
          fXMin := xl;
        if (xr > fXMax) then
          fXMax := xr;
        // ybounds:
        yl := TGLBlock(BlockList.Objects[i]).locationN -
          StrToFloat(TGLBlock(BlockList.Objects[i]).Vertices[1]);
        yr := TGLBlock(BlockList.Objects[i]).locationN +
          StrToFloat(TGLBlock(BlockList.Objects[i]).Vertices[1]);
        if (yl < fYMin) then
          fYMin := yl;
        if (yr > fYMax) then
          fYMax := yr;
        // zbounds:
        zl := TGLBlock(BlockList.Objects[i]).Elevation - 0.5 *
          TGLBlock(BlockList.Objects[i]).Thickness;
        zr := TGLBlock(BlockList.Objects[i]).Elevation + 0.5 *
          TGLBlock(BlockList.Objects[i]).Thickness;
        if (zl < fZMin) then
          fZMin := zl;
        if (zr > fZMax) then
          fZMax := zr;
      end;
    end;
  end;
end;

// ----- TGLSimulationGrid.GenerateIsoSurfaceMC --------------------------------
procedure TGLSimulationGrid.GenerateIsoSurfaceMC(isovalue: double);

type
  TDataPlane = array of array of single;
  PDataPlane = ^TDataPlane;

  TBlankPlane = array of array of boolean;
  PBlankPlane = ^TBlankPlane;

const
  SMALL = 1E-8;

var
  xdelta, ydelta, zdelta: double;

  PlaneAData, PlaneBData: TDataPlane;
  PlaneA, PlaneB: PDataPlane;

  PlaneABlank, PlaneBBlank: TBlankPlane;
  BlankA, BLankB: PBlankPlane;

  i, j, k, m: integer;
  x, y, z: double;
  Cubeidx: Word;
  CV0, CV1, CV2, CV3, CV4, CV5, CV6, CV7: TVector3f; // Cube vertex coordinates
  VV0, VV1, VV2, VV3, VV4, VV5, VV6, VV7: double; // Values at Cube Vertices
  bV0, bV1, bV2, bV3, bV4, bV5, bV6, bv7: boolean; // null at vertices;
  Verts: Array [0 .. 11] of TVector3f;
  blanks: array [0 .. 11] of boolean;
  Vertex1, Vertex2, Vertex3: TVector3f;

  function Interpolate(Const CVA, CVB: TVector3f; Const VVA, VVB: double)
    : TVector3f;

  Var
    Mu: double;

  Begin
    If (Abs(isovalue - VVA) < SMALL) Then
    Begin
      result := CVA;
      exit;
    End;
    If (Abs(isovalue - VVB) < SMALL) Then
    Begin
      result := CVB;
      exit;
    End;
    If (Abs(VVA - VVB) < SMALL) Then
    Begin
      result.X := 0.5 * (CVA.V[0] + CVB.V[0]);
      result.Y := 0.5 * (CVA.V[1] + CVB.V[1]);
      result.Z := 0.5 * (CVA.V[2] + CVB.V[2]);
      exit;
    End;

    Mu := (isovalue - VVA) / (VVB - VVA);
    result.X := CVA.V[0] + (CVB.V[0] - CVA.V[0]) * Mu;
    result.Y := CVA.V[1] + (CVB.V[1] - CVA.V[1]) * Mu;
    result.Z := CVA.V[2] + (CVB.V[2] - CVA.V[2]) * Mu;
  End;

// Swap the data planes
  Procedure SwapPlanes;
  Var
    P: PDataPlane;
    PB: PBlankPlane;
  Begin
    P := PlaneA;
    PlaneA := PlaneB;
    PlaneB := P;

    PB := BlankA;
    BlankA := BLankB;
    BLankB := PB;
  End;

// Fill PlaneB with new data
  Procedure EvaluatePlane(Const z: double; const n: integer);
  Var
    i, j: integer;
    x, y: single;
    bNull: boolean;
  Begin
    x := XMin;
    For i := 0 To fIsoNX Do
    Begin
      y := YMin;
      For j := 0 To fIsoNY Do
      Begin
        PlaneB^[i, j] := ObtainValue(i, j, n, bNull);
        BLankB^[i, j] := bNull;
        y := y + ydelta;
      End;
      x := x + xdelta;
    End;
  End;

begin
  xdelta := (XMax - XMin) / fIsoNX;
  ydelta := (YMax - YMin) / fIsoNY;
  zdelta := (ZMax - ZMin) / fIsoNZ;

  // Allocate planes (for min storage pick smallest plane)
  SetLength(PlaneAData, fIsoNX + 1, fIsoNY + 1);
  SetLength(PlaneBData, fIsoNX + 1, fIsoNY + 1);
  PlaneA := @PlaneAData;
  PlaneB := @PlaneBData;

  SetLength(PlaneABlank, fIsoNX + 1, fIsoNY + 1);
  SetLength(PlaneBBlank, fIsoNX + 1, fIsoNY + 1);
  BlankA := @PlaneABlank;
  BLankB := @PlaneBBlank;

  FreeForm.MeshObjects[0].Vertices.Clear;
  FreeForm.MeshObjects[0].Mode := momTriangles;

  // build
  z := ZMin;
  EvaluatePlane(z, 0);

  for k := 0 to fIsoNZ - 1 do
  begin
    SwapPlanes;
    EvaluatePlane(z + zdelta, k + 1);
    y := YMin;
    for j := 0 To fIsoNY - 1 Do
    begin
      x := XMin;
      for i := 0 To fIsoNX - 1 Do
      begin

        // Values at cube vertices
        VV0 := PlaneA^[i, j];
        bV0 := BlankA^[i, j];

        VV1 := PlaneA^[i + 1, j];
        bV1 := BlankA^[i + 1, j];

        VV2 := PlaneB^[i + 1, j];
        bV2 := BLankB^[i + 1, j];

        VV3 := PlaneB^[i, j];
        bV3 := BLankB^[i, j];

        VV4 := PlaneA^[i, j + 1];
        bV4 := BlankA^[i, j + 1];

        VV5 := PlaneA^[i + 1, j + 1];
        bV5 := BlankA^[i + 1, j + 1];

        VV6 := PlaneB^[i + 1, j + 1];
        bV6 := BLankB^[i + 1, j + 1];

        VV7 := PlaneB^[i, j + 1];
        bv7 := BLankB^[i, j + 1];

        // Evaluate cube index
        Cubeidx := 0;
        If (VV0 < isovalue) Then
          Cubeidx := Cubeidx Or 1;
        If (VV1 < isovalue) Then
          Cubeidx := Cubeidx Or 2;
        If (VV2 < isovalue) Then
          Cubeidx := Cubeidx Or 4;
        If (VV3 < isovalue) Then
          Cubeidx := Cubeidx Or 8;
        If (VV4 < isovalue) Then
          Cubeidx := Cubeidx Or 16;
        If (VV5 < isovalue) Then
          Cubeidx := Cubeidx Or 32;
        If (VV6 < isovalue) Then
          Cubeidx := Cubeidx Or 64;
        If (VV7 < isovalue) Then
          Cubeidx := Cubeidx Or 128;

        // the edge table tells us which vertices are inside/outside the surface
        If (EdgeTable[Cubeidx] <> 0) Then
        Begin

          SetVector(CV0, x, y, z);
          SetVector(CV1, x + xdelta, y, z);
          SetVector(CV2, x + xdelta, y, z + zdelta);
          SetVector(CV3, x, y, z + zdelta);
          SetVector(CV4, x, y + ydelta, z);
          SetVector(CV5, x + xdelta, y + ydelta, z);
          SetVector(CV6, x + xdelta, y + ydelta, z + zdelta);
          SetVector(CV7, x, y + ydelta, z + zdelta);

          // find the vertices where the surface intersects the cube, using interpolate
          If (EdgeTable[Cubeidx] And 1) <> 0 then
          begin
            Verts[0] := Interpolate(CV0, CV1, VV0, VV1);
            blanks[0] := bV0 or bV1;
          end;
          If (EdgeTable[Cubeidx] And 2) <> 0 Then
          begin
            Verts[1] := Interpolate(CV1, CV2, VV1, VV2);
            blanks[1] := bV1 or bV2;
          end;
          If (EdgeTable[Cubeidx] And 4) <> 0 Then
          begin
            Verts[2] := Interpolate(CV2, CV3, VV2, VV3);
            blanks[2] := bV2 or bV3;
          end;
          If (EdgeTable[Cubeidx] And 8) <> 0 Then
          begin
            Verts[3] := Interpolate(CV3, CV0, VV3, VV0);
            blanks[3] := bV3 or bV0;
          end;

          If (EdgeTable[Cubeidx] And 16) <> 0 Then
          begin
            Verts[4] := Interpolate(CV4, CV5, VV4, VV5);
            blanks[4] := bV4 or bV5;
          end;

          If (EdgeTable[Cubeidx] And 32) <> 0 Then
          begin
            Verts[5] := Interpolate(CV5, CV6, VV5, VV6);
            blanks[5] := bV5 or bV6;
          end;
          If (EdgeTable[Cubeidx] And 64) <> 0 Then
          begin
            Verts[6] := Interpolate(CV6, CV7, VV6, VV7);
            blanks[6] := bV6 or bv7;
          end;
          If (EdgeTable[Cubeidx] And 128) <> 0 Then
          begin
            Verts[7] := Interpolate(CV7, CV4, VV7, VV4);
            blanks[7] := bv7 or bV4;
          end;
          If (EdgeTable[Cubeidx] And 256) <> 0 Then
          begin
            Verts[8] := Interpolate(CV0, CV4, VV0, VV4);
            blanks[8] := bV0 or bV4;
          end;
          If (EdgeTable[Cubeidx] And 512) <> 0 Then
          begin
            Verts[9] := Interpolate(CV1, CV5, VV1, VV5);
            blanks[9] := bV1 or bV5;
          end;
          If (EdgeTable[Cubeidx] And 1024) <> 0 Then
          begin
            Verts[10] := Interpolate(CV2, CV6, VV2, VV6);
            blanks[10] := bV2 or bV6;
          end;
          If (EdgeTable[Cubeidx] And 2048) <> 0 Then
          begin
            Verts[11] := Interpolate(CV3, CV7, VV3, VV7);
            blanks[11] := bV3 or bv7;
          end;

          m := 0;
          While TriTable[Cubeidx, m] <> -1 do
          Begin
            Vertex1 := Verts[TriTable[Cubeidx][m]];
            Vertex2 := Verts[TriTable[Cubeidx][m + 1]];
            Vertex3 := Verts[TriTable[Cubeidx][m + 2]];

            if not(blanks[TriTable[Cubeidx][m]] or
              blanks[TriTable[Cubeidx][m + 1]] or
              blanks[TriTable[Cubeidx][m + 2]]) then
              FreeForm.MeshObjects[0].Vertices.Add(Vertex1, Vertex2, Vertex3);
            m := m + 3;
          End;
        End; // if edgetable..
        x := x + xdelta;
      End;
      y := y + ydelta;
    End;
    z := z + zdelta;
  End;
end;

// =============================================================================
end.
