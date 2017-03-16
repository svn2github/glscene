{-----------------------------------------------------------------------------
 Unit Name: cSimulationGrids
 Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)

 Purpose:   non-visual classes for storing and manipulating geothermal
 simulation grids (e.g. MULKON/TOUGH, TETRAD)

 Updates:
  28-Jul-2003:  Version 0.1
   - added TGridBlock and other code - note nothing actually works (yet)

-------------------------------------------------------------------------------}
unit cSimulationGrids;

interface

uses
  System.Classes, System.SysUtils, Vcl.Controls;

type
  TSVect = array[0..2] of single;

  TGridLayer = class;
  TGridLayers = class;
  TSimulationGrid = class;

  TGridVertex = class(TObject)
  private
    fLocationE : single;
    fLocationN : single;
    fVertexLabel : string;
  public
    property LocationE:single read fLocationE write fLocationE;
    property LocationN:single read fLocationN write fLocationN;
    property VertexLabel:string read fVertexLabel write fVertexLabel;
  end;

  TRockType = class(TObject)
  private
    fColourCode : string;
    fRockName : string;
    fPermeabilityX : double;
    fPermeabilityY : double;
    fPermeabilityZ : double;
    fPorosity : double;
    fSpecificHeat : double;
    fThermalConductivity : double;
  protected
    procedure SetPorosity(dValue:double);
  public
    property ColourCode : string read fColourCode write fColourCode;
    property RockName : string read fRockName write fRockName;
    property PermeabilityX : double read fPermeabilityX write fPermeabilityX;
    property PermeabilityY : double read fPermeabilityY write fPermeabilityY;
    property PermeabilityZ : double read fPermeabilityZ write fPermeabilityZ;
    property Porosity : double read fPorosity write SetPorosity;
    property SpecificHeat : double read fSpecificHeat write fSpecificHeat;
    property ThermalConductivity : double read fThermalConductivity write
      fThermalConductivity;
  end;

// cartesian xyz grid block
  TGridBlock = class(TObject)
  private
    fInActive : boolean;
    fBlockName : string;
    fColumn : integer;
    fRow : integer;
    fLayer : TGridLayer;
    fLocationE : single;
    fLocationN : single;
    fPr : single;
    fRockTypeCode : string;
    fSv : single;
    fTe : single;
    fVertices : TStringList;
    fTETRAD : boolean;

    fElevation : single;
    fThickness : single;
  protected
    function GetElevation : single;
    function GetLayerName : string;
    function GetLayerType : string;
    function GetPermeabilityX : double;
    function GetPermeabilityY : double;
    function GetPermeabilityZ : double;
    function GetPorosity : single;
    function GetThickness : single;
    procedure SetBlockName(sValue : string);
    procedure SetLocationE(dValue : single);
    procedure SetLocationN(dValue : single);

    procedure SetPr(dValue:single);
    procedure SetSv(dValue:single);
    procedure SetTe(dValue:single);
  public
    constructor Create(aLayer:TGridLayer);
    destructor Destroy;override;
    procedure ClearVertices;
    procedure AddVertex(sVertex:string);
    property InActive : boolean read fInActive write fInActive;
    property BlockName : string read fBlockName write SetBlockName;
    property Column : integer read fColumn write fColumn;
    property Elevation : single read GetElevation write fElevation;
    property Layer : TGridLayer read fLayer write fLayer;
    property LayerName : string read GetLayerName;
    property LayerType : string read GetLayerType;
    property LocationE : single read fLocationE write SetLocationE;
    property LocationN : single read fLocationN write SetLocationN;
    property Pr : single read fPr write SetPr;
    property PermeabilityX : double read GetPermeabilityX;
    property PermeabilityY : double read GetPermeabilityY;
    property PermeabilityZ : double read GetPermeabilityZ;
    property Porosity : single read GetPorosity;
    property RockTypeCode : string read fRockTypeCode write fRockTypeCode;
    property Row : integer read fRow write fRow;
    property Sv : single read fSv write SetSv;
    property Te : single read fTe write SetTe;
    property TETRAD : boolean read fTETRAD write fTETRAD;
    property Thickness : single read GetThickness write fThickness;
    property Vertices : TStringList read fVertices write fVertices;
  end;

  TGridLayer = class(TObject)
  private
    fBlockList : TStringList;
    fElevation : single; // elevation of the layer (midpoint)
    fLayerName : string;
    fLayerType : string;
    fParent : TGridLayers;
    fThickNess : single;
    fVisible : boolean;

  protected
    procedure SetVisible(bVisible:boolean);
    function GetGridBlock(iIndex:integer):TGridBlock;
    function GetMinPr:single;
    function GetMinTe:single;
    function GetMinSv:single;
    function GetMaxPr:single;
    function GetMaxTe:single;
    function GetMaxSv:single;
  public
    constructor Create(aParent:TGridLayers);
    destructor Destroy;override;
    procedure AddBlock(sBlockName:string;dLocationE,dLocationN:double);virtual;
    procedure AddTETRADBlock(sBlockName:string;dLocationE,dLocationN,dElevation,
      dTHickness,dX,dY:double;iRow,iCol:integer;bActive:boolean);virtual;

    procedure AddVertexOrder(sBlockName:string;vo:TStringList);
    procedure ClearBlocks;virtual;
    function GetBlockByName(sBlock:string):TGridBlock;
//  procedure DeleteBlock(iBlock:integer);virtual;
    property BlockList:TStringList read fBlockList;
    property Elevation:single read fElevation write fElevation;
    property LayerName:string read fLayerName write fLayerName;
    property LayerType:string read fLayerType write fLayerType;
    property MinPr : single read GetMinPr;
    property MinSv : single read GetMinSv;
    property MinTe : single read GetMinTe;
    property MaxPr : single read GetMaxPr;
    property MaxSv : single read GetMaxSv;
    property MaxTe : single read GetMaxTe;

    property Parent:TGridLayers read fParent;
    property Thickness:single read fThickNess write fThickness;
    property Block[iIndex:integer]:TGridBLock read GetGridBlock;
    property Visible:boolean read fVisible write SetVisible;
  end;

  TGridLayers = class(TObject)
  private
    fGrid : TSimulationGrid;
    fLayerList : TStringList;
  protected
  public
    constructor Create(aGrid:TSimulationGrid);
    destructor Destroy;override;
    procedure AddLayer(sLayerName,sLayerType:string;
      delevation,dthickness:single);virtual;
    procedure ClearLayers;virtual;
    procedure DeleteLayer(iIndex:integer);virtual;
    property Grid:TSimulationGrid read fGrid;
    property LayerList:TStringList read fLayerList;
  end;

// bounds on vertices...

  TSimulationGrid = class(TObject)
  private
    fComment : string;
    fGridDate : TDate;
    fGridName : string;
    fRockTypeList : TStringlist;
    fVertexList : TStringList;
    function GetRockType(iIndex:integer):TRockType;
    function GetGridVertex(iIndex:integer):TGridVertex;
    function GetVertexCount:integer;
    function GetMinVertexLimit : TSVect;
    function GetMaxVertexLimit : TSVect;
  public
    constructor Create;
    destructor Destroy;override;
    procedure AddVertex(sVertex:string;x,y:single);
    procedure AddRockType(sRock:string;dPermX,dPermY,dPermZ:double;dPorosity,
      dThermalConductivity,dSpecificHeat:single;sColourCode:string);
    procedure ClearAll;
    procedure ClearRockTypes;
    procedure ClearVertices;
    procedure DeleteVertex(iIndex:integer);
    procedure DeleteVertexByLabel(sLabel:string);
    function GetRockTypeByName(sRock:string):TRockType;
    function GetVertexByLabel(sLabel:string):TGridVertex;
    property Comment:string read fComment write fComment;
    property GridDate:TDate read fGridDate write fGridDate;
    property GridName:string read fGridName write fGridName;

    property MinVertexLimit : TSVect read GetMinVertexLimit;
    property MaxVertexLimit : TSVect read GetMaxVertexLimit;

    property RockType[iIndex:integer]:TRockType read GetRockType;
    property Vertex[iIndex:integer]:TGridVertex read GetGridVertex;
    property VertexCount:integer read GetVertexCount;
  end;

implementation
// =============================================================================
// TRockType Implementation
// =============================================================================
procedure TRockType.SetPorosity(dValue:double);

begin
// porosity must be [0.0,1.0]
  if (dValue > 1.0) then
    fPorosity := 1.0
  else if (dValue < 0.0) then
    fPorosity := 0.0
  else
    fPorosity := dValue;
end;
// =============================================================================
// TGRIDBLOCK Implementation
// =============================================================================
// ----- TGridBlock.GetElevation -----------------------------------------------
function TGridBlock.GetElevation:single;

begin
  if TETRAD then
    result := fElevation
  else
    result := TGridLayer(Layer).Elevation;
end;
// ----- TGridBlock.GetLayerName -----------------------------------------------
function TGridBlock.GetLayerName:string;

begin
  result := TGridLayer(Layer).LayerName;
end;
// ----- TGridBlock.GetLayerType -----------------------------------------------
function TGridBlock.GetLayerType:string;

begin
  result := TGridLayer(Layer).LayerType;
end;
// ----- TGridBlock.GetPermeabilityX -------------------------------------------
function TGridBlock.GetPermeabilityX:double;

var
  aRock : TRockType;

begin
  aRock := TGridLayer(fLayer).Parent.Grid.GetRockTypeByName(RockTypeCode);
  if (aRock <> nil) then
    result := aRock.PermeabilityX
  else
    result := 0;
end;
// ----- TGridBlock.GetPermeabilityY -------------------------------------------
function TGridBlock.GetPermeabilityY:double;

var
  aRock : TRockType;

begin
  aRock := TGridLayer(fLayer).Parent.Grid.GetRockTypeByName(RockTypeCode);
  if (aRock <> nil) then
    result := aRock.PermeabilityY
  else
    result := 0;
end;
// ----- TGridBlock.GetPermeabilityZ -------------------------------------------
function TGridBlock.GetPermeabilityZ:double;

var
  aRock : TRockType;

begin
  aRock := TGridLayer(fLayer).Parent.Grid.GetRockTypeByName(RockTypeCode);
  if (aRock <> nil) then
    result := aRock.PermeabilityZ
  else
    result := 0;
end;
// ----- TGridBlock.GetPorosity ------------------------------------------------
function TGridBlock.GetPorosity:single;

var
  aRock : TRockType;

begin
  aRock := TGridLayer(fLayer).Parent.Grid.GetRockTypeByName(RockTypeCode);
  if (aRock <> nil) then
    result := aRock.Porosity
  else
    result := 0.0; // should really be null
end;
// ----- TGridBlock.GetThickness -----------------------------------------------
function TGridBlock.GetThickness:single;

begin
  if TETRAD then
    result := fThickness
  else
    result := TGridLayer(Layer).Thickness;
end;
//------ TGridBlock.SetBlockName -----------------------------------------------
procedure TGridBlock.SetBlockName(sValue:string);

begin
  if (fBlockName <> sValue) then
    fBlockName := sValue;
end;
// ----- TGridBlock.SetLocationE -----------------------------------------------
procedure TGridBlock.SetLocationE(dValue:single);

begin
  if (fLocationE <> dValue) then
    fLocationE := dValue;
end;
// ----- TGridBlock.SetLocationN -----------------------------------------------
procedure TGridBlock.SetLocationN(dValue:single);

begin
  if (fLocationN <> dValue) then
    fLocationN := dValue;
end;
// ----- TGridBlock.SetPr ------------------------------------------------------
procedure TGridBlock.SetPr(dValue:single);

begin
  if (fPr <> dValue) then
    fPr := dValue;
end;
// ----- TGridBlock.SetSv ------------------------------------------------------
procedure TGridBlock.SetSv(dValue:single);

begin
  if (fSv <> dValue) then
  begin
// saturation must be [0.0,1.0]
    if (dValue > 1.0) then
      fSv := 1.0
    else if (dValue < 0.0) then
      fSv := 0.0
    else
      fSv := dValue;
  end;
end;
// ----- TGridBlock.SetTe ------------------------------------------------------
procedure TGridBlock.SetTe(dValue:single);

begin
  if (fTe <> dValue) then
    fTe := dValue;
end;
// ----- TGridBlock.Create -----------------------------------------------------
constructor TGridBlock.Create(aLayer:TGridLayer);

begin
  inherited Create;
  fLayer := aLayer;
  fTETRAD := false;
  fInActive := false;
  Vertices := TStringList.Create;
end;
// ----- TGridBlock.Destroy ----------------------------------------------------
destructor TGridBlock.Destroy;

begin
  ClearVertices;
  Vertices.Free;
  inherited Destroy;
end;
// ----- TGridBlock.AddVertex --------------------------------------------------
procedure TGridBlock.AddVertex(sVertex:string);

begin
  Vertices.Add(sVertex);
end;
// ----- TGridBlock.ClearVertices ----------------------------------------------
procedure TGridBlock.ClearVertices;

begin
  Vertices.Clear;
end;
// =============================================================================
// TGRIDLAYER Implementation
// =============================================================================
procedure TGridLayer.SetVisible(bVisible:boolean);

begin
  fVisible := bVisible;
// set block visibility ...
end;
// ----- TGridLayer.GetMinPr ---------------------------------------------------
// loop all blocks - looking for min Pressure
function TGridLayer.GetMinPr:single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Pr
      else
        if (result > Pr) then
          result := Pr;
    end;
  end;
end;
// ----- TGridLayer.GetMinTe ---------------------------------------------------
function TGridLayer.GetMinTe:single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Te
      else
        if (result > Te) then
          result := Te;
    end;
  end;
end;
// ----- TGridLayer.GetMinSv ---------------------------------------------------
function TGridLayer.GetMinSv:single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Sv
      else
        if (result > Sv) then
          result := Sv;
    end;
  end;
end;
// ----- TGridLayer.GetMaxPr ---------------------------------------------------
function TGridLayer.GetMaxPr:single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Pr
      else
        if (result < Pr) then
          result := Pr;
    end;
  end;
end;
// ----- TGridLayer.GetMaxTe ---------------------------------------------------
function TGridLayer.GetMaxTe: single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Te
      else
        if (result < Te) then
          result := Te;
    end;
  end;
end;
// ----- TGridLayer.GetMaxSv ---------------------------------------------------
function TGridLayer.GetMaxSv:single;

var
  i:integer;

begin
  for i:=0 to fBlockList.Count-1 do
  begin
    with TGridBlock(fBlockList.Objects[i]) do
    begin
      if i=0 then
        result := Sv
      else
        if (result < Sv) then
          result := Sv;
    end;
  end;
end;
// ----- TGridLayer.GetGridBlock -----------------------------------------------
function TGridLayer.GetGridBlock(iIndex:integer):TGridBlock;
begin
  if (iIndex >=0) and (iIndex < fBlockList.Count) then
    result := TGridBlock(fBlockList.Objects[iIndex])
  else
    result := nil;
end;
// ----- TGridLayer.Create -----------------------------------------------------
constructor TGridLayer.Create(aParent:TGridLayers);

begin
  inherited Create;
  fParent := aParent;
  fBlockList :=TStringList.Create;
end;
// ----- TGridLayer.Destroy ----------------------------------------------------
destructor TGridLayer.Destroy;

begin
  ClearBlocks;
  fBlockList.Free;
  inherited Destroy;
end;
// ----- TGridLayer.AddBlock ---------------------------------------------------
procedure TGridLayer.AddBlock(sBlockName:string;dLocationE,dLocationN:double);

var
  iIndex : integer;
  block : TGridBlock;

begin
// check to see if this block is already in the list...
  if (fBlockList.IndexOf(sBlockName) = -1) then
  begin
    block := TGridBlock.Create(self);
    fBlockList.AddObject(sBlockName,block);
    iIndex := fBlockList.IndexOf(sBlockName);
    with TGridBlock(fBlockList.Objects[iIndex]) do
    begin
      TETRAD := false;
      BlockName := sBlockName;
      LocationE := dLocationE;
      LocationN := dLocationN;
    end;
  end;
end;

procedure TGridLayer.AddTETRADBlock(sBlockName:string;dLocationE,dLocationN,
  dElevation,dThickness,dX,dY:double;iRow,iCol:integer;bActive:boolean);

var
  iIndex:integer;
  block : TGridBlock;

begin
// check to see if this block is already in the list...
  if (fBlockList.IndexOf(sBlockName) = -1) then
  begin
    block := TGridBlock.Create(self);
    fBlockList.AddObject(sBlockName,block);
    iIndex := fBlockList.IndexOf(sBlockName);
    with TGridBlock(fBlockList.Objects[iIndex]) do
    begin
      TETRAD := true;
      BlockName := sBlockName;
      LocationE := dLocationE;
      LocationN := dLocationN;
      Elevation := dElevation;
      Thickness := dThickness;
      Vertices.Add(FloatToStr(dx));
      Vertices.Add(FloatToStr(dy));
      Row := iRow;
      Column := iCol;
      InActive := not bActive;
    end;
  end;
end;
// ----- TGridLayer.AddVertexOrder ---------------------------------------------
procedure TGridLayer.AddVertexOrder(sBlockName:string;vo:TStringList);

var
  iIndex : integer;

begin
  iIndex := BlockList.IndexOf(sBlockName);
  if (iIndex <> -1) then
    TGridBlock(BlockList.Objects[iIndex]).Vertices.Assign(vo);
end;
// ----- TGridLayer.ClearBlocks ------------------------------------------------
procedure TGridLayer.ClearBlocks;

begin
  while (fBlockList.Count > 0) do
  begin
    TGridBlock(fBlockList.Objects[0]).Free;
    fBlockList.Delete(0);
  end;
  fBlockList.Clear;
end;
// ----- TGRidLayer.GetBlockByName ---------------------------------------------
function TGRidLayer.GetBlockByName(sBlock:string):TGridBlock;

begin
  result := GetGridBlock(fBlockList.IndexOf(sBlock));
end;
// =============================================================================
// TGRIDLAYERS Implementation
// =============================================================================
constructor TGridLayers.Create(aGRid:TSimulationGrid);

begin
  inherited Create;
  fGrid := aGrid;
  fLayerList := TStringList.Create;
end;
// ----- TGRidLayers.Destroy ---------------------------------------------------
destructor TGridLayers.Destroy;

begin
  fLayerList.Free;
  inherited Destroy;
end;
// ----- TGridLayers.AddLayer --------------------------------------------------
procedure TGridLayers.AddLayer(sLayerName,sLayerType:string;
  delevation,dthickness:single);

var
  iIndex : integer;
  layer : TGridLayer;

begin
// check to see if this layer is already in the list...
  if (fLayerList.IndexOf(sLayerName) = -1) then
  begin
    layer := TGridLayer.Create(self);
    fLayerList.AddObject(sLayerName,layer);
    iIndex := fLayerList.IndexOf(sLayerName);
    with TGridLayer(fLayerList.Objects[iIndex]) do
    begin
      LayerName := sLayerName;
      LayerType := sLayerType;
      Elevation := dElevation;
      Thickness := dThickness;
    end;
  end;
end;
// ----- TGridLayers.ClearLayers -----------------------------------------------
procedure TGridLayers.ClearLayers;

begin
  while (fLayerList.Count > 0) do
  begin
    TGridLayer(fLayerList.Objects[0]).Free;
    fLayerList.Delete(0);
  end;
  fLayerList.Clear;
end;
// ----- TGridLayers.DeleteLayer -----------------------------------------------
procedure TGridLayers.DeleteLayer(iIndex:integer);

begin
  if (iIndex >=0) and (iIndex < fLayerList.Count) then
  begin
    TGridLayer(fLayerList.Objects[iIndex]).Free;
    fLayerList.Delete(iIndex);
  end;
end;
// =============================================================================
// TSIMULATIONGRID Implementation
// =============================================================================
// ----- TSimulationGrid.GetRockType -------------------------------------------
function TSimulationGrid.GetRockType(iIndex:integer):TRockType;

begin
  if (iIndex >=0) and (iIndex < fRockTypeList.Count) then
    result := TRockType(fRockTypeList.Objects[iIndex])
  else
    result := nil;
end;
// ----- TSimulationGrid.GetGridVertex -----------------------------------------
function TSimulationGrid.GetGridVertex(iIndex:integer):TGridVertex;

begin
  if (iIndex >=0) and (iIndex < fVertexList.Count) then
    result := TGridVertex(fVertexList.Objects[iIndex])
  else
    result := nil;
end;
// ----- TSimulationGrid.GetVertexCount ----------------------------------------
function TSimulationGrid.GetVertexCount:integer;

begin
  result := fVertexList.Count;
end;
// ----- TSimulationGrid.GetMinVertexLimit -------------------------------------
function TSimulationGrid.GetMinVertexLimit : TSVect;

var
  i:integer;

begin
  if VertexCount > 0 then
  begin
    for i:=0 to VertexCount-1 do
      begin
// max singles
      result[0] := 3.4e+38;
      result[1] := result[0];
      result[2] := result[0];

      with TGridVertex(fVertexList.Objects[i]) do
      begin
        if (result[0] > LocationE) then
          result[0] := LocationE;
        if (result[1] > LocationN) then
          result[1] := LocationN;
      end;
    end;
  end else
  begin
    result[0] := 0.0;
    result[1] := 0.0;
    result[2] := 0.0;
  end;
end;
// ----- TSimulationGrid.GetMaxVertexLimit -------------------------------------
function TSimulationGrid.GetMaxVertexLimit : TSVect;

var
  i:integer;

begin
  if VertexCount > 0 then
  begin
    for i:=0 to VertexCount-1 do
      begin
// max singles
      result[0] := -3.4e+38;
      result[1] := result[0];
      result[2] := result[0];

      with TGridVertex(fVertexList.Objects[i]) do
      begin
        if (result[0] < LocationE) then
          result[0] := LocationE;
        if (result[1] < LocationN) then
          result[1] := LocationN;
      end;
    end;
  end else
  begin
    result[0] := 0.0;
    result[1] := 0.0;
    result[2] := 0.0;
  end;
end;
// ----- TSimulationGrid.Create ------------------------------------------------
constructor TSimulationGrid.Create;

begin
  inherited Create;
  fRockTypeList := TStringList.Create;
  fVertexList := TStringList.Create;
//  fGridLayers := TGridLayers.Create(self);
end;
// ----- TSimulationGrid.Destroy -----------------------------------------------
destructor TSimulationGrid.Destroy;

begin
  ClearRockTypes;
  fRockTypeList.Free;
  fVertexList.Free;
//  fGridLayers.Free;
  inherited Destroy;
end;
// ----- TSimulationGrid.AddRockType -------------------------------------------
procedure TSimulationGrid.AddRockType(sRock:string;dPermX,dPermY,dPermZ:double;
  dPorosity,dThermalConductivity,dSpecificHeat:single;sColourCode:string);

var
  iIndex : integer;
  rock : TRockType;

begin
// check to see if this rock is already in the list...
  if (fRockTypeList.IndexOf(sRock) = -1) then
  begin
    rock := TRockType.Create;
    fRockTypeList.AddObject(sRock,rock);
    iIndex := fRockTypeList.IndexOf(sRock);
    with TRockType(fRockTypeList.Objects[iIndex]) do
    begin
      RockName := sRock;
      PermeabilityX := dPermX;
      PermeabilityY := dPermY;
      PermeabilityZ := dPermZ;
      Porosity := dPorosity;
      ThermalConductivity := dThermalConductivity;
      SpecificHeat := dSpecificHeat;
      ColourCode := sColourCode;
    end;
  end;
end;
// ----- TSimulationGrid.AddVertex ---------------------------------------------
procedure TSimulationGrid.AddVertex(sVertex:string;x,y:single);

var
  iIndex : integer;
  v : TGridVertex;

begin
// check to see if this vertex is already in the list...
  if (fVertexList.IndexOf(sVertex) = -1) then
  begin
    v := TGridVertex.Create;
    fVertexList.AddObject(sVertex,v);
    iIndex := fVertexList.IndexOf(sVertex);
    with TGridVertex(fVertexList.Objects[iIndex]) do
    begin
      VertexLabel := sVertex;
      LocationE := x;
      LocationN := y;
    end;
  end;
end;
// ----- TSimulationGrid.ClearRockTypes ----------------------------------------
procedure TSimulationGrid.ClearRockTypes;

begin
  while (fRockTypeList.Count > 0) do
  begin
    TRockType(fRockTypeList.Objects[0]).Free;
    fRockTypeList.Delete(0);
  end;
  fRockTypeList.Clear;
end;
// ----- TSimulationGrid.ClearVertices -----------------------------------------
procedure TSimulationGrid.ClearVertices;

begin
  while (fVertexList.Count>0) do
    DeleteVertex(0);
  fVertexList.Clear;
end;
// ----- TSimulationGrid.DeleteVertex ------------------------------------------
procedure TSimulationGrid.DeleteVertex(iIndex:integer);

begin
  if (iIndex >=0) and (iIndex < fVertexList.Count) then
  begin
    TGridVertex(fVertexList.Objects[iIndex]).Free;
    fVertexList.Delete(iIndex);
  end;
end;
// ----- TSimulationGrid.DeleteVertexByLabel -----------------------------------
procedure TSimulationGrid.DeleteVertexByLabel(sLabel:string);

begin
  DeleteVertex(fVertexList.IndexOf(sLabel));
end;
// ----- TSimulationGrid.GetRockTypeByName -------------------------------------
function TSimulationGrid.GetRockTypeByName(sRock:string):TRockType;

begin
  result := GetRockType(fRockTypeList.IndexOf(sRock));
end;
// ----- TSimulationGrid.GetVertexByLabel --------------------------------------
function TSimulationGrid.GetVertexByLabel(sLabel:string):TGridVertex;

begin
  result := GetGridVertex(fVertexList.IndexOf(sLabel));
end;
// ----- TSimulationGrid.ClearAll ----------------------------------------------
procedure TSimulationGrid.ClearAll;

begin
  ClearRockTypes;
  ClearVertices;
end;
// =============================================================================
end.




