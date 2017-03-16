{-------------------------------------------------------------------------------
 Unit Name: frmGeosimGrid
 Author:    HochwimmerA
 Purpose:   Used to enter data for a geothermal simulation grid (e.g. TOUGH)
 History:
-------------------------------------------------------------------------------}
unit frmGeosimGrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Grids, DBGrids, ComCtrls, ExtCtrls, ActnList,
  DB, {kbmMemTable,} XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls,
  geImportFile, ImgList,cGLSimulationGrids,cUtilities;

type
  TGridMode =(gmTOUGH,gmTETRAD);

  TformGeoGrid = class(TForm)
    pcGridInformation: TPageControl;
    pnlBottom: TPanel;
    tsVertices: TTabSheet;
    dbgVertices: TDBGrid;
    bOK: TBitBtn;
    ActionManager: TActionManager;
    dsVertices: TDataSource;
    GEImportFile: TGEImportFile;
    ImageList: TImageList;
    tsLayers: TTabSheet;
    dbgLayers: TDBGrid;
    dsLayers: TDataSource;
    tsBlocks: TTabSheet;
    acImport: TAction;
    dbgBlocks: TDBGrid;
    dsBlocks: TDataSource;
    tsVertexOrder: TTabSheet;
    dbgVertexOrder: TDBGrid;
    dsVertexOrder: TDataSource;
    tsRocks: TTabSheet;
    dbgRocks: TDBGrid;
    dsRocks: TDataSource;
    tsRockDist: TTabSheet;
    dsRockDistribution: TDataSource;
    dbgRockDist: TDBGrid;
    tsPartial: TTabSheet;
    dsPartialLayers: TDataSource;
    dbgPartialLayers: TDBGrid;
    ColorDialog: TColorDialog;
    BitBtn1: TBitBtn;
    ActionToolBar: TActionToolBar;
    acOK: TAction;
    tsPTS: TTabSheet;
    dsPTS: TDataSource;
    dbgPTS: TDBGrid;
    tsTETRAD: TTabSheet;
    dbgTETRADGrid: TDBGrid;
    dsTETRAD: TDataSource;
    tsTETRADRocks: TTabSheet;
    dbgTETRADRocks: TDBGrid;
    dsTETRADRocks: TDataSource;
    tsTETRADPTS: TTabSheet;
    dsTETRADPTS: TDataSource;
    dbgTETRADPTS: TDBGrid;
    ///kbmTETRAD: TkbmMemTable;
    ///kbmTETRADRocks: TkbmMemTable;
    ///kbmTETRADPTS: TkbmMemTable;
    ///kbmBlocks: TkbmMemTable;
    ///kbmVertices: TkbmMemTable;
    ///kbmLayers: TkbmMemTable;
    ///kbmVertexOrder: TkbmMemTable;
    ///kbmRocks: TkbmMemTable;
    ///kbmRockDist: TkbmMemTable;
    ///kbmPartialLayers: TkbmMemTable;
    ///kbmPTS: TkbmMemTable;
    procedure FormShow(Sender: TObject);
    procedure pcGridInformationChange(Sender: TObject);

    procedure acImportExecute(Sender: TObject);
    procedure dbgRocksDblClick(Sender: TObject);
    procedure dbgRocksDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
     
  public
    gm: TGridMode;
    iRow,iCol : integer;
    procedure SaveGrid(aGrid:TGLSimulationGrid;var bl:TStringlist);
     
  end;

implementation

{$R *.dfm}
// ----- TformGeoGrid.SaveGrid -------------------------------------------------
// populates aGrid with data from the memory tables}
procedure TformGeoGrid.SaveGrid(aGrid:TGLSimulationGrid;var bl:TStringList);

var
  sBlock,sRock,sVertex,sLayer,slayerType,sTemp:string;
  x,y,dElevation,dThickness:double;
  i : integer;
  sl : TStringList;
  bActive:boolean;

begin
(*
// importing a TOUGH/MULKOM style grid
  if (gm = gmTOUGH) then
  begin
// Vertices
    with kbmVertices do
    begin
      First;
      while not eof do
      begin
        sVertex:=FieldByName('Vertex').AsString;
        x := FieldByName('Location E').AsFloat;
        y := FieldByName('Location N').AsFloat;
        aGrid.AddVertex(sVertex,x,y);
        Next;
      end;
    end;

// Layers
    with kbmLayers do
    begin
      SortOn('Elevation',[]); // order descending
      First;
      while not eof do
      begin
        sLayer := FieldByName('Layer').AsString;
        sLayerType := FieldByName('Layer Type').AsString;
        dElevation := FieldByName('Elevation').AsFloat;
        dThickness := FieldByName('Thickness').AsFloat;
        aGrid.GridLayers.AddLayer(sLayer,sLayerType,dElevation,dThickness);
        Next;
      end;
    end;
// rocks
    with kbmRocks do
    begin
      First;
      while not eof do
      begin
        aGrid.AddRockType(
          FieldByName('Rock Code').AsString,
          FieldByName('Permeability-X').AsFloat,
          FieldByName('Permeability-Y').AsFloat,
          FieldByName('Permeability-Z').AsFloat,
          FieldByName('Porosity').AsFloat,
          FieldByName('Thermal Conductivity').AsFloat,
          FieldByName('Specific Heat').AsFloat,
          FieldByName('Colour Code').AsString);
        Next;
      end;
    end;

// adding blocks
    bl.clear;
    with kbmBlocks do
    begin
      First;
      while not eof do
      begin
        sBlock := FieldByName('Grid Block').AsString;
        if (bl.IndexOf(sBlock)=-1) then
          bl.Add(sBlock);
        x := FieldByName('Location E').AsFloat;
        y := FieldByName('Location N').AsFloat;

// loop over layers;
        for i:=0 to aGrid.GridLayers.LayerList.Count-1 do
        begin
          sLayer := aGrid.GridLayers.GLLayer[i].LayerName;
          if (agrid.GridLayers.GLLayer[i].LayerType <> 'P') then
            agrid.GridLayers.GLLayer[i].AddBlock(sBlock,x,y)
        end;
        Next;
      end;
    end;

// partial layers
    with kbmPartialLayers do
    begin
      First;
      while not eof do
      begin
        sLayer := FieldByName('Grid Layer').AsString;
        sBlock := FieldByName('Grid Block').AsString;

        if kbmBlocks.Locate('Grid Block',sBlock,[]) then
        begin
          TGLGridLayer(aGrid.GridLayers.GetGLLayerByName(slayer)).AddBlock(
            sBlock,
            kbmBlocks.FieldByName('Location E').AsFloat,
            kbmBlocks.FieldByName('Location N').AsFloat);
        end;
        Next;
      end;
    end;

// grid rock types
    with kbmRockDist do
    begin
      First;
      while not eof do
      begin
        sLayer := FieldByName('Grid Layer').AsString;
        sBlock := FieldByName('Grid Block').AsString;
        sRock := FieldByName('Rock Type').AsString;
// get the right layer + block - needs safety check
        with aGrid.GridLayers.GetGLLayerByName(sLayer) do
        begin
          if (GetBlockByName(sBlock) <> nil) then
            GetBlockByname(sBlock).RockTypeCode := sRock;
        end;
        Next;
      end;
    end;

// PTS?
    with kbmPTS do
    begin
      First;
      while not eof do
      begin
        sLayer := FieldByName('Grid Layer').AsString;
        sBlock := FieldByName('Grid Block').AsString;
// obtain the correct layer and the correct blockn
        with aGrid.GridLayers.GetGLLayerByName(sLayer) do
        begin
          if (GetBlockByName(sBlock) <> nil) then
            with GetBlockByName(sBlock) do
            begin
              Pr := FieldByName('Pressure').AsFloat;
              Te := FieldByName('Temperature').AsFloat;
              Sv := FieldByName('Saturation').AsFloat;
            end;
        end;
        Next;
      end;
    end;

    sl := TStringList.Create;
    with kbmVertexOrder do
    begin
      First;
      sBlock := '';
      while not eof do
      begin
        if (sBlock <> FieldByName('Grid Block').AsString) then
        begin
          if sl.Count>0 then
          begin
            for i:=0 to agrid.GRidLayers.LayerList.Count-1 do
            begin
              if (LowerCase(agrid.GridLayers.GLLayer[i].LayerType) <> 'p') then
                TGLGridLayer(agrid.GridLayers.GLLayer[i]).AddVertexOrder(sBlock,sl)
              else
              begin
                if (TGLGridLayer(agrid.GridLayers.GLLayer[i]).GetBlockByName(sBlock) <> nil) then
                  TGLGridLayer(agrid.GridLayers.GLLayer[i]).AddVertexOrder(sBlock,sl)
              end;
            end;
          end;
          sl.Clear;
          sBlock := FieldByName('Grid Block').AsString;
        end;
        sl.Add(FieldByName('Grid Vertex').AsString);
        Next;
        if eof then
        begin
          for i:=0 to agrid.GRidLayers.LayerList.Count-1 do
          begin
            with TGLGridLayer(aGrid.GridLayers.GLLayer[i]) do
            begin
              if (LowerCase(LayerType) <> 'p') then
                AddVertexOrder(sBlock,sl)
              else
                if (GetBlockByName(sBlock) <> nil) then
                  AddVertexOrder(sBlock,sl)
            end;
          end;
        end;
      end;
    end;
    sl.Free;
  end else if (gm = gmTETRAD) then
  begin
    with kbmTETRAD do
    begin
      i := -1;
      iRow := 0;
      iCol := 0;
      DisableControls;
      First;
      sLayer := '';
      while not eof do
      begin
        sTemp := FieldByName('Layer').AsString;
        if (sTemp <> sLayer) then
        begin
          Inc(i);
          sLayer := sTemp;
          aGrid.GridLayers.AddTETRADLayer(sLayer);
        end;
        bActive := true;

        if kbmTETRADPTS.Locate('Block',VarArrayOf(
          [FieldByName('Block').AsString]),[]) then
          bActive := (kbmTETRADPTS.FieldByName('Active Block').AsInteger = 1);


        if FieldByName('Row').AsInteger > iRow then
          iRow := FieldByName('Row').AsInteger;

        if FieldByName('Column').AsInteger > iCol then
          iCol := FieldByName('Column').AsInteger;

        TGLGridLayer(aGrid.GridLayers.GetGLLayerByName(slayer)).AddTETRADBlock(
          FieldByName('Block').AsString,
          FieldByName('Location E').AsFloat,
          FieldByName('Location N').AsFloat,
          FieldByName('Elevation').AsFloat,
          FieldByName('Thickness').AsFloat,
          FieldByName('dX').AsFloat,
          FieldByName('dY').AsFloat,
          FieldByName('Row').AsInteger,
          FieldByName('Column').AsInteger,
          bActive);

        if kbmTETRADRocks.Locate('Block',VarArrayOf([FieldByName('Block').AsString]),[]) then
        begin
          aGrid.AddRockType(kbmTETRADRocks.FieldByName('Block').AsString,
            kbmTETRADRocks.FieldByName('Permeability-X').AsFloat,
            kbmTETRADRocks.FieldByName('Permeability-Y').AsFloat,
            kbmTETRADRocks.FieldByName('Permeability-Z').AsFloat,
            kbmTETRADRocks.FieldByName('Porosity').AsFloat,
            0.0,0.0,'ff0000');
        end;

        if kbmTETRADPTS.Locate('Block',VarArrayOf(
          [FieldByName('Block').AsString]),[]) then
        begin
          with aGrid.GridLayers.GLLayer[i] do
          begin
          if (GetBlockByName(FieldByName('Block').AsString) <> nil) then
            with GetBlockByName(FieldByName('Block').AsString) do
            begin
              Pr := kbmTETRADPTS.FieldByName('Pressure').AsFloat;
              Te := kbmTETRADPTS.FieldByName('Temperature').AsFloat;
              Sv := kbmTETRADPTS.FieldByName('Vapour Saturation').AsFloat;
            end;
          end;
        end;



        with aGrid.GridLayers.GetGLLayerByName(sLayer) do
        begin
          if (GetBlockByName(FieldByName('Block').AsString) <> nil) then
            GetBlockByname(FieldByName('Block').AsString).RockTypeCode := FieldByName('Block').AsString;
        end;

        Next;
      end;

      EnableControls;
    end;
  end;
*)
end;
// ----- TformGeoGrid.FormShow -------------------------------------------------
procedure TformGeoGrid.FormShow(Sender: TObject);
begin
(*
// assumes that the gridmode (GM) has been set prior:
  if (gm = gmTOUGH) then
  begin
    GEImportFile.Identifier := 'vertices';
    GEImportFile.Destination := kbmVertices;
// show and hide tabs
    tsVertices.TabVisible := true;
    tsLayers.TabVisible := true;
    tsBlocks.TabVisible := true;
    tsVertexOrder.TabVisible := true;
    tsRocks.TabVisible := true;
    tsRockDist.TabVisible := true;
    tsPartial.TabVisible := true;
    tsPTS.TabVisible := true;
    tsTETRAD.TabVisible := false;
    tsTETRADRocks.TabVisible := false;
    tsTETRADPTS.TabVisible := false;
    pcGridInformation.ActivePage := tsVertices;
  end else if (gm = gmTETRAD) then
  begin
    GEImportFile.Identifier := 'tetrad';
    GEImportFile.Destination := kbmTETRAD;
// show and hide tabs
    tsVertices.TabVisible := false;
    tsLayers.TabVisible := false;
    tsBlocks.TabVisible := false;
    tsVertexOrder.TabVisible := false;
    tsRocks.TabVisible := false;
    tsRockDist.TabVisible := false;
    tsPartial.TabVisible := false;
    tsPTS.TabVisible := false;
    tsTETRAD.TabVisible := true;
    tsTETRADRocks.TabVisible := true;
    tsTETRADPTS.TabVisible := true;
    pcGRidInformation.ActivePage := tsTETRAD;
  end;
*)
end;
// ----- TformGeoGrid.pcGridInformationChange ----------------------------------
procedure TformGeoGrid.pcGridInformationChange(Sender: TObject);
begin
(*
  if (pcGridInformation.ActivePage=tsVertices) then
  begin
    GEImportFile.Identifier := 'vertices';
    GEImportFile.Destination := kbmVertices;
  end else if (pcGridInformation.ActivePage=tsLayers) then
  begin
    GEImportFile.Identifier := 'layers';
    GEImportFile.Destination := kbmLayers;
  end else if (pcGridInformation.ActivePage = tsBlocks) then
  begin
    GEImportFile.Identifier := 'blocks';
    GEImportFile.Destination := kbmBlocks;
  end else if (pcGridInformation.ActivePage = tsVertexOrder) then
  begin
    GEImportFile.Identifier := 'vertexorder';
    GEImportFile.Destination := kbmVertexOrder;
  end else if (pcGridInformation.ActivePage = tsRocks) then
  begin
    GEImportFile.Identifier := 'rocks';
    GEImportFile.Destination := kbmRocks;
  end else if (pcGridInformation.ActivePage = tsRockDist) then
  begin
    GEImportFile.Identifier := 'rockdistribution';
    GEImportFIle.Destination := kbmRockDist;
  end else if (pcGridInformation.ActivePage = tsPartial) then
  begin
    GEImportFile.Identifier := 'partiallayers';
    GEImportFile.Destination := kbmPartialLayers;
  end else if (pcGridInformation.ActivePage = tsPTS) then
  begin
    GEImportFile.Identifier := 'pts';
    GEImportFile.Destination := kbmPTS;
  end else if (pcGridInformation.ActivePage = tsTETRAD) then
  begin
    GEImportFile.Identifier := 'tetrad';
    GEImportFile.Destination := kbmTETRAD;
  end else if (pcGridInformation.ActivePage = tsTETRADRocks) then
  begin
    GEImportFile.Identifier := 'tetradrocks';
    GEImportFile.Destination := kbmTETRADRocks;
  end else if (pcGridInformation.ActivePage = tsTETRADPTS) then
  begin
    GEImportFile.Identifier := 'tetradpts';
    GEImportFile.Destination := kbmTETRADPTS;
  end;
*)
end;

// ----- TformGeoGrid.acImportBlocksExecute ------------------------------------
procedure TformGeoGrid.acImportExecute(Sender: TObject);
begin
  GEImportFile.Execute;
end;
// ----- TformGeoGrid.dbgRocksDblClick -----------------------------------------
procedure TformGeoGrid.dbgRocksDblClick(Sender:TObject);
begin
(*
  if (dbgRocks.Columns.Items[dbgRocks.SelectedIndex].FieldName = 'Colour Code') then
  begin
    ColorDialog.Color := HexToColor(kbmRocks.FieldByName('Colour Code').AsString);
    if ColorDialog.Execute then
    begin
      kbmRocks.Edit;
      kbmRocks.FieldByName('Colour Code').AsString :=
        ColorToHex(ColorDialog.Color);
      kbmRocks.Post;
    end;
  end;
*)
end;
// ----- TformGeoGrid.dbgRocksDrawColumnCell -----------------------------------
procedure TformGeoGrid.dbgRocksDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
begin
  if (Column.Field.FieldName <> 'Colour Code') then
    dbgRocks.DefaultDrawColumnCell(Rect,DataCol,Column,State)
  else
{** Colour}
  begin
    if (Column.Field.DisplayText = '') then
      dbgRocks.Canvas.Brush.Color := clWhite
    else
      try
        dbgRocks.Canvas.Brush.Color := HexToColor(Column.Field.DisplayText);
      except
        dbgRocks.Canvas.Brush.Color := clWhite;
      end;
    dbgRocks.DefaultDrawColumnCell(Rect,DataCol,Column,State);
  end;
end;
// =============================================================================
end.


