{-------------------------------------------------------------------------------
 Unit Name: frmColourTool
 Author:    HochwimmerA
 Purpose:   Colour Tool - used to enter colour by RGBA or Hex and output to file
   Thanks to Dave Kerr for the suggestion.
 History:
-------------------------------------------------------------------------------}
unit frmColourTool;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, ComCtrls, geExportFile, geImportFile,
  ToolWin, ActnMan, ActnCtrls, XPStyleActnCtrls, ActnList, StdCtrls,
  ExtCtrls, {kbmMemTable,} cUtilities, ImgList, GLTexture, GLColor ;

type
  TformColourEdit = class(TForm)
    ActionManager: TActionManager;
    ActionToolBarColourEdit: TActionToolBar;
    dsRGBA: TDataSource;
    geExportFile: TGEExportFile;
    geImportFile: TGEImportFile;

    PageControl1: TPageControl;

    tsHex: TTabSheet;
    tsRGB: TTabSheet;
    dsHex: TDataSource;
    dbgRGBA: TDBGrid;
    dbgHex: TDBGrid;
    acImport: TAction;
    ImageList: TImageList;
    acExport: TAction;
    acImportCSV: TAction;
    acImportTSV: TAction;
    acImportSSV: TAction;
    acImportFixed: TAction;
    acImportClipboard: TAction;
    acExportCSV: TAction;
    acExportSSV: TAction;
    acExportTSV: TAction;
    acExportLatex: TAction;
    acExportXML: TAction;
    acExportClipboard: TAction;
    acDone: TAction;
    ColorDialog: TColorDialog;
    ///kbmRGBA: TkbmMemTable;
    ///kbmHex: TkbmMemTable;
    procedure dbgHexDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure acImportExecute(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dbgRGBADrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure kbmRGBABeforePost(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acExportUpdate(Sender: TObject);
    procedure acImportClipboardExecute(Sender: TObject);
    procedure acImportClipboardUpdate(Sender: TObject);
    procedure acImportCSVExecute(Sender: TObject);
    procedure acImportCSVUpdate(Sender: TObject);
    procedure acImportFixedExecute(Sender: TObject);
    procedure acImportFixedUpdate(Sender: TObject);
    procedure acImportSSVExecute(Sender: TObject);
    procedure acImportSSVUpdate(Sender: TObject);
    procedure acImportTSVExecute(Sender: TObject);
    procedure acImportTSVUpdate(Sender: TObject);
    procedure acExportCSVExecute(Sender: TObject);
    procedure acExportCSVUpdate(Sender: TObject);
    procedure acExportSSVExecute(Sender: TObject);
    procedure acExportSSVUpdate(Sender: TObject);
    procedure acExportTSVExecute(Sender: TObject);
    procedure acExportTSVUpdate(Sender: TObject);
    procedure acExportLatexExecute(Sender: TObject);
    procedure acExportLatexUpdate(Sender: TObject);
    procedure acExportXMLExecute(Sender: TObject);
    procedure acExportXMLUpdate(Sender: TObject);
    procedure acExportClipboardExecute(Sender: TObject);
    procedure acExportClipboardUpdate(Sender: TObject);
    procedure acDoneExecute(Sender: TObject);
    procedure dbgHexDblClick(Sender: TObject);
    procedure dbgRGBADblClick(Sender: TObject);
  private
    aGLColour : TGLColor;

     
  public
     
  end;

implementation

{$R *.dfm}
// ----- TfrmColourEdit.dbgHexDrawColumnCell -----------------------------------
procedure TformColourEdit.dbgHexDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);

begin
  if (Column.Field.FieldName <> 'Hex') then
    dbgHex.DefaultDrawColumnCell(Rect,DataCol,Column,State)
  else
  begin
{** Colour}
    if (Column.Field.DisplayText = '') then
      dbgHex.Canvas.Brush.Color := clWhite
    else
      try
        dbgHex.Canvas.Brush.Color := HexToColor(Column.Field.DisplayText);
      except
        dbgHex.Canvas.Brush.Color := clWhite;
      end;
    dbgHex.DefaultDrawColumnCell(Rect,DataCol,Column,State);
  end;
end;
// ----- TfrmColourEdit.acImportExecute ----------------------------------------
procedure TformColourEdit.acImportExecute(Sender: TObject);

begin
  ///geImportFile.Destination.Edit;
  geImportFile.Execute;
end;
// ----- TfrmColourEdit.PageControl1Change -------------------------------------
procedure TformColourEdit.PageControl1Change(Sender: TObject);

begin
  if PageControl1.ActivePage = tsHex then
  begin
    geImportFile.Identifier := 'Hex';
    ///geImportFile.Destination := kbmHex;
    geExportFile.Identifier := 'Hex';
    ///geExportFile.Source := kbmHex;
  end else if (PageControl1.ActivePage = tsRGB) then
  begin
    geImportFile.Identifier := 'RGBA';
    ///geImportFile.Destination := kbmRGBA;
    geExportFile.Identifier := 'RGBA';
    ///geExportFile.Source := kbmRGBA;
  end;
end;
// ----- TfrmColourEdit.FormShow -----------------------------------------------
procedure TformColourEdit.FormShow(Sender: TObject);
begin
  PageControl1Change(nil); // set up the hex
end;
// ----- TformColourEdit.dbgRGBADrawColumnCell ---------------------------------
procedure TformColourEdit.dbgRGBADrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);

begin
  if (Column.Field.FieldName <> 'Hex') then
    dbgRGBA.DefaultDrawColumnCell(Rect,DataCol,Column,State)
  else
  begin
{** Colour}
    if (Column.Field.DisplayText = '') then
      dbgRGBA.Canvas.Brush.Color := clWhite
    else
      try
        dbgRGBA.Canvas.Brush.Color := HexToColor(Column.Field.DisplayText);
      except
        dbgRGBA.Canvas.Brush.Color := clWhite;
      end;
    dbgRGBA.DefaultDrawColumnCell(Rect,DataCol,Column,State);
  end;
end;
// ----- TformColourEdit.kbmRGBABeforePost -------------------------------------
// before posting ensure that the R,G,B,A fields are bounded : [0,1]
procedure TformColourEdit.kbmRGBABeforePost(DataSet: TDataSet);

  procedure BoundField(aField:TField);

  begin
    if (aField.AsFloat < 0.0) then
      aField.AsFloat := 0.0
    else if (aField.AsFloat > 1.0) then
      aField.AsFloat := 1.0;
  end;

begin
(*
  BoundField(kbmRGBA.FieldByName('Red'));
  BoundField(kbmRGBA.FieldByName('Green'));
  BoundField(kbmRGBA.FieldByName('Blue'));
  BoundField(kbmRGBA.FieldByName('Alpha'));

// calculate Hex string
  aGLColour.Red := kbmRGBA.FieldByName('Red').AsFloat;
  aGLColour.Blue := kbmRGBA.FieldByName('Blue').AsFloat;
  aGLColour.Green := kbmRGBA.FieldByName('Green').AsFloat;
  aGLColour.Alpha := kbmRGBA.FieldByName('Alpha').AsFloat;

  kbmRGBA.FieldByName('Hex').AsString := ColorToHex(aGLColour.AsWinColor);
*)
end;
// ----- TformColourEdit.FormCreate --------------------------------------------
procedure TformColourEdit.FormCreate(Sender: TObject);

begin
  aGLColour := TGLColor.Create(nil);
end;
// ----- TformColourEdit.FormDestroy -------------------------------------------
procedure TformColourEdit.FormDestroy(Sender: TObject);
begin
  aGLColour.Free;
end;
// ----- TformColourEdit.acExportExecute ---------------------------------------
procedure TformColourEdit.acExportExecute(Sender: TObject);
begin
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportUpdate ----------------------------------------
procedure TformColourEdit.acExportUpdate(Sender: TObject);

begin
  acExport.Enabled := (geExportFile.Source.RecordCount > 0);
end;
// ----- TformColourEdit.acImportCSVExecute ------------------------------------
procedure TformColourEdit.acImportCSVExecute(Sender: TObject);
begin
  geImportFile.ImportFileType := ifCSV;
  acImport.Execute;
end;
// ----- TformColourEdit.acImportCSVUpdate -------------------------------------
procedure TformColourEdit.acImportCSVUpdate(Sender: TObject);
begin
  acImportCSV.Checked := (geImportFile.ImportFileType = ifCSV);
end;
// ----- TformColourEdit.acImportTSVExecute ------------------------------------
procedure TformColourEdit.acImportTSVExecute(Sender: TObject);
begin
  geImportFile.ImportFileType := ifTSV;
  acImport.Execute;
end;
// ----- TformColourEdit.acImportTSVUpdate -------------------------------------
procedure TformColourEdit.acImportTSVUpdate(Sender: TObject);
begin
  acImportTSV.Checked := (geImportFile.ImportFileType = ifTSV);
end;
// ----- TformColourEdit.acImportSSVExecute ------------------------------------
procedure TformColourEdit.acImportSSVExecute(Sender: TObject);
begin
  geImportFile.ImportFileType := ifSSV;
  acImport.Execute;
end;
// ----- TformColourEdit.acImportSSVUpdate -------------------------------------
procedure TformColourEdit.acImportSSVUpdate(Sender: TObject);
begin
  acImportSSV.Checked := (geImportFile.ImportFileType = ifSSV);
end;
// ----- TformColourEdit.acImportFixedExecute ----------------------------------
procedure TformColourEdit.acImportFixedExecute(Sender: TObject);

begin
  geImportFile.ImportFileType := ifFixedFormatSpace;
  acImport.Execute;
end;
// ----- TformColourEdit.acImportFixedUpdate -----------------------------------
procedure TformColourEdit.acImportFixedUpdate(Sender: TObject);

begin
  acImportFixed.Checked := (geImportFile.ImportFileType = ifFixedFormatSpace);
end;
// ----- TformColourEdit.acImportClipboardExecute ------------------------------
procedure TformColourEdit.acImportClipboardExecute(Sender: TObject);
begin
  geImportFile.ImportFileType := ifClipboard;
  acImport.Execute;
end;
// ----- TformColourEdit.acImportClipboardUpdate -------------------------------
procedure TformColourEdit.acImportClipboardUpdate(Sender: TObject);

begin
  acImportClipboard.Checked := (geImportFile.ImportFileType = ifClipboard);
end;
// ----- TformColourEdit.acExportCSVExecute ------------------------------------
procedure TformColourEdit.acExportCSVExecute(Sender: TObject);

begin
  GEExportFile.ExportFileType := efCSV;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportCSVUpdate -------------------------------------
procedure TformColourEdit.acExportCSVUpdate(Sender: TObject);

begin
  acExportCSV.Checked := (geExportFile.ExportFileType = efCSV);
end;
// ----- TformColourEdit.acExportSSVExecute ------------------------------------
procedure TformColourEdit.acExportSSVExecute(Sender: TObject);

begin
  GEExportFile.ExportFileType := efSSV;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportSSVUpdate -------------------------------------
procedure TformColourEdit.acExportSSVUpdate(Sender: TObject);

begin
  acExportSSV.Checked := (geExportFile.ExportFileType = efSSV);
end;
// ----- TformColourEdit.acExportTSVExecute ------------------------------------
procedure TformColourEdit.acExportTSVExecute(Sender: TObject);

begin
  GEExportFile.ExportFileType := efTSV;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportTSVUpdate -------------------------------------
procedure TformColourEdit.acExportTSVUpdate(Sender: TObject);

begin
  acExportTSV.Checked := (geExportFile.ExportFileType = efTSV);
end;
// ----- TformColourEdit.acExportLatexExecute ----------------------------------
procedure TformColourEdit.acExportLatexExecute(Sender: TObject);

begin
  GEExportFile.ExportFileType := efLaTex;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportLatexUpdate -----------------------------------
procedure TformColourEdit.acExportLatexUpdate(Sender: TObject);

begin
  acExportLatex.Checked := (geExportFile.ExportFileType = efLatex);
end;
// ----- TformColourEdit.acExportXMLExecute ------------------------------------
procedure TformColourEdit.acExportXMLExecute(Sender: TObject);
begin
  GEExportFile.ExportFileType := efXML;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportXMLUpdate -------------------------------------
procedure TformColourEdit.acExportXMLUpdate(Sender: TObject);

begin
  acExportXML.Checked := (geExportFile.ExportFileType = efXML);
end;
// ----- TformColourEdit.acExportClipboardExecute ------------------------------
procedure TformColourEdit.acExportClipboardExecute(Sender: TObject);

begin
  GEExportFile.ExportFileType := efClipboard;
  GEExportFile.Execute;
end;
// ----- TformColourEdit.acExportClipboardUpdate -------------------------------
procedure TformColourEdit.acExportClipboardUpdate(Sender: TObject);

begin
  acExportClipboard.Checked := (geExportFile.ExportFileType = efClipboard);
end;
// ----- TformColourEdit.acDoneExecute -----------------------------------------
procedure TformColourEdit.acDoneExecute(Sender: TObject);

begin
  Close;
end;
// =============================================================================
procedure TformColourEdit.dbgHexDblClick(Sender: TObject);
begin
(*
  if (dbgHex.Columns.Items[dbgHex.SelectedIndex].FieldName = 'Hex') then
  begin
    if (kbmHex.FieldByName('Hex').AsString <> '') then
    begin
      try
        ColorDialog.Color := HexToColor(kbmHex.FieldByName('Hex').AsString)
      except
        ColorDialog.Color := clWhite;
      end;
    end else
      ColorDialog.Color := clWhite;
    if ColorDialog.Execute then
    begin
      kbmHex.Edit;
      kbmHex.FieldByName('Hex').AsString := ColorToHex(ColorDialog.Color);
      kbmHex.Post;
    end;
  end;
*)
end;
// ----- TformColourEdit.dbgRGBADblClick ---------------------------------------
procedure TformColourEdit.dbgRGBADblClick(Sender: TObject);
begin
(*
  if (dbgRGBA.Columns.Items[dbgRGBA.SelectedIndex].FieldName = 'Hex') then
  begin
    if (kbmRGBA.FieldByName('Hex').AsString <> '') then
    begin
      try
        ColorDialog.Color := HexToColor(kbmRGBA.FieldByName('Hex').AsString)
      except
        ColorDialog.Color := clWhite;
      end;
    end else
      ColorDialog.Color := clWhite;

    if ColorDialog.Execute then
    begin
      aGLColour.AsWinColor := ColorDialog.Color;
      kbmRGBA.Edit;
      kbmRGBA.FieldByName('Red').AsFloat := aGLColour.Red;
      kbmRGBA.FieldByName('Green').AsFloat := aGLColour.Green;
      kbmRGBA.FieldByName('Blue').AsFloat := aGLColour.Blue;
      kbmRGBA.Post;
    end;
  end;
*)
end;
// =============================================================================
end.


