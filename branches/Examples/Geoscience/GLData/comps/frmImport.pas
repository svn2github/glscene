{
@abstract(form for use with )
@author(Aaron Hochwimer <aaron@graphic-edge.co.nz>)
@created(June 27, 2003)
@lastmod(June 29, 2003)
This unit provides the TformGEImporter - which is used in conjunction with the
TGEImportFile component.
}
unit frmImport;

interface

uses
  Windows,Messages,
  System.SysUtils, System.Variants, System.Classes, System.UITypes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB,
  Vcl.Grids, Vcl.DBGrids, Vcl.XPStyleActnCtrls, Vcl.ActnList, Vcl.ActnMan,
  Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Clipbrd
// Third party components
  {,kbmMemTable};

type
  TformGEImporter = class(TForm)
    ActionManager: TActionManager;
    bCancel: TBitBtn;
    bClear: TBitBtn;
    bOk: TBitBtn;
    bReload: TBitBtn;
    cbDelimiter: TComboBox;
    cbxGroupDelimiter: TCheckBox;
    cbxOverwrite: TCheckBox;
    dbgImport: TDBGrid;
    dsImport: TDataSource;
    ebDateFormat: TEdit;
    ebHeaderSkip: TEdit;
    {kbmImport: TkbmMemTable;}
    lblDateFormat: TLabel;
    lblDelimiter: TLabel;
    lblHeaderSkip: TLabel;
    ///pmFieldNames: TPopupActionBarEx;
    pnlSettings: TPanel;
    sBarImport: TStatusBar;
    upHeaderSkip: TUpDown;
    procedure bClearClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure bReloadClick(Sender: TObject);
    procedure cbDelimiterChange(Sender: TObject);
    procedure cbxGroupDelimiterClick(Sender: TObject);
    procedure dbgImportDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure dbgImportTitleClick(Column: TColumn);
    procedure ebHeaderSkipExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure kbmImportAfterDelete(DataSet: TDataSet);
    procedure kbmImportAfterInsert(DataSet: TDataSet);
    procedure kbmImportAfterPost(DataSet: TDataSet);
    procedure pmFieldNamesPopup(Sender: TObject);
    procedure SelectField(Sender:TObject);
    procedure SelectIgnore(Sender:TObject);
    procedure upHeaderSkipClick(Sender: TObject; Button: TUDBtnType);
  private
    iHeaderSkip : integer;
    iSelected : integer;
{programatically adds an entry to aMenu (the TMenu or TMenuItem you wish to add
the item to. aEvent is the TNotify event that will be called when selecting the
item. sCaption is the new menu items caption. bChangeCase will change the
caption's first character to uppercase and the rest to lower case. This function
 will return a newly created TMenuitem.
Example: AddMenuItem(popupmenu1,'New Item',nil,false);}
    function AddMenuItem(AMenu:TComponent;sCaption:string;aEvent:TNotifyEvent;
      bChangeCase:Boolean):TMenuItem;
{sets up the colours to indicate which rows and columns will be ignored}
    procedure SetColours;
{updates the display labels for the titles of the grid,dbgImport}
    procedure UpdateDisplayLabels;
  public
    allowsl : TStringList;
    bEditing : boolean;
    fsl : TStringList;
{toggles all columns to 'ignore' status}
    procedure CheckAllIgnore(var bIgnoreAll:boolean;fsl:TstringList);
{displays the record count of the dbgImport}
    procedure DisplayRecCount;
    procedure PrepImportTable;
{loads the delimiter type - there are two different types of delimiters: ',"
0= none ,1= ' ,2= " ,3='..',4=".."
The choice between 1 and 3 say is made with the AllDelimiters flag}
    procedure SetDelimiters(sDelimiter:string;
      bAllDelimiters,bGroupDelimiters:boolean);
    procedure SetHeaderSkip(iSkip:integer);
    procedure SetupFieldDefs(sl:TStringList);
  end;

implementation

uses
  geImportFile;

{$R *.dfm}
// ----- TformGEImporter.bClearClick -------------------------------------------
procedure TformGEImporter.bClearClick(Sender: TObject);

var
  i:integer;

begin
  for i:= 0 to fsl.Count-1 do
    fsl[i] := sIGNORE;
  UpdateDisplayLabels;
  SetColours;
end;
// ----- TformGEImporter.bOkClick ----------------------------------------------
procedure TformGEImporter.bOkClick(Sender: TObject);

var
  i:integer;
  bSomeEntries:boolean;

begin
  bSomeEntries := false;
  for i:=0 to fsl.Count-1 do
    ///if (kbmImport.Fields[i].DisplayLabel <> sIGNORE) then
    begin
      bSomeEntries := true;
      break;
    end;

  if not bSomeEntries then
  begin
    MessageDlg('Cannot import if there are no columns assigned!',
      mtInformation, [mbOK], 0);
    ModalResult := mrNone;
    exit;
  end;
end;
// ----- TformGEImporter.bReloadClick ------------------------------------------
procedure TformGEImporter.bReloadClick(Sender: TObject);

var
  clip:TStringList;

begin
  if (MessageDlg('Are you sure you wish to reload the data?', mtConfirmation,
    [mbYes,mbNo], 0) = mrYes) then
  begin
    UpdateDisplayLabels;
    bEditing := false;
    if (TGEImportFile(owner).ImportFileType = ifClipboard) then
    begin
      clip := TStringList.Create;
      clip.Text := Clipboard.AsText;
      TGEImportFile(owner).ImportDataFromClipboard(clip);
      clip.Free;
    end else
      TGEImportFile(owner).ImportDataFromFile(0,false);
    bEditing := true;
    UpdateDisplayLabels;
  end;
end;
// ----- TformGEImporter.cbDelimiterChange -------------------------------------
procedure TformGEImporter.cbDelimiterChange(Sender: TObject);
begin
// evaluated the delimiter based on the selection - must do it here because of
// the provision to "reload" the data
  with TGEImportFile(Owner) do
  begin
    case cbDelimiter.ItemIndex of
      0: Delimiter := '';
      1,3: Delimiter := '''';
      2,4: Delimiter := '"';
    end;
    case cbDelimiter.ItemIndex of
      1,2: AllDelimiters:=true;
      0,3,4: AllDelimiters:=false;
    end;
  end;
end;
// ----- TformGEImporter.cbxGroupDelimiterClick --------------------------------
procedure TformGEImporter.cbxGroupDelimiterClick(Sender: TObject);

begin
  TGEImportFile(Owner).GroupDelimiters := cbxGroupDelimiter.Checked;
end;
// ----- TformGEImporter.dbgImportDrawColumnCell -------------------------------
procedure TformGEImporter.dbgImportDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);

begin
(*
  if (kbmImport.RecNo <= iHeaderSkip) and not (gdSelected in State) then
  begin
    dbgImport.Canvas.Brush.Color := clInfoBk;
    dbgImport.Canvas.FillRect(Rect);
    dbgimport.Canvas.TextOut(Rect.Left+2,Rect.Top+2,Column.Field.DisplayText);
  end else
    dbgImport.DefaultDrawColumnCell(Rect,DataCol,Column,State);
*)
end;
// ----- TformGEImporter.dbgImportTitleClick -----------------------------------
procedure TformGEImporter.dbgImportTitleClick(Column:TColumn);

begin
  dbgImport.SelectedIndex := Column.Field.FieldNo-1;
  ///pmFieldNames.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;
// ----- TformGEImporter.ebHeaderSkipExit --------------------------------------
procedure TformGEImporter.ebHeaderSkipExit(Sender: TObject);

begin
  try
    iHeaderSkip := StrToInt(ebHeaderSkip.Text);
    if (iHeaderSkip < 0) then
    begin
      iHeaderskip := 0;
      ebHeaderSkip.Text := '0';
    end;
    TGEImportFile(owner).HeaderSkip := iHeaderSkip;
    SetColours;
    dbgImport.Refresh;
  except
    ebHeaderSkip.Text := IntToStr(iHeaderSkip);
  end;
end;
// ----- TformGEImporter.FormCreate --------------------------------------------
procedure TformGEImporter.FormCreate(Sender: TObject);

begin
  allowsl := TStringList.Create;
  fsl := TStringList.Create;
end;
// ----- TformGEImporter.FormDestroy -------------------------------------------
procedure TformGEImporter.FormDestroy(Sender: TObject);

begin
  allowsl.free;
  fsl.free;
end;
// ----- TformGEImporter.FormShow ----------------------------------------------
procedure TformGEImporter.FormShow(Sender: TObject);

begin
  if (ebDateFormat.Text = '') then
    ebDateFormat.Text := System.SysUtils.FormatSettings.ShortDateFormat;
  bEditing := true;
  UpdateDisplayLabels;
end;
// ----- TformGEImporter.kbmImportAfterDelete ----------------------------------
procedure TformGEImporter.kbmImportAfterDelete(DataSet: TDataSet);

begin
  if bEditing then
    DisplayRecCount;
end;
// ----- TformGEImporter.kbmImportAfterInsert ----------------------------------
procedure TformGEImporter.kbmImportAfterInsert(DataSet: TDataSet);

begin
  if bEditing then
    DisplayRecCount;
end;
// ----- TformGEImporter.kbmImportAfterPost ------------------------------------
procedure TformGEImporter.kbmImportAfterPost(DataSet: TDataSet);
begin
  if bEditing then
    DisplayRecCount;
end;

// ----- TformImporter.pmFieldNamesPopup ---------------------------------------
procedure TformGEImporter.pmFieldNamesPopup(Sender: TObject);

var
  i : integer;
  sFieldName : string;
  mi : TMenuItem;

begin
  iSelected := dbgimport.SelectedField.FieldNo-1;
// clear out the menu
  ///pmFieldNames.Items.Clear;

// add the first entry to the menu as Ignore
  ///mi := AddMenuItem(pmFieldNames,sIGNORE,SelectIgnore,false);
  mi.Checked := (sIGNORE = fsl.Strings[iSelected]);

// adds separator
  ///AddMenuItem(pmFieldNames,'-',nil,false);

// construct the menu from the list of allowable fields - minus any allocated
// fields e.g. (allow - fsl)}
  for i := 0 to allowsl.count-1 do
  begin
    sFieldName := allowsl.Strings[i];
    if (fsl.IndexOf(sFieldName) = -1) then
      ///AddMenuItem(pmFieldNames,sFieldName,SelectField,false);
  end;
end;
// ----- TformImporter.SelectField ---------------------------------------------
procedure TformGEImporter.SelectField(Sender:Tobject);

begin
  fsl.Strings[iSelected] := TMenuItem(Sender).Caption;
  ///kbmImport.Fields[iSelected].DisplayLabel:=fsl.Strings[iSelected];
  SetColours;
end;
// ----- TformImporter.SelectIgnore --------------------------------------------
procedure TformGEImporter.SelectIgnore(Sender:Tobject);

begin
 fsl.Strings[iSelected] := sIGNORE;
 ///kbmImport.Fields[iSelected].DisplayLabel:=fsl.Strings[iSelected];
 SetColours;
end;
// ----- TformGEImporter.upHeaderSkipClick -------------------------------------
procedure TformGEImporter.upHeaderSkipClick(Sender: TObject;
  Button: TUDBtnType);
begin
  if (Button = btNext) then
  begin
    Inc(iheaderskip);
    ebheaderSkip.Text := InttoStr(iHeaderskip);
    ebHeaderskipExit(nil);
  end else if (Button = btPrev) then
  begin
    if iHeaderSkip > 0 then
    begin
      Dec(iHeaderSkip);
      ebheaderSkip.Text := InttoStr(iHeaderskip);
      ebHeaderskipExit(nil);
    end;
  end;
end;
//------ AddMenuItem -----------------------------------------------------------
function TformGEImporter.AddMenuItem(AMenu:TComponent;sCaption:string;AEvent:TNotifyEvent;
 bChangeCase: Boolean):TMenuItem;

var
  i:integer;

begin
  result := TMenuItem.Create(aMenu.Owner);
  if bChangeCase then
  begin
    if (Pos('&',sCaption) = 1) then
      i:=1
    else
      i:=0;
    sCaption := UpperCase(Copy(sCaption,1,i+1)) +
      LowerCase(Copy(sCaption,2+i,Length(sCaption)));
  end;
// assign caption and event to result
  result.Caption := sCaption;
  result.OnClick := aEvent;

  if (aMenu is TPopupMenu) or (aMenu is TMainMenu) then
    TPopupMenu(aMenu).Items.Add(result)
  else if (aMenu is TMenuItem) then
    TMenuItem(aMenu).Add(result);
end;
// ----- TformGEImporter.SetColours --------------------------------------------
procedure TformGEImporter.SetColours;

var
 i:integer;

begin
  with dbgimport do
  begin
    Columns.RebuildColumns;
    for i:= 0 to Columns.Count-1 do
    begin
      Columns[i].Width := 100;
      Columns[i].Title.Alignment := taCenter;
      if (fsl.Strings[i] = sIGNORE) then
        Columns[i].Color := clInfoBk
      else
        Columns[i].Color := clWhite;
    end;
  end;
end;
// ----- TformGEImporter.UpdateDisplayLabels -----------------------------------
procedure TformGEImporter.UpdateDisplayLabels;

var
 i:integer;

begin
// only loop over visible columns
{
  for i:=0 to kbmImport.Fieldcount-1 do
    kbmImport.Fields[i].DisplayLabel := fsl.Strings[i]
}
end;
// ------ TformImporter.CheckAllIgnore -----------------------------------------
procedure TformGEImporter.CheckAllIgnore(var bIgnoreAll:Boolean;fsl:TStringList);

var
  i:integer;

begin
  bIgnoreAll := true;
(*
  for i:=0 to kbmImport.FieldCount-1 do
  begin
    if (fsl.Strings[i] <> sIGNORE) then
    begin
      bIgnoreAll := false;
      break;
    end;
  end;
*)
end;
// ----- TformImporter.DisplayRecCount -----------------------------------------
procedure TformGEImporter.DisplayRecCount;

begin
  ///sBarImport.Panels[0].Text := IntToStr(kbmImport.RecordCount);
end;
// ----- TformImporter.PrepImportTable -----------------------------------------
procedure TformGEImporter.PrepImportTable;

begin
(*
  with kbmImport do
  begin
    DisableControls;
    if Active then
      Close;
    Open;
    First;
  end;
*)
end;
// ------ TFormImporter.SetDelimiters ------------------------------------------
procedure TformGEImporter.SetDelimiters(sDelimiter:string;
  bAllDelimiters,bGroupDelimiters:boolean);

begin
  if (sDelimiter = '') then
    cbDelimiter.ItemIndex := 0
  else if (sDelimiter = '"') then
  begin
    if bAllDelimiters then
      cbDelimiter.ItemIndex := 2
    else
      cbDelimiter.ItemIndex := 4;
  end else
  begin
    if bAllDelimiters then
      cbDelimiter.ItemIndex := 1
    else
      cbDelimiter.ItemIndex := 3;
  end;
  cbxGroupDelimiter.Checked := bGroupDelimiters;
end;
// ------ TFormImporter.SetHeaderSkip ------------------------------------------
procedure TformGEImporter.SetHeaderSkip(iSkip:integer);

begin
  iHeaderSkip := iSkip;
  ebHeaderSkip.Text := IntToStr(iSkip);
end;
//------ TformImporter.SetupFieldDefs ------------------------------------------
procedure TformGEImporter.SetupFieldDefs(sl:TStringList);

var
 i:integer;

begin
// now assign entries from Stored fields and stored units
(*
  kbmImport.FieldDefs.Clear;
  for i:=0 to sl.Count-1 do
  begin
    with kbmImport.FieldDefs.AddFieldDef do
    begin
      Name := 'Field' + IntToStr(i);
      DataType:= ftstring;
      Size := 255;
    end;
  end;
// add ignores where appropriate
  while (sl.Count > fsl.Count) do
    fsl.Add(SIGNORE);

// prune other columns
  while (sl.Count < fsl.Count) do
    fsl.Delete(fsl.Count-1);

  kbmImport.Active:=true;
*)
  SetColours;
end;
// =============================================================================
end.
