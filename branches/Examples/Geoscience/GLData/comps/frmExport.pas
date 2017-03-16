unit frmExport;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, Data.DB;

type
  TformExporter = class(TForm)
    Panel2: TPanel;
    pnlAvailable: TPanel;
    pnlOrder: TPanel;
    bDown: TSpeedButton;
    bUp: TSpeedButton;
    bRemoveAll: TButton;
    bRemove: TButton;
    bAddAll: TButton;
    bAdd: TButton;
    Panel1: TPanel;
    lblAvailableFields: TLabel;
    listboxavailable: TListBox;
    Splitter1: TSplitter;
    pnlOptions: TPanel;
    pnlExport: TPanel;
    listboxfields: TListBox;
    Panel3: TPanel;
    Label1: TLabel;
    pnlTop: TPanel;
    lblExportOptions: TLabel;
    cbxOutputHeader: TCheckBox;
    cbxOpenAfterExport: TCheckBox;
    pnlDelimit: TPanel;
    lblDelimiter: TLabel;
    cbxOnlySTrings: TCheckBox;
    cbDelimiter: TComboBox;
    pnlDateFiltering: TPanel;
    lblDateFilter: TLabel;
    cbDateTimeField: TComboBox;
    cbxLowerBound: TCheckBox;
    cbxUpperBound: TCheckBox;
    dtpLowerDate: TDateTimePicker;
    dtpUpperDate: TDateTimePicker;
    dtpLowerTime: TDateTimePicker;
    dtpUpperTime: TDateTimePicker;
    cbxAcceptDateBounds: TCheckBox;
    cbxUseDateLimits: TCheckBox;
    pnlLatex: TPanel;
    cbxvRules: TCheckBox;
    cbxhlines: TCheckBox;
    lblColumnType: TLabel;
    cbLatexColType: TComboBox;
    cbLatexEnvironment: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    procedure bAddClick(Sender: TObject);
    procedure cbDateTimeFieldChange(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxLowerBoundClick(Sender: TObject);
    procedure cbxUpperBoundClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bAddAllClick(Sender: TObject);
    procedure bUpClick(Sender: TObject);
    procedure bDownClick(Sender: TObject);
    procedure bRemoveClick(Sender: TObject);
    procedure bRemoveAllClick(Sender: TObject);
    procedure listboxavailableDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure listboxavailableDblClick(Sender: TObject);
    procedure listboxfieldsDblClick(Sender: TObject);
    procedure listboxfieldsClick(Sender: TObject);
    procedure dtpLowerDateChange(Sender: TObject);
    procedure dtpLowerTimeChange(Sender: TObject);
    procedure dtpUpperDateChange(Sender: TObject);
    procedure dtpUpperTimeChange(Sender: TObject);
    procedure cbxUseDateLimitsClick(Sender: TObject);

  private
    procedure SetAddRemoveButtons;
    procedure SetSelectedListFields;
    procedure SetUpDownButtons;
  public
    storedfields : TStringList;
    source:TDataSet;
    procedure Copysettings(bOpen,bFields,bHidden,bLowTime,bHighTime,
      bDelimitEverything,bAcceptDatebounds:boolean;
      aLowDateTime,aHighDateTime:TDateTime;sDelimiter:string;
      iPrecision:integer);
    procedure CopyLaTeXSettings(sEnvironment:string;bhLine,bVRule:boolean);
    procedure SaveSettings(var bOpen,bFields,bHidden,bLowTime,bHighTime,
      bDelimitEverything,bAcceptDateBounds:boolean;
      var aLowDateTime,aHighDateTime:TDateTime;
      var sDateTimeField,sDelimiter:string;var iPrecision:integer);
    procedure SaveLaTeXSettings(var sEnvironment:string;var bHLine,
      bVRule:boolean);
    procedure GenerateListFields;
    procedure PrepCaption(iType:integer;LaTeXEnvironment:string;
      LatexHLine,LatexVRule:boolean);
    procedure PrepFilters(sDateTimeFilterField:string;sl:TStringList);
  end;

implementation

{$R *.dfm}
// ----- TformExporter.bAddClick -----------------------------------------------
procedure TformExporter.bAddClick(Sender: TObject);

var
	i: integer;
begin
	i := listboxavailable.ItemIndex;
	if i <> -1 then
	begin
		if (listboxfields.Items.IndexOf(listboxavailable.Items[i]) = -1) then
			listboxfields.Items.AddObject(listboxavailable.Items[i],
        listboxavailable.Items.Objects[i]);
  end;
	listboxavailable.Invalidate;
  SetUpDownButtons;
end;
// ----- TformExporter.SetAddRemoveButtons -------------------------------------
procedure TformExporter.SetAddRemoveButtons;

begin
	bAdd.Enabled := (listboxavailable.ItemIndex <> -1);
	bAddAll.Enabled := listboxavailable.Items.Count > listboxfields.Items.Count;
	bRemove.Enabled := listboxfields.ItemIndex <> -1;
	bRemoveAll.Enabled := listboxfields.Items.Count > 0;
end;
// ----- TformExporter.SetSelectedListFields -----------------------------------
procedure TformExporter.SetSelectedListFields;

var
  i:integer;
  sfield:string;

begin
  listboxfields.clear;
  for i:=0 to storedfields.count-1 do
  begin
    sField := storedfields.Strings[i];
    if (Source.FieldDefs.IndexOf(sField) = -1) then
      continue;  {** skip if the stored field is missing}
    listboxfields.Items.AddObject(sField,TField(Source.FieldByName(sField)))
  end;
  listboxavailable.Invalidate;
  SetUpDownButtons;
end;
// ----- TformExporter.SetUpDownButtons ----------------------------------------
procedure TformExporter.SetUpDownButtons;

begin
  if ListboxFields.Items.Count=0 then
  begin
    bUp.Enabled := false;
    bDown.Enabled := false;
  end else if listboxfields.ItemIndex = 0 then
  begin
    bUp.Enabled := false;
    bDown.Enabled := true;
  end else if (listBoxFields.ItemIndex = listboxFields.Items.Count-1) then
  begin
    bUp.Enabled := true;
    bDown.Enabled := false;
  end else if (listboxfields.ItemIndex <> -1) then
  begin
    bUp.Enabled := true;
    bDown.Enabled := true;
  end else
  begin
    bUp.Enabled := false;
    bDown.Enabled := false;
  end;
end;
// ----- TformExporter.cbDateTimeFieldChange -----------------------------------
procedure TformExporter.cbDateTimeFieldChange(Sender: TObject);

var
  aField:TField;

begin
  aField:=Source.Fields.FindField(cbDateTimeField.Text);
  if (aField = nil) then
    exit;
  if (aField is TDateField) then
  begin
    dtpLowerTime.Enabled := false;
    dtpUpperTime.Enabled := false;
    dtpLowerTime.Time := 0.0;
    dtpUpperTime.TIme := 0.0;
  end else
  begin
    dtpLowerTime.Enabled := (true and cbxLowerBound.CHecked);
    dtpUpperTime.Enabled := (true and cbxUpperBound.Checked);
  end;
  dtpLowerDate.Enabled := (true and cbxLowerBound.CHecked);
  dtpUpperDate.Enabled := (true and cbxUpperBound.Checked);
end;
// --------- TformExporter.CopySettings ----------------------------------------
procedure TformExporter.CopySettings(bOpen,bFields,bHidden,
  bLowTime,bHighTime,bDelimitEverything,bAcceptDateBounds:boolean;
  aLowDateTime,aHighDateTime:TDateTime;sDelimiter:string;iPrecision:integer);

begin
  cbxOpenAfterExport.Checked:= bOpen;
  cbxOutputHeader.Checked := bFields;

  cbxLowerBound.Checked := bLowTime;
  cbxUpperBound.Checked := bHighTime;

  try
    dtpLowerDate.DateTime := aLowDateTime;
  except
    dtpLowerDate.DateTime := now;
  end;

  try
    dtpLowerTime.DateTime := aLowDateTime;
  except
    dtpLowerTime.DateTime := now;
  end;

  cbxAcceptDateBounds.Checked := bAcceptDateBounds;

  try
    dtpUpperDate.DateTime := aHighDateTime;
  except
    dtpUpperDate.dateTime := now;
  end;

  try
    dtpUpperTime.DateTime := aHighDateTime;
  except
    dtpUpperTime.DateTime := now;
  end;

  cbLatexColType.ItemIndex := 0;

  if (sDelimiter = '''') then
    cbDelimiter.ItemIndex := 1
  else if (sDelimiter = '"') then
    cbDelimiter.ItemIndex := 2
  else
    cbDelimiter.ItemIndex := 0;
  cbxOnlyStrings.Checked := not bDelimitEverything;
end;
// ------ TformExporter.CopyLatexSettings --------------------------------------
procedure TformExporter.CopyLatexSettings(sEnvironment:string;
  bhline,bvrule:boolean);

begin
  if sEnvironment = 'longtable' then
    cbLatexEnvironment.ItemIndex := 0
  else
    cbLatexEnvironment.Itemindex := 1;

  cbLatexColType.ItemIndex := 0;
  cbxhlines.CHecked := bHLine;
  cbxvrules.Checked := bVRule;
end;
// --------- TformExporter.SaveSettings ----------------------------------------
procedure TformExporter.SaveSettings(var bOpen,bFields,bHidden,
  bLowTime,bHighTime,bDelimitEverything,bAcceptDateBounds:boolean;
  var aLowDateTime,aHighDateTime:TDateTime;
  var sDateTimeField,sDelimiter:string;var iprecision:integer);

begin
  bOpen:=cbxOpenAfterExport.Checked;
  bFields:=cbxOutputHeader.CHecked;
  bLowTime := cbxLowerBound.Checked;
  bHighTime := cbxUpperBound.Checked;
  bAcceptDateBounds := cbxAcceptDateBounds.Checked;

  if pnlDateFiltering.Visible then
  begin
    aLowDateTime := Trunc(dtpLowerDate.DateTime) + Frac(dtpLowerTime.DateTime);
    aHighDateTime := Trunc(dtpUpperDate.DateTime) + Frac(dtpUpperTime.DateTime);
    sDateTimeField := cbDateTimeField.Text
  end else
    sDateTimeField := '';

  if cbDelimiter.ItemIndex = 1 then
    sDelimiter := ''''
  else if cbDelimiter.ItemIndex = 2 then
    sDelimiter := '"'
  else
    sDelimiter := '';

  bDelimitEverything := not cbxOnlyStrings.Checked;

  iPrecision := 15;
end;
// ------ TformExporter.SaveLatexSettings --------------------------------------
procedure TformExporter.SaveLatexSettings(var sEnvironment:string; var bHline,
  bVrule:boolean);

begin
  sEnvironment := cbLatexEnvironment.Text;
  bHline := cbxhlines.CHecked;
  bVRule := cbxvrules.CHecked;
end;
//------ TformExporter.GenerateListFields --------------------------------------
{** this generates a list of field names from the source table}
procedure TformExporter.GenerateListFields;

var
  i:integer;
  sField:string;

begin
  listboxavailable.Clear;
  listboxfields.Clear;

// loop over all the source fields
  for i:=0 to Source.FieldCount-1 do
  begin
    sField := Source.Fields[i].FieldName;
    listboxavailable.Items.AddObject(sField,TField(Source.Fields[i]));
  end;
end;

// ------ TformExporter.PrepCaption --------------------------------------------
procedure TformExporter.PrepCaption(iType:integer;LaTexEnvironment:string;
  LatexHLine,LatexVRule:boolean);

begin
  case iType of
{** CSV}
    0: begin
      Caption := 'Export to Comma Separated Value File';
      bOK.Caption := 'Save';
    end;
{** TSV}
    1 : begin
      Caption := 'Export to Tab Separated Value File';
      bOk.Caption := 'Save';
    end;
{** clipboard}
    2: begin
      Caption := 'Copy to Clipboard';
      bOk.Caption := 'Copy';
    end;
{** SSV}
    3: begin
      Caption := 'Export to Space Separated Value File';
      bOk.Caption := 'Save';
    end;
{** XML}
    4: begin
      Caption := 'Export to XML File';
      bOK.Caption := 'Save';
    end;
{** LaTeX}
    5: begin
      Caption := 'Export to LaTeX File';
      CopyLaTeXSettings(LatexEnvironment,LatexHLine,LatexVRule);
      bOk.Caption := 'Save';
    end;
  end;
end;
// ------ TformExporter.PrepFilters --------------------------------------------
procedure TformExporter.PrepFilters(sDateTimeFilterField:string;sl:TStringList);

begin
  if (sl.Count > 0) then
  begin
    pnlDateFiltering.Visible := true;
    cbDateTimeField.Items.Assign(sl);
    cbDateTimeField.ItemIndex := cbDateTimeField.Items.
      IndexOf(sDateTimeFilterField);
    cbDateTimeFieldChange(nil);
  end else
    pnlDateFiltering.Visible := false;
end;
// ----- TformExporter.bOKClick ------------------------------------------------
procedure TformExporter.bOKClick(Sender: TObject);

var
  i:integer;
  sField : string;

begin
  storedFields.Clear;
  for i:=0 to listboxfields.Items.Count-1 do
  begin
    sfield := TField(listboxfields.Items.Objects[i]).FieldName;
    storedfields.Add(sField);
  end;
end;

procedure TformExporter.FormCreate(Sender: TObject);
begin
  storedFields := TStringList.Create;
end;

procedure TformExporter.FormDestroy(Sender: TObject);
begin
  storedfields.Free;
end;

procedure TformExporter.FormShow(Sender: TObject);
begin
  GenerateListFields;
  SetSelectedListFields;
  SetUpDownButtons;
  listboxAvailable.SetFocus;
end;

procedure TformExporter.cbxLowerBoundClick(Sender: TObject);
begin
  cbDateTimeFieldCHange(nil);
end;

procedure TformExporter.cbxUpperBoundClick(Sender: TObject);
begin
  cbDateTimeFieldChange(nil);
end;
// ----- TformExporter.FormClose -----------------------------------------------
procedure TformExporter.FormClose(Sender: TObject;
  var Action: TCloseAction);

var
  low,high : TDateTime;

begin
{** check for the dates}
  if ((ModalResult = mrOK) and cbxLowerBound.Checked and cbxUpperBound.Checked) then
  begin
    low := dtpLowerDate.Date + dtpLowerTime.Time;
    high := dtpUpperDate.Date + dtpUpperTime.Time;
    if (low >= high) then
    begin
      MessageDlg('You selected bounded date-time filtering.'+
        'The lower Bound must '+#13+#10+'be less than the upper bound.'+
        'Please correct!', mtWarning, [mboK], 0);
      Action := caNone;
    end;
  end;
end;
// ----- TformExporter.bAddAllClick --------------------------------------------
procedure TformExporter.bAddAllClick(Sender: TObject);

var
  i:integer;

begin
// don't add any in the sort fields
  listboxfields.Items.Clear;
  for i:=0 to listboxavailable.Items.Count-1 do
    if (listboxfields.Items.IndexOf(listboxavailable.Items.Strings[i])=-1) then
      listboxfields.Items.AddObject(listboxavailable.Items.Strings[i],
        listboxavailable.Items.Objects[i]);
	listboxavailable.Invalidate;
	SetAddRemoveButtons;
  SetUpDownButtons;
end;

procedure TformExporter.bUpClick(Sender: TObject);
begin
  listBoxFields.Items.Exchange(listboxfields.ItemIndex,listboxfields.ItemIndex-1);
  SetUpDownButtons;
end;

procedure TformExporter.bDownClick(Sender: TObject);
begin
  listBoxFields.Items.Exchange(listboxfields.ItemIndex,listboxfields.ItemIndex+1);
  SetUpDownButtons;
end;

procedure TformExporter.bRemoveClick(Sender: TObject);

var
	i: integer;

begin
	i := listboxfields.ItemIndex;
	if i <> -1 then
		listboxfields.Items.Delete(i);

	listboxavailable.Invalidate;
	SetAddRemoveButtons;
  SetUpDownButtons;
end;

procedure TformExporter.bRemoveAllClick(Sender: TObject);
begin
	listboxfields.Items.Clear;
	listboxavailable.Invalidate;
	SetAddRemoveButtons;
  SetUpDownButtons;
end;

procedure TformExporter.listboxavailableDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
	with (Control as TListBox).Canvas do
	begin
		FillRect(Rect);
		if not (odSelected in State) then
			if (listboxFields.Items.IndexOf(listboxavailable.Items[Index]) <> -1) then
				Font.Color := clMaroon
			else
				Font.Color := clBlack;
		TextOut(Rect.Left, Rect.Top, (Control as TListBox).Items[Index]);
	end;
end;

procedure TformExporter.listboxavailableDblClick(Sender: TObject);
begin
  bAddClick(nil);
end;

procedure TformExporter.listboxfieldsDblClick(Sender: TObject);
begin
  bRemoveClick(nil);
end;

procedure TformExporter.listboxfieldsClick(Sender: TObject);
begin
  SetAddRemoveButtons;
  SetUpdownButtons;
end;

procedure TformExporter.dtpLowerDateChange(Sender: TObject);
begin
  cbxUseDateLimits.Checked := false;
end;

procedure TformExporter.dtpLowerTimeChange(Sender: TObject);
begin
  cbxUseDateLimits.Checked :=false;
end;

procedure TformExporter.dtpUpperDateChange(Sender: TObject);
begin
  cbxUseDateLimits.Checked := false;
end;

procedure TformExporter.dtpUpperTimeChange(Sender: TObject);
begin
  cbxUseDateLimits.Checked := false;
end;
// ----- TformExporter.cbxUseDateLimitsClick -----------------------------------
procedure TformExporter.cbxUseDateLimitsClick(Sender: TObject);

var
  aField:TField;
  mindt,maxdt,currdt : TDateTime;

begin
  if (not cbxUseDateLimits.Checked) then
    exit;

  aField := Source.Fields.FindField(cbDateTimeField.Text);
  if (aField = nil) or (Source.RecordCount = 0) then
    exit;

  Source.DisableControls;
  Source.First;
  mindt := aField.AsDateTime;
  maxdt := aField.AsDateTime;

  while not (Source.eof) do
  begin
    currdt := Source.FieldByName(cbDateTimeField.Text).AsDateTime;
    if (mindt > currdt)then
      mindt := currdt;
    if (maxdt < currdt) then
      maxdt := currdt;
    Source.Next;
  end;
  Source.EnableControls;
  dtpLowerDate.DateTime := mindt;
  dtpUpperDate.DateTime := maxdt;

  dtpLowerTime.Time := Frac(mindt);
  dtpUpperTime.Time := Frac(maxdt);

  cbDateTimeFieldChange(nil);
end;

end.
