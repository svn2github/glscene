unit frmSort;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TformSort = class(TForm)
    bCancel: TBitBtn;
    bOk: TBitBtn;
    bv1: TBevel;
    bv2: TBevel;
    bv3: TBevel;
    cbField1: TComboBox;
    cbField2: TComboBox;
    cbField3: TComboBox;
    cbxCIField1: TCheckBox;
    cbxCIField2: TCheckBox;
    cbxCIField3: TCheckBox;
    cbxDescField1: TCheckBox;
    cbxDescField2: TCheckBox;
    cbxDescField3: TCheckBox;
    cbxINField1: TCheckBox;
    cbxINField2: TCheckBox;
    cbxINField3: TCheckBox;
    lblSortField1: TLabel;
    lblSortField2: TLabel;
    lblSortField3: TLabel;
    pnlTop: TPanel;
    pnlBottom: TPanel;
  private
     
  public
    procedure LoadFields(sl:TStringList);
    function SortString:string;
  end;

implementation

{$R *.dfm}
// ----- TformSort.LoadFields --------------------------------------------------
procedure TformSort.LoadFields(sl:TStringList);

var
  i:integer;

begin
  cbField1.Items.Clear;
  cbField1.Items.Add('');
  cbField2.Items.Clear;
  cbField2.Items.Add('');
  cbField3.Items.Clear;
  cbField3.Items.Add('');
  for i:=0 to sl.Count-1 do
  begin
    cbField1.Items.Add(sl.Strings[i]);
    cbField2.Items.Add(sl.Strings[i]);
    cbField3.Items.Add(sl.Strings[i]);
  end;
  cbField1.ItemIndex := 0;
  cbField2.ItemIndex := 0;
  cbField3.ItemIndex := 0;
end;
// ----- TformSort.SortString --------------------------------------------------
// The normal syntax is: fldname1;fldname2;fldname3...
// The enhanced syntax allows adding a colon and some options to each of
// the fieldnames.
// The following options are recognized:
//   C for Case insensitive,
//   D for Descending,
//   N for ignore Nulls,
function TformSort.SortString:string;

var
  sTemp,sSuffix,s1,s2,s3 : string;

begin
  s1 := cbField1.Text;
  s2 := cbField2.Text;
  s3 := cbField3.Text;
  sTemp := '';

{** for field 1}
  if (cbField1.Text <> '') then
  begin
    sTemp := s1;
    sSuffix := '';
    if cbxDescField1.Checked then
      sSuffix := sSuffix + 'D';
    if cbxCIField1.Checked then
      sSuffix := sSuffix + 'C';
    if cbxINField1.Checked then
      sSuffix := sSuffix + 'N';
    if (sSuffix <> '') then
      sTemp := sTemp + ':' + sSuffix;
    sTemp := sTemp + ';';
  end;

{** for field 2}
  if (cbField2.Text <> '') then
  begin
    sTemp := s2;
    sSuffix := '';
    if cbxDescField2.Checked then
      sSuffix := sSuffix + 'D';
    if cbxCIField2.Checked then
      sSuffix := sSuffix + 'C';
    if cbxINField2.Checked then
      sSuffix := sSuffix + 'N';
    if (sSuffix <> '') then
      sTemp := sTemp + ':' + sSuffix;
    sTemp := sTemp + ';';
  end;

{** for field 3}
  if (cbField3.Text <> '') then
  begin
    sTemp := sTemp + s2;
    sSuffix := '';
    if cbxDescField3.Checked then
      sSuffix := sSuffix + 'D';
    if cbxCIField3.Checked then
      sSuffix := sSuffix + 'C';
    if cbxINField3.Checked then
      sSuffix := sSuffix + 'N';
    if (sSuffix <> '') then
      sTemp := sTemp + ':' + sSuffix;
    sTemp := sTemp + ';';
  end;
  result := sTemp;
end;
// =============================================================================
end.






