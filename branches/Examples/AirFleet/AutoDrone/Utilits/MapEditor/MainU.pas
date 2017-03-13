unit MainU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, IdCoder, IdCoder3to4, IdCoderMIME, IdBaseComponent,
  StdCtrls;

type
  TMainFrm = class(TForm)
    Encoder: TIdEncoderMIME;
    Decoder: TIdDecoderMIME;
    OD: TOpenDialog;
    SD: TSaveDialog;
    MM: TMainMenu;
    BtnFile: TMenuItem;
    BtnOpen: TMenuItem;
    BtnSave: TMenuItem;
    none2: TMenuItem;
    BtnExit: TMenuItem;
    BtnSaveAs: TMenuItem;
    none1: TMenuItem;
    BtnClose: TMenuItem;
    Field: TMemo;
    BtnSet: TMenuItem;
    FD: TFontDialog;
    BtnFont: TMenuItem;
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure FieldChange(Sender: TObject);
  private
     
  public
     
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

procedure TMainFrm.BtnOpenClick(Sender: TObject);
var i: integer;
begin
  if OD.Execute then
  begin
    Field.Lines.LoadFromFile(OD.FileName);
    for i := 0 to Field.Lines.Count - 1 do
      Field.Lines.Strings[i] := Decoder.DecodeToString(Field.Lines.Strings[i]);
    BtnClose.Enabled:=true;
    BtnSave.Enabled:=true;
  end;
end;

procedure TMainFrm.BtnSaveClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to Field.Lines.Count - 1 do
    Field.Lines.Strings[i] := Encoder.Encode(Field.Lines.Strings[i]);
  Field.Lines.SaveToFile(OD.FileName);
  for i := 0 to Field.Lines.Count - 1 do
    Field.Lines.Strings[i] := Decoder.DecodeToString(Field.Lines.Strings[i]);
end;

procedure TMainFrm.BtnSaveAsClick(Sender: TObject);
var i: integer;
begin
  if SD.Execute then
  begin
    for i := 0 to Field.Lines.Count - 1 do
      Field.Lines.Strings[i] := Encoder.Encode(Field.Lines.Strings[i]);
    Field.Lines.SaveToFile(SD.FileName);
    for i := 0 to Field.Lines.Count - 1 do
      Field.Lines.Strings[i] := Decoder.DecodeToString(Field.Lines.Strings[i]);
  end;
end;

procedure TMainFrm.BtnFontClick(Sender: TObject);
begin
if FD.Execute then Field.Font:=FD.Font;
end;

procedure TMainFrm.FieldChange(Sender: TObject);
begin
if (Field.Lines.Count<>0)and (Field.Lines.Strings[0]<>'') then
  BtnSaveAs.Enabled:=true
else BtnSaveAs.Enabled:=false
end;

end.

