unit MainU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdCoder, IdCoder3to4, IdCoderMIME, StdCtrls;
type
  TMainFrm = class(TForm)
    TestLoadMap: TButton;
    OD: TOpenDialog;
    D: TIdDecoderMIME;
    procedure TestLoadMapClick(Sender: TObject);
    procedure FillFreeFormArray(filename: string; Decoder: TIdDecoderMIME);
    procedure LoadTerain;
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;


type
  TArrayOfComp = array of TLabel;
  TFFrm = record
    X: integer;
    Y: integer;
    Z: integer;
    sX: integer;
    sY: integer;
    sZ: integer;
    ModelName: string;
  end;

var
  MainFrm: TMainFrm;
  Objects: array of TFFrm;
  Comp: TArrayOfComp;
implementation

{$R *.dfm}

procedure TMainFrm.FillFreeFormArray(filename: string; Decoder: TIdDecoderMIME);

var
  Cord: array[1..6] of integer;

  procedure DivString(s: string);
  var cnt: byte;
  begin
    cnt := 0;
    while (s <> '') and (pos(' ', s) <> 0) do
    begin
      inc(cnt);
      Cord[cnt] := strtoint(copy(s, 1, pos(' ', s) - 1));
      delete(s, 1, pos(' ', s));
    end;
    if s <> '' then
    begin
      inc(cnt);
      Cord[cnt] := strtoint(s);
    end;
  end;

  procedure ClearList;
  var i:integer;
  begin
    for i:=0 to length(Comp)-1 do
    Comp[i].Free
  end;
var
  f: textfile;
  tmp: string;
  n, i: integer;

begin
  ClearList;
  assignfile(f, filename);
  reset(f);
  Readln(f, tmp);
  n := StrToInt(Decoder.DecodeToString(tmp));
  SetLength(Objects, n);
  for i := 0 to n - 1 do
  begin
    Readln(f, tmp);
    tmp := Decoder.DecodeToString(tmp);
    DivString(tmp);
    Objects[i].X := Cord[1];
    Objects[i].Y := Cord[2];
    Objects[i].Z := Cord[3];
    Objects[i].sX := Cord[4];
    Objects[i].sY := Cord[5];
    Objects[i].sZ := Cord[6];
    Readln(f, tmp);
    tmp := Decoder.DecodeToString(tmp);
    Objects[i].ModelName := tmp;
  end;
  CloseFile(f);
  LoadTerain;
end;

procedure TMainFrm.LoadTerain;
var i: integer;
begin
  SetLength(Comp, length(Objects));
  for i := 0 to length(Objects) - 1 do // Создание объектов
  begin
    Comp[i] := Tlabel.Create(Self);
    Comp[i].Parent := Self;
    Comp[i].Left := Objects[i].X;
    Comp[i].Top := Objects[i].Y;
    Comp[i].Width := Objects[i].sX;
    Comp[i].Height := Objects[i].sY;
    Comp[i].Caption := Objects[i].ModelName;
  end;
end;

procedure TMainFrm.TestLoadMapClick(Sender: TObject);
begin
  if OD.Execute then
    FillFreeFormArray(OD.FileName, D);
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  OD.InitialDir:=ExtractFilePath(application.ExeName);
end;

end.

