// Demo application for Voro+GraphObjects

unit main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls,
  GraphObjects,
  GLMaterial,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  GLBaseClasses,
  GLScene,
  Voro;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    PaintBox1: TPaintBox;
    Button1: TButton;
    Label1: TLabel;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  List: TList;
  Vor: TVoronoi;
  mx, my, i: Integer;
  down: boolean;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  z: Integer;
begin
  PaintBox1.Canvas.Brush.Color := clBlack;
  PaintBox1.Canvas.FillRect(Rect(0, 0, 1200, 1000));
  if List = nil then
    exit;
  PaintBox1.Canvas.Font.Color := clWhite;
  for z := 0 to List.Count - 1 do
  begin
    TGraphObject(List.items[z]).draw;
    if (TObject(List.items[z]) is TGPoint) and CheckBox3.Checked then
      PaintBox1.Canvas.TextOut(round(TGPoint(List.items[z]).GetX + 1),
        round(TGPoint(List.items[z]).GetY + 1), inttostr(z));
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  mx := X;
  my := Y;
  Form1.Caption := inttostr(mx) + ' ' + inttostr(my);
  if down then
  begin
    TGPoint(List.items[i]).MoveTo(X, Y);
    CheckBox1Click(Form1);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  List := TList.Create;
  down := false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  List.Clear;
  PaintBox1.OnPaint(Form1);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Vor := TVoronoi.Create(PaintBox1.Canvas, List);
  Vor.ClearLines;
  Vor.CalcVoronoi(CheckBox1.Checked, CheckBox2.Checked);
  Vor.Free;
  PaintBox1.OnPaint(Form1);

end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TGPoint;
  z: Integer;
begin
  p := TGPoint.Create(X, Y, nil, nil);
  p.closeDist := 5;
  for z := 0 to List.Count - 1 do
  begin
    if TObject(List.items[z]) is TGPoint then
      if p.Match(TGPoint(List.items[z])) then
      begin
        if (Button = TMouseButton.mbRight) then
          TGPoint(List.items[z]).Delete(true)
        else
        begin
          down := true;
          i := z;
        end;
        CheckBox1Click(Form1);
        exit;
      end;
  end;
  p.Free;

  TGPoint.Create(X, Y, List, PaintBox1.Canvas);
  CheckBox1Click(Form1);

end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down := false;
end;

end.
