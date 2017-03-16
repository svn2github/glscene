unit uLauncher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uMain, ExtCtrls;

type
  TLauncherFrm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StartBtn: TButton;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure StartBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
     
  public
     
  end;

var
  LauncherFrm: TLauncherFrm;

implementation

{$R *.dfm}

procedure TLauncherFrm.StartBtnClick(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: begin
         MainForm.sw := 1024;
         MainForm.sh := 768;
       end;
    1: begin
         MainForm.sw := 800;
         MainForm.sh := 600;
       end;
    2: begin
         MainForm.sw := 640;
         MainForm.sh := 480;
       end;
    end;

  case ComboBox2.ItemIndex of
    0: begin
         MainForm.rr :=100;
       end;
    1: begin
         MainForm.rr := 85;
       end;
    2: begin
         MainForm.rr := 75;
       end;
    3: begin
         MainForm.rr := 60;
       end;
    end;

  case ComboBox3.ItemIndex of
    0: begin
         MainForm.bpp :=32;
       end;
    1: begin
         MainForm.bpp := 16;
       end;
    end;

  Panel1.Visible := true;
  LauncherFrm.Caption := 'Results';

  MainForm.Show;

  LauncherFrm.Left := Screen.Width div 2 - LauncherFrm.Width div 2;
  LauncherFrm.Top := Screen.Height div 2 - LauncherFrm.Height div 2;
end;

procedure TLauncherFrm.Timer1Timer(Sender: TObject);
begin
  if MainForm.Active then
    Panel1.Caption := 'Среднее значение FPS: ' + IntToStr(MainForm.averageFPS);
end;

end.
