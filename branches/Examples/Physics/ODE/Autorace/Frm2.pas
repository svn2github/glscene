unit Frm2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFrmMenu = class(TForm)
    RBlue: TRadioButton;
    RGreen: TRadioButton;
    RRed: TRadioButton;
    RYellow: TRadioButton;
    RSpecial1: TRadioButton;
    RSpecial2: TRadioButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
     
  public
     
  end;

var
  FrmMenu: TFrmMenu;

implementation

uses Unit1;

{$R *.dfm}

procedure TFrmMenu.Button2Click(Sender: TObject);
begin
  Frm.close;
end;

procedure TFrmMenu.Button1Click(Sender: TObject);
begin
  if RBlue.Checked then
    Modelcar:= 1
  else
  if RGreen.Checked then
    Modelcar:= 2
  else
  if RRed.Checked then
    Modelcar:= 3
  else
  if RYellow.Checked then
    Modelcar:= 4
  else
  if RSpecial1.Checked then
    Modelcar:= 5
  else
  if RSpecial2.Checked then
    Modelcar:= 6;
  Frm.SetResolution(0,0);  //0,0 Default windows screen size.
  Frm.Ode:= TOde.create;
  Frm.Ode.Loadmap('track02.3DS');
  Frm.Car:= TCars.create;
  Frm.GLCadencer.Enabled:= True;
  close;
end;

end.
