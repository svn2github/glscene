unit uNew;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, Spin, Buttons,
   
  GLVectorGeometry;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    sp_w: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    sp_h: TSpinEdit;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
     
  public
     
  end;

var
  Form2: TForm2;

implementation

uses
  uMain;

{$R *.dfm}

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  with form1 do
  begin
    glplane1.Width := round(sp_w.Value / 50) * 2;
    glplane1.Height := round(sp_h.Value / 50) * 2;
    gr.XSamplingScale.Min := -glplane1.Width / 2;
    gr.XSamplingScale.Max := glplane1.Width / 2;
    gr.YSamplingScale.Min := -glplane1.Height / 2;
    gr.YSamplingScale.Max := glplane1.Height / 2;
    dc_world.DeleteChildren;
    cam.FocalLength := 80 / maxFloat(glplane1.Width, glplane1.Height);
  end;
  Close;
end;

end.
