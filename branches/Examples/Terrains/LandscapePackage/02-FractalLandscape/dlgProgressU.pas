unit dlgProgressU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Gauges;

type
  TdlgProgress = class(TForm)
    ggTaskProgress: TGauge;
    lblTask: TLabel;
    timTask: TTimer;
    procedure timTaskTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
     
  public
    procedure Execute;
  end;

var
  dlgProgress: TdlgProgress;

implementation

uses dlgFracLanU;

{$R *.dfm}

procedure TdlgProgress.Execute;
begin
  Show;
  timTask.Enabled:=True;
end;

procedure TdlgProgress.timTaskTimer(Sender: TObject);
begin
  lblTask.Caption:=dlgFracLan.FractalHDS.Task;
  ggTaskProgress.Progress:=dlgFracLan.FractalHDS.TaskProgress;
end;

procedure TdlgProgress.FormHide(Sender: TObject);
begin
  timTask.Enabled:=False;
  lblTask.Caption:='';
  ggTaskProgress.Progress:=0;
end;

end.
