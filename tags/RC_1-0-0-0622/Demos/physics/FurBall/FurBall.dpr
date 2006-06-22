program FurBall;

uses
  Forms,
  fFurBall in 'fFurBall.pas' {frmFurBall};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFurBall, frmFurBall);
  Application.Run;
end.
