program FurBall;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fFurBall in 'fFurBall.pas', GLScene_ODE {frmFurBall};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFurBall, frmFurBall);
  Application.Run;
end.
