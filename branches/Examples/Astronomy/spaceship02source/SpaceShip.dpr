program SpaceShip;

uses
  Forms,
  dlgSpaceShipU in 'dlgSpaceShipU.pas' {dlgSpaceShip};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgSpaceShip, dlgSpaceShip);
  Application.Run;
end.
