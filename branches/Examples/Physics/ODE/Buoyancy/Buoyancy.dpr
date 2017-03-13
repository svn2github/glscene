program Buoyancy;

uses
  Forms,
  fBuoyancy in 'fBuoyancy.pas' {frmBuoyancy},
  StrFunctions in 'StrFunctions.PAS';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBuoyancy, frmBuoyancy);
  Application.Run;
end.
