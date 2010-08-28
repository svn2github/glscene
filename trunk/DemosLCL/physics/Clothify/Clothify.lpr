program Clothify;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fClothify in 'fClothify.pas', GLScene_ODE {frmClothify};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClothify, frmClothify);
  Application.Run;
end.
