program Clothify;

uses
  Forms,
  fClothify in 'fClothify.pas' {frmClothify};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClothify, frmClothify);
  Application.Run;
end.
