program Clothify;

uses
  Forms,
  fClothify in 'fClothify.pas' {frmClothify},
  GLBaseMeshSilhouette in '..\..\..\Source\GLBaseMeshSilhouette.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClothify, frmClothify);
  Application.Run;
end.
