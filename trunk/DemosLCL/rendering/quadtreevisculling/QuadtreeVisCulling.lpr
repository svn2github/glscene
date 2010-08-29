program QuadtreeVisCulling;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fQuadtreeVisCulling in 'fQuadtreeVisCulling.pas' {frmQuadtreeVisCulling};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQuadtreeVisCulling, frmQuadtreeVisCulling);
  Application.Run;
end.
