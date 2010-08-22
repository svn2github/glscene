program Ragdoll;

uses
  Forms,
  fRagdoll in 'fRagdoll.pas' {frmRagdoll};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmRagdoll, frmRagdoll);
  Application.Run;
end.
