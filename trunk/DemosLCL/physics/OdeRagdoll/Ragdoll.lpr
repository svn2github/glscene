program Ragdoll;

{$MODE Delphi}

uses
  Forms, Interfaces,
  fRagdoll in 'fRagdoll.pas', GLScene_ODE {frmRagdoll};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmRagdoll, frmRagdoll);
  Application.Run;
end.
