program SceletonColliderEditor;

uses
  Forms,
  fMain in 'fMain.pas' {MainForm},
  uSkeletonColliders in 'uSkeletonColliders.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Skeletal Mesh Collider Editor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
