program BuildMesh;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uClasses in 'uClasses.pas',
  uMap in 'uMap.pas',
  uMaterials in 'uMaterials.pas',
  uMesh in 'uMesh.pas',
  uUtils in 'uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
