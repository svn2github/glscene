program GLSViewer;

uses
  Forms,
  FMain in 'FMain.pas' {Main},
  GLFileSTL in '..\..\Source\FileFormats\GLFileSTL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GLSViewer';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
