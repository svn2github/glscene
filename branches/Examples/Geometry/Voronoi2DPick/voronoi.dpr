program voronoi;

uses
  Forms,
  main in 'main.pas' {Form1},
  GraphObjects in 'GraphObjects.pas',
  voro in 'voro.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
