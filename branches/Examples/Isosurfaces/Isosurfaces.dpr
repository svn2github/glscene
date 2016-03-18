program Isosurfaces;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GLMarchingCubes in 'GLMarchingCubes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
