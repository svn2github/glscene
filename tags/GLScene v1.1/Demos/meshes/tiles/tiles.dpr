program tiles;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  GLTilePlane in '..\..\..\Source\GLTilePlane.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
