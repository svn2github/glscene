program tiles;

uses
  Forms,
  u_Main in 'u_Main.pas' {Form1},
  u_Map in 'u_Map.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
