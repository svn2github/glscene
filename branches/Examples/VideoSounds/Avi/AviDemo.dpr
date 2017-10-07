program AviDemo;

uses
  Forms,
  main in 'main.pas' {Form1},
  glAvi in 'glAvi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
