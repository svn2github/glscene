program MX2b_Demo;

uses
  Forms,
  main in 'main.pas' {Form1},
  GLFileMX2 in 'GLFileMX2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
