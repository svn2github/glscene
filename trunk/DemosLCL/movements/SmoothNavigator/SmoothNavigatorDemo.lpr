program SmoothNavigatorDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uMainForm in 'uMainForm.pas', glscene_runtime {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
