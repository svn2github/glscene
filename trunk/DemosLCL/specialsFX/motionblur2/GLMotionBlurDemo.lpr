program GLMotionBlurDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
