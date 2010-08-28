Program DemoGizmoEx;

{$MODE Delphi}

uses
  Forms, Interfaces,
  DemoGizmoForm in 'DemoGizmoForm.pas' {Form1};

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

