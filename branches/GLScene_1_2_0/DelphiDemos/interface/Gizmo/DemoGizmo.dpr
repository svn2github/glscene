Program DemoGizmo;

uses
  Forms,
  DemoGizmoForm in 'DemoGizmoForm.pas' {Form1};

{$R *.res}

Begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

