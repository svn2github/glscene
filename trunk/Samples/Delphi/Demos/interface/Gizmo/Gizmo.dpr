{: GLGizmo component demo.
  Version History:
  29/09/2007 - DaStr - Initial version.
}
program Gizmo;

uses
  Forms,
  DemoGizmoForm in 'DemoGizmoForm.pas' {Form1};

{$R *.res}

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

