program GuiEditor;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MainFormUnit in 'MainFormUnit.pas', glscene_designtime {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
