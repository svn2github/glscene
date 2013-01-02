{$IFDEF VER200}
  {$DEFINE DELPHI2009UP}
  {$DEFINE DELPHI2007}
  {$DEFINE D2005UP}
{$ENDIF}
program oit;

uses
  Forms,
  uDemo in 'uDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF DELPHI2009UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
