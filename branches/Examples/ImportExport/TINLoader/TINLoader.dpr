/// /////////////////////////////////////////////////////////////////////
// TIN Loader - initial version by anslasax 2003.02.25
/// /////////////////////////////////////////////////////////////////////

program TINLoader;

uses
  Forms,
  fTINLoader in 'fTINLoader.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
