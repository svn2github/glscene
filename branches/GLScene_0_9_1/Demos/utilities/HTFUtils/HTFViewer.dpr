program HTFViewer;

uses
  Forms,
  FViewerForm in 'FViewerForm.pas' {ViewerForm},
  FNavForm in 'FNavForm.pas' {NavForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TViewerForm, ViewerForm);
  Application.CreateForm(TNavForm, NavForm);
  Application.Run;
end.
