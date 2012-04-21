program OES;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uDev in 'uDev.pas' {GLMainForm5: TGLSceneForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLMainForm5, GLMainForm5);
  Application.Run;
end.
