program GLSLBumpshaderDemo;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces,
  umainform,  GLSceneLCL_DesignTime,  GLSceneLCL_runtime;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGLSLTestForm, GLSLTestForm);
  Application.Run;
end.
