program GLSL_BumpEarth;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FBumpEarth};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFBumpEarth, FBumpEarth);
  Application.Run;
end.
