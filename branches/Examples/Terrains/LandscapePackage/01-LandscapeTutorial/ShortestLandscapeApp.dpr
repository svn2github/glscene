program ShortestLandscapeApp;

uses
  Forms,
  ShortestU in 'ShortestU.pas' {Form1},
  ahGLrandomHDS in '..\ahGLrandomHDS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
