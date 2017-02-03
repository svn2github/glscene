program FractalArchipelago;

uses
  Forms,
  dlgFracArchipU in 'dlgFracArchipU.pas' {dlgFracArchip},
  ahGLrandomHDS in '..\ahGLrandomHDS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdlgFracArchip, dlgFracArchip);
  Application.Run;
end.
