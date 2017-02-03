program DuneFighter;

uses
  Forms,
  fDuneFighterU in 'fDuneFighterU.pas' {fDuneFighter},
  ahGLrandomHDS in '..\ahGLrandomHDS.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfDuneFighter, fDuneFighter);
  Application.Run;
end.
