program AnimatedTexture;

uses
  Forms,
  uMain in 'uMain.pas' {Main},
  OffSetAnim in 'OffSetAnim.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
