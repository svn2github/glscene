{: A simple ODE-based game of a ball rolling on a plane you can incline.<p>

   Under construction, missing:
   - main screen and time/score charts
   - levels and levels ordering
   - additionnal structures

   Eric Grange (egrange@glscene.org)
   http://glscene.org
}
program TheBall;

uses
  Forms, Dialogs, GLCrossPlatform,
  FMain in 'FMain.pas' {Main},
  UTheBallStructures in 'UTheBallStructures.pas';

{$R *.res}

begin
   if GetCurrentColorDepth<24 then begin
      ShowMessage( 'Your current desktop color depth is below 24 bits,'#13#10
                  +'this may affect performance of this demo.');
   end;

  Application.Initialize;
  Application.Title := 'TheBall';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
