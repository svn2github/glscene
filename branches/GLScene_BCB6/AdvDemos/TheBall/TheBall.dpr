program TheBall;

uses
  Forms, Dialogs, GLScreen,
  FMain in 'FMain.pas' {Main},
  UTheBallStructures in 'UTheBallStructures.pas';

{$R *.res}

begin
   if CurrentScreenColorDepth<24 then begin
      ShowMessage( 'Your current desktop color depth is below 24 bits,'#13#10
                  +'this may affect performance of this demo.');
   end;

  Application.Initialize;
  Application.Title := 'TheBall';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
