program fullscreen;

{$MODE Delphi}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Unit1;

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.

