program basicsdl;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas', GLS_SDL {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
