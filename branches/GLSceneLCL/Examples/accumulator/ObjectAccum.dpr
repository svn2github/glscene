program ObjectAccum;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  Unit1 in 'Unit1.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Accumulator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
