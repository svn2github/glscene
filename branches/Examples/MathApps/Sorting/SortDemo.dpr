program SortDemo;

uses
  Forms,
  Main in 'Main.pas' {Master},
  Sorts in 'Sorts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Extended Button Demo';
  Application.CreateForm(TMaster, Master);
  Application.Run;
end.
