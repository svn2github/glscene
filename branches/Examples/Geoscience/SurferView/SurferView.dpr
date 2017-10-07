{ -----------------------------------------------------------------------------
  Unit Name: main
  Author:    Aaron Hochwimmer (hochwimmera@pbworld.com)
  Purpose:   Simple demo for viewing Surfer Grid files
  ----------------------------------------------------------------------------- }

program surferview;

uses
  Forms,
  main in 'main.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
