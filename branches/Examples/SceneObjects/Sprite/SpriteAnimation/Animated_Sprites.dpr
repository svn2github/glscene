program Animated_Sprites;

uses
  Forms,
  mainUn in 'mainUn.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
