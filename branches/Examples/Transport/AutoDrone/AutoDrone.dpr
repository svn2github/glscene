   {Demo of dynamic path planning using potential functions
   History:
      19.08.04 - Max - Creation;
      05.05.06 - Max - Some fixes;
   }
program AutoDrone;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;                                     
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

