program Christmas;

uses
  Forms, GLSound,
  FMain in 'FMain.pas' {Main};

{$E scr}

{$R *.res}

begin
   // don't complain about missing sound support
   vVerboseGLSMErrors:=False;
   Application.Initialize;
   Application.Title := 'GLScene Christmas 2002';
   Application.CreateForm(TMain, Main);
   Application.Run;
end.
