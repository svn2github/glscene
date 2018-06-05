{
  Here we try to use the results of Conrec to draw isolines in a XY plane
  formed by a GLscene TGLfreeform.
  Some variables are declared but never used in the demo
  More information in forum http://sourceforge.net/p/glscene/discussion/93606/
  Sugestions are welcome sergiofeitoza@cognitor.com.br
}

program Isolines;

uses
  Forms,
  ABOUT in 'ABOUT.PAS' {AboutBox},
  LevelCurves in 'LevelCurves.pas' {LevelCurvesForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TLevelCurvesForm, LevelCurvesForm);
  Application.Run;
end.
 
