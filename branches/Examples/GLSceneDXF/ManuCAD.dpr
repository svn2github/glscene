program ManuCAD;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  GLDXFVectorFile in 'GLDXFVectorFile.pas',
  FileDXF in 'FileDXF.pas',
  ProgressForm in 'ProgressForm.pas' {Progress},
  TypesDXF in 'TypesDXF.pas',
  ObjectsDXF in 'ObjectsDXF.pas',
  MathsDXF in 'MathsDXF.pas',
  GLDXFRenderer in 'GLDXFRenderer.pas',
  CodesValuesDXF in 'CodesValuesDXF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TProgress, Progress);
  Application.Run;
end.
