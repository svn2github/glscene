{
The program to visualize isosurfaces

Hystory:
- Updated interpolation mode by Chris Rorden 2016
- Converted to RAD Studio 10 by Pavel Vassiliev 2015
- Initial version was implemented by Wolf Blecher 2004.
}

program IsosurfaceSphere;

uses
  Forms,
  FMain in 'FMain.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
