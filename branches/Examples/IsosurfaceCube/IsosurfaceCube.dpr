{
The project to visualize isosurfaces

Input Dataset:  SampleCube.vol
---------------
Binary Char Data Set
27x27x27
cube size 15x15x15, isovalue of 128

Processing steps
----------------
1. Enter size of data
2. Load data
3. Enter IsoValue
4. Extract IsoSurface
5. Display

implemented by Wolf Blecher 2004. (Blechwolf@myrealbox.com)
and updated for RAD Studio 10 by PW
}

program IsosurfaceCube;

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
