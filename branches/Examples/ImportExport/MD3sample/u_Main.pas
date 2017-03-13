unit u_Main;

interface

uses
  Winapi.Windows,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Imaging.JPEG,

  GLWin32Viewer, GLCrossPlatform, GLBaseClasses, GLScene, GLVectorFileObjects,
  GLCoordinates, GLObjects, GLCadencer, GLRenderContextInfo, GLVectorGeometry,

  GLFileMD3, GLAsyncTimer;


type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    cad: TGLCadencer;
    dogl: TGLDirectOpenGL;
    ff: TGLFreeForm;
    at: TGLAsyncTimer;
    procedure FormCreate(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure atTimer(Sender: TObject);
    procedure cadProgress(Sender: TObject; const deltaTime, newTime: Double);
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}


//
// FormCreate
//
procedure TForm1.FormCreate;
begin

  ff.LoadFromFile('gus.md3');

end;


//
// cadProcess
//
procedure TForm1.cadProgress;
begin

  vp.Invalidate;

end;


//
// doglRender
//
procedure TForm1.doglRender;

  procedure doMorph(mol:TGLMeshObjectList; offset,spd:single);
  var t:single; c:integer;
  begin
    t := (offset + cad.CurrentTime) * spd;
    c := round(t - 0.5) mod (mol.MorphTargetCount - 1);
    mol.Lerp(c, c+1, frac(t));
    end;

begin

  ff.Position.SetPoint(-1.5, 0, 0);
  doMorph(ff.MeshObjects, 0, 25);
  ff.Render(rci);

  ff.Position.SetPoint(1.5, 0, 0);
  doMorph(ff.MeshObjects, 0.5, 50);
  ff.Render(rci);

end;


//
// atTimer
//
procedure TForm1.atTimer(Sender: TObject);
begin

  caption := vp.FramesPerSecondText(2);
  vp.ResetPerformanceMonitor;

end;

end.
