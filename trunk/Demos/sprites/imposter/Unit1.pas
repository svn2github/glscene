unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLTeapot, GLObjects, GLMisc, GLWin32Viewer, OpenGL1x,
  VectorGeometry, GLTexture, GLCadencer, GLImposter, GLPolyhedron,
  StdCtrls, ExtCtrls, GLSkydome;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    GLDodecahedron1: TGLDodecahedron;
    Timer1: TTimer;
    Label1: TLabel;
    GLSkyDome1: TGLSkyDome;
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    imposter : TImposter;
    impBuilder : TGLStaticImposterBuilder;
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   impBuilder:=TGLStaticImposterBuilder.Create(Self);
   impBuilder.SampleSize:=256;
   impBuilder.SamplingRatioBias:=1.3;
   impBuilder.Coronas.Items[0].Samples:=32;
   impBuilder.Coronas.Add(15, 24);
   impBuilder.Coronas.Add(30, 24);
   impBuilder.Coronas.Add(45, 12);
   impBuilder.Coronas.Add(60, 12);
   impBuilder.Coronas.Add(75, 12);
   impBuilder.Coronas.Add(89, 12);
   imposter:=impBuilder.CreateNewImposter;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TRenderContextInfo);
var
   camPos : TVector;
begin
   if imposter.Texture.Handle=0 then begin
      impBuilder.Render(rci, GLTeapot1, GLSceneViewer1.Buffer, imposter);
      Caption:=Format('%d x %d - %.1f%%',
                      [impBuilder.TextureSize.X, impBuilder.TextureSize.Y,
                       impBuilder.TextureFillRatio*100]);
   end;

//   camPos:=XHmgVector;
//   RotateVector(camPos, YHmgVector, GLCadencer1.CurrentTime*30*cPIdiv180);
   camPos:=GLTeapot1.AbsoluteToLocal(GLCamera1.AbsolutePosition);

   imposter.BeginRender(rci);
   imposter.Render(rci, NullHmgPoint, camPos, 5*0.75);
   imposter.EndRender(rci); 
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Label1.Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   end;
   mx:=x; my:=y;
end;

end.
