unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLTeapot, GLObjects, GLMisc, GLWin32Viewer, OpenGL1x,
  VectorGeometry, GLTexture, GLCadencer, GLImposter, GLPolyhedron,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLSprite1: TGLSprite;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    GLDodecahedron1: TGLDodecahedron;
    Timer1: TTimer;
    Label1: TLabel;
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    imposter : TImposter;
    impBuilder : TGLStaticImposterBuilder;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   impBuilder:=TGLStaticImposterBuilder.Create(Self);
   impBuilder.Coronas.Items[0].Samples:=256;
   impBuilder.SampleSize:=128;
   imposter:=impBuilder.CreateNewImposter;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TRenderContextInfo);
var
   camPos : TVector;
begin
   if imposter.Texture.Handle=0 then begin
      impBuilder.Render(rci, GLTeapot1, GLSceneViewer1.Buffer, imposter);
      Caption:=Format('%d x %d', [impBuilder.TextureSize.X, impBuilder.TextureSize.Y]);
   end;

   camPos:=XHmgVector;
   RotateVector(camPos, YHmgVector, GLCadencer1.CurrentTime*30*cPIdiv180);

   imposter.BeginRender(rci);
   imposter.Render(rci, NullHmgPoint, camPos, 2);
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

end.
