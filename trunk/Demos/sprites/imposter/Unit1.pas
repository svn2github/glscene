unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLTeapot, GLObjects, GLMisc, GLWin32Viewer, OpenGL1x,
  VectorGeometry, GLTexture, GLCadencer, GLImposter, StdCtrls, ExtCtrls, GLSkydome;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLTeapot1: TGLTeapot;
    GLLightSource1: TGLLightSource;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLSkyDome1: TGLSkyDome;
    GLDummyCube1: TGLDummyCube;
    Panel1: TPanel;
    Label1: TLabel;
    CBShowTeapot: TCheckBox;
    CBShowImposter: TCheckBox;
    CBSampleSize: TComboBox;
    Label2: TLabel;
    procedure GLDirectOpenGL1Render(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBSampleSizeChange(Sender: TObject);
    procedure CBShowImposterClick(Sender: TObject);
    procedure CBShowTeapotClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    imposter : TGLImposter;
    impBuilder : TGLStaticImposterBuilder;
    renderPoint : TGLRenderPoint;
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
   renderPoint:=TGLRenderPoint(GLDummyCube1.AddNewChild(TGLRenderPoint));

   impBuilder:=TGLStaticImposterBuilder.Create(Self);
   impBuilder.SampleSize:=64;
   impBuilder.SamplingRatioBias:=1.3;
   impBuilder.Coronas.Items[0].Samples:=32;
   impBuilder.Coronas.Add(15, 24);
   impBuilder.Coronas.Add(30, 24);
   impBuilder.Coronas.Add(45, 12);
   impBuilder.Coronas.Add(60, 12);
   impBuilder.Coronas.Add(75, 12);
   impBuilder.Coronas.Add(89, 12);
   impBuilder.RenderPoint:=renderPoint;

   imposter:=TGLImposter(GLScene1.Objects.AddNewChild(TGLImposter));
   imposter.Builder:=impBuilder;
   imposter.ImpostoredObject:=GLTeapot1;

//   imposter:=impBuilder.CreateNewImposter;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TRenderContextInfo);
var
   camPos : TVector;
begin
{   if imposter.Texture.Handle=0 then begin
      impBuilder.Render(rci, GLTeapot1, imposter);
      Caption:=Format('%d x %d - %.1f%%',
                      [impBuilder.TextureSize.X, impBuilder.TextureSize.Y,
                       impBuilder.TextureFillRatio*100]);
   end;}

//   camPos:=XHmgVector;
//   RotateVector(camPos, YHmgVector, GLCadencer1.CurrentTime*30*cPIdiv180);
   camPos:=GLTeapot1.AbsoluteToLocal(GLCamera1.AbsolutePosition);

{   imposter.BeginRender(rci);
   imposter.Render(rci, NullHmgPoint, camPos, 5*0.75);
   imposter.EndRender(rci);} 
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;

   Label1.Caption:=Format('%d x %d - %.1f%%',
                          [impBuilder.TextureSize.X, impBuilder.TextureSize.Y,
                           impBuilder.TextureFillRatio*100]);

end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   end;
   mx:=x; my:=y;
end;

procedure TForm1.CBSampleSizeChange(Sender: TObject);
var
   s : Integer;
begin
   s:=StrToInt(CBSampleSize.Text);
   if (GLSceneViewer1.Width>=s) and (GLSceneViewer1.Height>=s) then
      impBuilder.SampleSize:=s
   else begin
      ShowMessage('Viewer is too small to allow rendering the imposter samples');
   end;
end;

procedure TForm1.CBShowImposterClick(Sender: TObject);
begin
   imposter.Visible:=CBShowImposter.Checked;
end;

procedure TForm1.CBShowTeapotClick(Sender: TObject);
begin
   GLTeapot1.Visible:=CBShowTeapot.Checked;
end;

end.
