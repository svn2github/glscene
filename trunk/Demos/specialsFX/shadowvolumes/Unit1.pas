{
   This demo is under construction...

   246
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLCadencer, GLMisc, GLWin32Viewer, GLShadowVolume,
  ExtCtrls, StdCtrls, GLVectorFileObjects, GLFileSMD;

type
  TForm1 = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLShadowVolume: TGLShadowVolume;
    GLSphere1: TGLSphere;
    DCLight1Turn: TGLDummyCube;
    DCLight1Pitch: TGLDummyCube;
    GLPlane1: TGLPlane;
    GLPlane2: TGLPlane;
    GLPlane3: TGLPlane;
    Timer1: TTimer;
    Panel1: TPanel;
    CBShowVolumes: TCheckBox;
    Label1: TLabel;
    RBZFail: TRadioButton;
    RBZPass: TRadioButton;
    RBNoShadows: TRadioButton;
    RBDarkening: TRadioButton;
    DCLight2: TGLDummyCube;
    GLLightSource2: TGLLightSource;
    GLSphere2: TGLSphere;
    CBMainLight: TCheckBox;
    CBBlueLight: TCheckBox;
    DCLight3: TGLDummyCube;
    GLLightSource3: TGLLightSource;
    GLSphere3: TGLSphere;
    CBRedLight: TCheckBox;
    DCSpheres: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure CBShowVolumesClick(Sender: TObject);
    procedure RBZFailClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBMainLightClick(Sender: TObject);
    procedure CBBlueLightClick(Sender: TObject);
    procedure CBRedLightClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
const
   cSpacing = 1;
   cRadius = 0.3;
   cNb = 1;
var
   x, y, z : Integer;
   sphere : TGLSphere;
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

   // Dynamically construct an array of spheres, and make them shadow casters
   // Note that as the spheres are children of the shadowvolume component,
   // they are thus also shadow receivers. If they were created as child of
   // another object (not under the shadow volume), they would not receive
   // shadows (which can sometimes be interesting).
   for x:=-cNb to cNb do
      for y:=-cNb to cNb do
         for z:=-cNb to cNb do if (x and y and z)<>0 then begin
            sphere:=TGLSphere(DCSpheres.AddNewChild(TGLSphere));
            sphere.Position.SetPoint(x*cSpacing, y*cSpacing, z*cSpacing);
            sphere.Radius:=cRadius;
            GLShadowVolume.Occluders.AddCaster(sphere); 
         end;
   DCSpheres.MoveTo(GLShadowVolume);
   GLFreeForm.LoadFromFile('trinityrage.smd');
   GLFreeForm.BuildSilhouetteConnectivityData;
   GLShadowVolume.Occluders.AddCaster(GLFreeForm);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DCLight2.TurnAngle:=newTime*45;
   DCLight3.RollAngle:=newTime*50;
   GLSceneViewer.Invalidate;
end;

procedure TForm1.CBShowVolumesClick(Sender: TObject);
begin
   with GLShadowVolume do if CBShowVolumes.Checked then
      Options:=Options+[svoShowVolumes]
   else Options:=Options-[svoShowVolumes];
end;

procedure TForm1.RBZFailClick(Sender: TObject);
begin
   // this event handles all the radio buttons
   if RBDarkening.Checked then
      GLShadowVolume.Mode:=svmDarkening
   else if RBNoShadows.Checked then
      GLShadowVolume.Mode:=svmOff
   else begin
      GLShadowVolume.Mode:=svmAccurate;
      if RBZFail.Checked then
         GLShadowVolume.Capping:=svcAlways
      else GLShadowVolume.Capping:=svcNever;
   end;
end;

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      GLCamera.MoveAroundTarget((my-y)*0.5, (mx-x)*0.5)
   else if Shift=[ssRight] then begin
      DCLight1Turn.Turn((mx-x)*0.5);
      DCLight1Pitch.Pitch((my-y)*0.5);
      GLLightSource1.TransformationChanged;
   end;
   mx:=x; my:=y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   GLCamera.SceneScale:=GLSceneViewer.Width/450;
end;

procedure TForm1.CBMainLightClick(Sender: TObject);
begin
   GLLightSource1.Shining:=CBMainLight.Checked;
end;

procedure TForm1.CBBlueLightClick(Sender: TObject);
begin
   GLLightSource2.Shining:=CBBlueLight.Checked;
end;

procedure TForm1.CBRedLightClick(Sender: TObject);
begin
   GLLightSource3.Shining:=CBRedLight.Checked;
end;

end.
