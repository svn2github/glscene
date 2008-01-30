// THIS DEMO IS PART OF THE GLSCENE PROJECT
// by Marcus Oblak
unit Unit1;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLProxyObjects, GLVectorFileObjects, GLObjects, GLMisc,
  GLCadencer, GLTexture, GLWin32Viewer, GLGeomObjects,GLFileSMD,JPEG, ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    InvisibleDummyCube: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    MasterActor: TGLActor;
    GLActorProxy1: TGLActorProxy;
    GLActorProxy2: TGLActorProxy;
    GLArrowLine1: TGLArrowLine;
    GLLightSource1: TGLLightSource;
    GLArrowLine2: TGLArrowLine;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
   i:integer;
begin
     MasterActor.LoadFromFile('..\..\media\TRINITYrage.smd');
     MasterActor.AddDataFromFile('..\..\media\run.smd');
     MasterActor.AddDataFromFile('..\..\media\jump.smd');

     MasterActor.Animations.Items[0].Name:='still';
     MasterActor.Animations.Items[1].Name:='walk';
     MasterActor.Animations.Items[2].Name:='jump';

     for i := 0 to MasterActor.Animations.Count-1 do
     begin
          MasterActor.Animations[i].MakeSkeletalTranslationStatic;
          MasterActor.SwitchToAnimation(i); // forces animations to be initialized for ActorsProxies
     end;
     MasterActor.SwitchToAnimation(0);   // revert back to empty animation (not necessary)
     MasterActor.AnimationMode:=aamLoop; // animationmode is shared between proxies.

     GLActorProxy1.StoreBonesMatrix:=true;
     GLActorProxy2.StoreBonesMatrix:=true;


     GLActorProxy1.Animation := MasterActor.Animations[1].Name;
     GLActorProxy2.Animation := MasterActor.Animations[2].Name;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     // Align object to hand
     GLArrowLine1.Matrix := GLActorProxy1.BoneMatrix('Bip01 R Finger1');
     GLArrowLine2.Matrix := GLActorProxy2.BoneMatrix('Bip01 R Finger1');

     // turn actors
     GLActorProxy2.Turn(deltaTime *100);
     GLActorProxy1.Turn(-deltaTime *130);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     Caption:=GLSceneViewer1.FramesPerSecondText(0);
     GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
