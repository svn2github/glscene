unit Unit1;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  //GLS
  GLScene, GLObjects, GLCadencer, GLWin32Viewer, GLShadowVolume,
  GLVectorFileObjects, GLFileSMD, GLTexture,
  GLGeomObjects, GLSilhouette, GLVectorGeometry, GLMaterial, GLCoordinates,
  GLCrossPlatform, GLSimpleNavigation, GLBaseClasses;

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
    GLCube1: TGLCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCylinder1: TGLCylinder;
    GLSphere4: TGLSphere;
    GLSphere_Shadow: TGLSphere;
    Label2: TLabel;
    ScrollBar_ShadowResolution: TScrollBar;
    Button_GenerateSilhouette: TButton;
    GLLines1: TGLLines;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBShowVolumesClick(Sender: TObject);
    procedure RBZFailClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBMainLightClick(Sender: TObject);
    procedure CBBlueLightClick(Sender: TObject);
    procedure CBRedLightClick(Sender: TObject);
    procedure ScrollBar_ShadowResolutionChange(Sender: TObject);
    procedure Button_GenerateSilhouetteClick(Sender: TObject);
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

   MediaPath : String;
   I : Integer;
begin
  MediaPath := ExtractFilePath(ParamStr(0));
  I := Pos('Samples', MediaPath);
  if (I <> 0) then
  begin
    Delete(MediaPath, I+8, Length(MediaPath)-I); //or MediaPath := Copy(MediaPath,1,I+7);
    SetCurrentDir(MediaPath+'Media\');
  end;
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
            GLShadowVolume.Occluders.AddCaster(sphere, 0, scmParentVisible);
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
   if Shift=[ssLeft] then begin
      GLCamera.MoveAroundTarget((my-y)*0.5, (mx-x)*0.5);
      GLCadencer1.Progress;
   end else if Shift=[ssRight] then begin
      DCLight1Turn.Turn((mx-x)*0.5);
      DCLight1Pitch.Pitch((my-y)*0.5);
      GLLightSource1.TransformationChanged;
      GLCadencer1.Progress;
   end;
   mx:=x; my:=y;
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

procedure TForm1.ScrollBar_ShadowResolutionChange(Sender: TObject);
begin
  GLSphere_Shadow.Stacks := ScrollBar_ShadowResolution.Position;
  GLSphere_Shadow.Slices := ScrollBar_ShadowResolution.Position;
  GLShadowVolume.FlushSilhouetteCache;
end;

procedure TForm1.Button_GenerateSilhouetteClick(Sender: TObject);
var
  silhouetteParameters : TGLSilhouetteParameters;
  Silhouette : TGLSilhouette;
  i : integer;
  Target : TGLSceneObject;
begin
  Target := GLSphere4;

  silhouetteParameters.CappingRequired := false;
  SetVector(silhouetteParameters.SeenFrom,
    GLLines1.AbsoluteToLocal(GLCamera.AbsolutePosition));

  silhouetteParameters.Style := ssOmni;

  Silhouette := Target.GenerateSilhouette(silhouetteParameters);

  GLLines1.Nodes.Clear;

  for i := 0 to Silhouette.Indices.Count-1 do
    GLLines1.Nodes.AddNode(GLLines1.AbsoluteToLocal(Target.LocalToAbsolute(Silhouette.Vertices[Silhouette.Indices[i]])));

  FreeAndNil(Silhouette);
end;

end.
