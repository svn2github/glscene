unit uMainform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Types, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  //DGLE
  DGLSLog, DGLTypes, DGLCrossPlatform, dglOpenGL, DGLBaseClasses, DGLContext, DGLContextHandles, DGLState,
  DGLWin32Viewer, DGLViewer, DGLScene, DGLObjects, DGLCadencer, DGLShader, DGLMaterial;


{DEFINE GLS_LOGGING}

type
  TMainForm = class(TForm)
    DGLMaterialLibrary1: TDGLMaterialLibrary;
    DGLShaderLibrary1: TDGLShaderLibrary;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Viewer : TDGLSceneViewer;
    Scene : TDGLScene;
    Cadencer : TDGLCadencer;
    Camera : TDGLCamera;
    World : TDGLDummyCube;
    ShaderLib : TDGLShaderLibrary;
    MatLib : TDGLMaterialLibrary;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Runtime Create
  // DGLSceneViewer
  DGLSLogger.LogInfo('Create Viewer');
  Viewer:= TDGLSceneViewer.Create(self);
  Viewer.Parent := Self;
  Viewer.Align := alClient;
  Viewer.Buffer.BackgroundColor := clBlack;
  // DGLScene
  DGLSLogger.LogInfo('Create Scene');
  Scene := TDGLScene.Create(self);
  // Cadencer
  DGLSLogger.LogInfo('Create Cadencer');
  Cadencer := TDGLCadencer.Create(self);
  // DummyCube
  DGLSLogger.LogInfo('Create World DummyCube');
  World := TDGLDummyCube.CreateAsChild(Scene.Objects);
  // Camera
  DGLSLogger.LogInfo('Create Camera');
  Camera := TDGLCamera.Create(self);
  Camera.Position.SetPoint(0,10,-10);
  Camera.TargetObject := World;
  // Shader Library
  DGLSLogger.LogInfo('Create Shader Library');
  ShaderLib := TDGLShaderLibrary.Create(self);
  // MAterial Library
  DGLSLogger.LogInfo('Create Material Library');
  MatLib := TDGLMaterialLibrary.Create(self);

 // DGLSLogger.LogInfo('Set Camera to Viewer');
 // Viewer.Camera := Camera;
  DGLSLogger.DisplayLog;

end;

end.
