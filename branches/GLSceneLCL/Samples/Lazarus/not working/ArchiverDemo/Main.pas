unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, GLScene,
  GLObjects, GLVectorFileObjects, GLMaterial, GLCadencer, GLSArchiveManager,
  GLLCLViewer, GLBaseClasses, GLVectorGeometry,
  GLFileMS3D, TGA, GLFileZLIB;

type

  { TForm1 }

  TForm1 = class(TForm)
    GLCadencer1: TGLCadencer;
    GLCamera: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLFreeForm: TGLFreeForm;
    GLFreeForm1: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLPlane1: TGLPlane;
    GLSArchiveManager1: TGLSArchiveManager;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: double);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses glutils;
{ TForm1 }

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: double);
begin
  GLCamera.Position.Rotate(VectorMake(0, 1, 0), deltaTime * 0.1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  setGLSceneMediaDir();

 (* with GLSArchiveManager1.Archives[0] do
  begin
    LoadFromFile('Chair.zlib');
    if FileName = '' then
      ShowMessage('Archive Can not be Loaded');
    {: Automatic loading from archive.
       If file is not in archive, then it's loaded from harddrive. }

  end;  *)

   GLFreeForm.LoadFromFile('Chair.ms3d');
 //   {: Direct loading from archive }
 //   GLFreeForm1.LoadFromStream('Chair.ms3d', GetContent('Chair.ms3d'));
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  GLCadencer1.Enabled:=true;
end;

end.

