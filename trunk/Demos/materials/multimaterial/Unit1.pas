{: Basic GLMultiMaterialShader example.<p>

   The GLMultiMaterialShader applies a pass for each material in
   the assigned MaterialLibrary. This example shows how to apply
   three blended textures to an object without the use of
   multi-texuring.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLWin32Viewer, GLTexture, GLMisc, OpenGL1x,
  GLCadencer, GLMultiMaterialShader;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary2: TGLMaterialLibrary;
    GLMultiMaterialShader1: TGLMultiMaterialShader;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    mx,my : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses JPEG;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

  // GLMaterialLibrary2 is the source of the GLMultiMaterialShader
  // passes.
  with GLMaterialLibrary2 do begin
    // Base texture
    AddTextureMaterial('Pass1','glscene.bmp');

    // Add a bit of detail
    with AddTextureMaterial('Pass2','detailmap.jpg').Material do begin
      MaterialOptions:=[moNoLighting];
      BlendingMode:=bmAdditive;
      Texture.TextureMode:=tmBlend;
    end;

    // And a little specular reflection
    with AddTextureMaterial('Pass3','terrain.bmp').Material do begin
      BlendingMode:=bmAdditive;
      Texture.TextureMode:=tmModulate;
      Texture.MappingMode:=tmmCubeMapReflection;
    end;

    // This isn't limited to 3, try adding some more passes!
  end;

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in shift then
    GLCamera1.MoveAroundTarget(my-y,mx-x);
  mx:=x;
  my:=y;
end;

end.
