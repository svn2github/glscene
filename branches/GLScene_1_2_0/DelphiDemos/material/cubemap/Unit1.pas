{ : Basic sample for loading and applying a cube map.<p>

  In this extremely simple demo, with a single teapot, we just setup and
  enabled reflection cube map environment mapping. Cube maps allow higher
  quality reflections than sphere map (no singularity) at the expense of
  memory use (they require 6 images instead of one).<p>
  Setting up a cube map is straightforward and similar to setting up any other
  kind of texture, all parameters and options apply, but instead of specifying
  a single image, you have to specify 6.<p>

  The cube map images used in this sample are from the Developper section
  of the nVidia website (http://www.nvidia.com).
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,

  GLScene_Core,
  GLScene_Objects,
  GLScene_Objects_Teapot,
  GLScene_Base_Coordinates,
  GLScene_Platform,
  GLScene_Base_Classes,
  GLScene_Viewer_VCL,
  GLScene_Material,
  GLScene_MaterialEx;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Button1: TButton;
    Teapot1: TGLTeapot;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure GLMaterialLibraryEx1MatLibComponentFail(const AComponent
      : TGLBaseMaterialCollectionItem; AFail: TMatLibComponentFails);
  private
    { Private declarations }
    CubmapSupported: Boolean;
  public
    { Public declarations }
    mx, my: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLScene_Texture,
  GLScene_Base_Context,
  GLScene_Files_DDS;

procedure TForm1.GLMaterialLibraryEx1MatLibComponentFail(const AComponent
  : TGLBaseMaterialCollectionItem; AFail: TMatLibComponentFails);
begin
  if (AComponent is TGLTextureImageEx) and (AFail = mlcfSupport) then
  // Cube map warning message
  // If you don't check and turn off cube maps yourself in your apps when
  // cube maps aren't supported, GLScene will just show error-image texture
  // (ie. no error generated, just a different output)
  begin
    ShowMessage('Your graphics board does not support cube maps...');
    Exit;
  end;
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  CubmapSupported := GL.ARB_texture_cube_map;
  GLSceneViewer1.BeforeRender := nil;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  t: TGLTextureImageEx;
begin
  // Define cubemap source file
  t := GLMaterialLibraryEx1.Components.GetTextureByName('CubeTexture')
    as TGLTextureImageEx;
  t.SourceFile := ExtractFilePath(Application.ExeName) +
    '..\..\..\media\deadmeat_skymorning.dds';

  with GLMaterialLibraryEx1.Materials.GetLibMaterialByName('CubeMapMaterial') do
  begin
    // Select reflection cube map environment mapping
    // This is the mode you'll most commonly use with cube maps, normal cube
    // map generation is also supported (used for diffuse environment lighting)
    FixedFunction.Texture.MappingMode := tmmCubeMapReflection;
    FixedFunction.Texture.Enabled := True;
  end;

  Button1.Visible := False;
end;

// standard issue handlers for mouse movement

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my - Y, mx - X)
    else
      GLCamera1.RotateTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

end.
