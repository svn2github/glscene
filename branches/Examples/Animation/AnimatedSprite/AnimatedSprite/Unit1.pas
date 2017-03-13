unit Unit1;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.Jpeg,
  GLTexture, GLCadencer, GLAnimatedSprite, GLScene, GLObjects,
  GLWin32Viewer, GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLAnimatedSprite1: TGLAnimatedSprite;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLAnimatedSprite2: TGLAnimatedSprite;
    GLAnimatedSprite3: TGLAnimatedSprite;
    GLAnimatedSprite4: TGLAnimatedSprite;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
     
  public
     
  end;

  const
  maxsprites=100;

var
  Form1: TForm1;
  examplesprites: array [0..maxsprites] of TGLAnimatedSprite;

implementation

{$R *.dfm}

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLDummyCube1.Roll(20*deltatime);
  GLDummyCube1.turn(20*deltatime);
end;

end.

