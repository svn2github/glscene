unit Unit1;

interface
       
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLScene, GLWin32Viewer,
  GLTexture, JPEG, GLMaterial, GLCoordinates, GLCrossPlatform,
  GLBaseClasses, GLVectorFileObjects, GLFile3DS;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLFreeForm1: TGLFreeForm;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    mx,my:integer;
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLActor1.LoadFromFile('AMX.3DS');
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
