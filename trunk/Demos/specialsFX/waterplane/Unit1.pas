unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLWin32Viewer, GLWaterPlane,
  GLCadencer, ExtCtrls, Jpeg, GLTexture, GLUserShader, OpenGL1x,
  VectorGeometry;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DCTarget: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    GLWaterPlane1: TGLWaterPlane;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLUserShader1: TGLUserShader;
    GLSphere1: TGLSphere;
    GLDirectOpenGL1: TGLDirectOpenGL;
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
    procedure GLDirectOpenGL1Render(var rci: TRenderContextInfo);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    reflectionToggle : Boolean;
    procedure ClickPond(x, y : Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ClickPond(x, y : Integer);
var
   ip : TVector;
begin
   // create a ripple in the pond on a right-mousebutton click

   GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXZ(
                           VectorMake(x, GLSceneViewer1.Height-y, 0),
                           GLWaterPlane1.Position.Y, ip);
   GLWaterPlane1.CreateRippleAtWorldPos(ip[0], ip[1], ip[2]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');

   // Load the cube map which is used both for environment and as reflection texture

   with GLMaterialLibrary1.Materials[0].Material.Texture do begin
      ImageClassName:=TGLCubeMapImage.ClassName;
      with Image as TGLCubeMapImage do begin
         // Load all 6 texture map components of the cube map
         // The 'PX', 'NX', etc. refer to 'positive X', 'negative X', etc.
         // and follow the RenderMan specs/conventions
         Picture[cmtPX].LoadFromFile('cm_left.jpg');
         Picture[cmtNX].LoadFromFile('cm_right.jpg');
         Picture[cmtPY].LoadFromFile('cm_top.jpg');
         Picture[cmtNY].LoadFromFile('cm_bottom.jpg');
         Picture[cmtPZ].LoadFromFile('cm_back.jpg');
         Picture[cmtNZ].LoadFromFile('cm_front.jpg');
      end;
   end;        
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
   if ssRight in Shift then
      ClickPond(x, y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera1.MoveAroundTarget(my-y, mx-x)
   else if ssRight in Shift then
      ClickPond(x, y);
   mx:=x; my:=y;
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TRenderContextInfo);
var
   cubeMapMode : Integer;
begin
   // Here is the shader trick: the same cubemap is used in reflection mode
   // for the pond, and in normal mode for the environment sphere
   // Our basic user shader takes care of that.
   if reflectionToggle then
      cubeMapMode:=GL_REFLECTION_MAP_ARB
   else cubeMapMode:=GL_NORMAL_MAP_ARB;
   
   glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, cubeMapMode);
   glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, cubeMapMode);
   glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, cubeMapMode);
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
   reflectionToggle:=False;   // toggle for environment sphere
end;

procedure TForm1.GLDirectOpenGL1Render(var rci: TRenderContextInfo);
begin
   reflectionToggle:=True;    // toggle for pond/water plane
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

end.
