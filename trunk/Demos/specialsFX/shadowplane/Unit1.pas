{: Simple projective shadows.<p>

   The TGLShadowPlane component allows to render simple projective shadows.
   They have the benefit of being quite fast, but as the name says, your shadows
   will be projected only on a plane and  must be (entirely) on the same side
   of the plane as the light (the side pointed by the plane's direction).<p>
   Note that stenciling is required for proper operation (it is an option of
   the Viewer.Buffer.ContextOptions), which should be available on all modern
   graphics hardware. When stenciling is not activated, the ShadowPlane will
   use opaque shadows and you may see shadows appear beyond the plane limits...
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, GLShadowPlane, GLMisc, GLScene, GLWin32Viewer, GLObjects,
  GLCadencer, StdCtrls, Geometry, ExtCtrls;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    DCShadowing: TDummyCube;
    GLLightSource1: TGLLightSource;
    Cube1: TCube;
    Sphere1: TSphere;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    DCLight: TDummyCube;
    Sphere2: TSphere;
    Torus1: TTorus;
    DCCameraTarget: TDummyCube;
    GLShadowPlane1: TGLShadowPlane;
    Timer1: TTimer;
    Panel1: TPanel;
    CBShadows: TCheckBox;
    CBStencil: TCheckBox;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBShadowsClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Jpeg;

procedure TForm1.FormCreate(Sender: TObject);
begin
   with GLShadowPlane1.Material.Texture.Image do
      LoadFromFile(ExtractFilePath(Application.ExeName)+'..\..\Media\BeigeMarble.jpg');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DCLight.PitchAngle:=Sin(newTime)*60;
   DCShadowing.TurnAngle:=newTime*10;
end;

procedure TForm1.CBShadowsClick(Sender: TObject);
begin
   if CBShadows.Checked then
      GLShadowPlane1.ShadowedLight:=GLLightSource1
   else GLShadowPlane1.ShadowedLight:=nil;
end;

procedure TForm1.CBStencilClick(Sender: TObject);
begin
   if CBStencil.Checked then
      GLShadowPlane1.ShadowOptions:=[spoUseStencil]
   else GLShadowPlane1.ShadowOptions:=[];
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
