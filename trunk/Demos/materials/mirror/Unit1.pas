unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLMisc, GLExtrusion, GLMirror, GLMultiPolygon,
  ExtCtrls, GLCadencer, StdCtrls;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Sphere1: TSphere;
    DCObjects: TDummyCube;
    Torus1: TTorus;
    Teapot1: TTeapot;
    Cylinder1: TCylinder;
    GLMirror1: TGLMirror;
    ExtrusionSolid1: TExtrusionSolid;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Label1: TLabel;
    CBOpaque: TCheckBox;
    CBStencil: TCheckBox;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CBOpaqueClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TForm1.CBOpaqueClick(Sender: TObject);
begin
   if CBOpaque.Checked then
      GLMirror1.MirrorOptions:=GLMirror1.MirrorOptions+[moOpaque]
   else GLMirror1.MirrorOptions:=GLMirror1.MirrorOptions-[moOpaque];
end;

procedure TForm1.CBStencilClick(Sender: TObject);
begin
   if CBStencil.Checked then begin
      GLMirror1.MirrorOptions:=GLMirror1.MirrorOptions+[moUseStencil];
   end else begin
      GLMirror1.MirrorOptions:=GLMirror1.MirrorOptions-[moUseStencil];
   end;
end;

end.
