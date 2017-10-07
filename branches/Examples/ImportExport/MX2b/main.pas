{-----------------------------------------------------------------------------
 Unit Name: main
 Author:    stefan_bazelkov
 Purpose:   MX2 file import demo
 History:
-----------------------------------------------------------------------------}


unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, GLScene, GLObjects, GLCadencer,
  GLTexture, GLWin32Viewer, GLVectorFileObjects, JPEG,
  GLFileMX2, ComCtrls, GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    Main_Panel1: TPanel;
    Tools_Panel2: TPanel;
    btnLoadMX2Actor1: TButton;
    OpenDialog1: TOpenDialog;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    GLLightSource2: TGLLightSource;
    dbg1: TMemo;
    procedure btnLoadMX2Actor1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
  private
    mx, my: Integer;
    procedure FeedBackMX2(Mt: TMx2MessageType; const S: string);
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses Math;

procedure TForm1.btnLoadMX2Actor1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    with GLActor1 do
    begin
      GLMaterialLibrary1.Materials.Clear;
      LoadFromFile(OpenDialog1.FileName);
      Animations.SetToStrings(dbg1.Lines);
      dbg1.Lines.Insert(0,'Animations:');
    end;
end;

procedure TForm1.FeedBackMX2(Mt: TMx2MessageType; const S: string);
begin
  StatusBar1.Panels[0].Text := S;
  StatusBar1.Refresh;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLFileMx2.OnMx2Message := FeedBackMX2;
  with GLActor1 do
    begin
     FrameInterpolation := afpLinear;
     AnimationMode := aamBounceForward;
    end;
  GLMaterialLibrary1.TexturePaths:=ExtractFilePath(ParamStr(0))+'Images\';
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(0.98, WheelDelta / 150));
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then GLCamera1.MoveAroundTarget(my - y, mx - x);
  mx := x;
  my := y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Caption:=Format('MX2 Demo %.1f FPS', [GLSceneViewer1.FramesPerSecond]);
 GLSceneViewer1.ResetPerformanceMonitor;
end;

end.

