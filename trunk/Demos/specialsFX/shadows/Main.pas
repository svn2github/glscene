{: Shadow casting with GLzBuffer by Rene Lindsay.

}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GLScene, GLMisc, GLGraph, GLObjects, GLTexture, StdCtrls,
  ComCtrls,jpeg, glgraphics, VectorTypes, geometry, GLHUDObjects,
  GLzBuffer, OpenGL12,
  //GLBehaviours,
  GLCadencer, AsyncTimer, Spin, GLWin32Viewer;

type
  TMainFm = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    GLScene1: TGLScene;
    Label1: TLabel;
    Label2: TLabel;
    GLCamera1: TGLCamera;
    GLCamera2: TGLCamera;
    Objects: TDummyCube;
    GLLightSource1: TGLLightSource;
    Viewer: TGLSceneViewer;
    Caster: TGLSceneViewer;
    Panel4: TPanel;
    Label4: TLabel;
    DistanceBar: TTrackBar;
    Label3: TLabel;
    DistanceBar2: TTrackBar;
    Panel5: TPanel;
    GLMaterialLibrary1: TGLMaterialLibrary;
    MemView: TGLMemoryViewer;
    Shadows1: TZShadows;
    Cube1: TCube;
    FrustBox: TCheckBox;
    AsyncTimer1: TAsyncTimer;
    Torus1: TTorus;
    RotateBox: TCheckBox;
    ShadowOnBox: TCheckBox;
    GLCadencer1: TGLCadencer;
    HeightField1: THeightField;
    Teapot1: TTeapot;
    SoftBox: TCheckBox;
    SkyShadBox: TCheckBox;
    Focal: TTrackBar;
    Label5: TLabel;
    CastBtn: TButton;
    TimeLbl: TLabel;
    Label6: TLabel;
    XresBox: TComboBox;
    Label7: TLabel;
    YresBox: TComboBox;
    Panel6: TPanel;
    FadeBox: TCheckBox;
    dovBar: TTrackBar;
    Memo1: TMemo;
    AlphaBar: TTrackBar;
    Label9: TLabel;
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CasterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CasterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DistanceBarChange(Sender: TObject);
    procedure DistanceBar2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CastBtnClick(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CasterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FadeBoxClick(Sender: TObject);
    procedure HeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure FrustBoxClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure RotateBoxClick(Sender: TObject);
    procedure ShadowOnBoxClick(Sender: TObject);
    procedure XResBoxChange(Sender: TObject);
    procedure YresBoxChange(Sender: TObject);
    procedure SoftBoxClick(Sender: TObject);
    procedure SkyShadBoxClick(Sender: TObject);
    procedure FocalChange(Sender: TObject);
    procedure dovBarChange(Sender: TObject);
    procedure AlphaBarChange(Sender: TObject);
  private
    { Private declarations }
  public
    mx,my   :integer;
    mx2,my2 :integer;
    zViewer, zCaster :TGLzBuffer;
  end;

var
  MainFm: TMainFm;

implementation

{$R *.DFM}

procedure TMainFm.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 mx:=x; my:=y;
 MainFm.ActiveControl:=DistanceBar;
end;

procedure TMainFm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Shift<>[] then GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TMainFm.CasterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 mx2:=x; my2:=y;
 MainFm.ActiveControl:=DistanceBar2;
end;

procedure TMainFm.CasterMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Shift<>[] then GLCamera2.MoveAroundTarget(my2-y, mx2-x);
   mx2:=x; my2:=y;
   Shadows1.CastShadow;
   Caster.refresh;
   Viewer.Refresh;
end;

procedure TMainFm.DistanceBarChange(Sender: TObject);
var Dist, NewDist :single;
begin
 With GLCamera1 do begin
    Dist:=DistanceToTarget;
   NewDist:=Sqr(DistanceBar.position/4)+1;
   position.AsAffineVector:=VectorScale(position.AsAffineVector,NewDist/dist);
 end;
end;

procedure TMainFm.DistanceBar2Change(Sender: TObject);
var Dist, NewDist :single;
begin
 With GLCamera2 do begin
   Dist:=DistanceToTarget;
   NewDist:=Sqr(DistanceBar2.position/4)+1;
   position.AsAffineVector:=VectorScale(position.AsAffineVector,NewDist/dist);
 end;
 Shadows1.CastShadow;
 Caster.Refresh;
end;

procedure TMainFm.FormCreate(Sender: TObject);
begin
 GLMaterialLibrary1.Materials[2].Material.texture.Image.loadFromFile('..\..\media\marbletiles.jpg');
 GLMaterialLibrary1.Materials[2].Material.texture.disabled:=false;

 GLMaterialLibrary1.Materials[3].Material.texture.Image.loadFromFile('..\..\media\beigemarble.jpg');
 GLMaterialLibrary1.Materials[3].Material.texture.disabled:=false;
{
 GLMaterialLibrary1.Materials[4].Material.texture.Image.loadFromFile('marble.jpg');
 GLMaterialLibrary1.Materials[4].Material.texture.disabled:=false;
}
end;

procedure TMainFm.CastBtnClick(Sender: TObject);
var RefTime :double;
begin
 RefTime:=GLCadencer1.GetcurrentTime;
 Shadows1.CastShadow;
 Viewer.refresh;
 TimeLbl.Caption:=IntToStr(Round((GLCadencer1.GetCurrentTime-refTime)*100));
end;

procedure TMainFm.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
Viewer.Visible:=True;
end;

procedure TMainFm.CasterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 Shadows1.CastShadow;
end;

procedure TMainFm.FadeBoxClick(Sender: TObject);
begin
Shadows1.DepthFade:=FadeBox.checked;
end;

procedure TMainFm.HeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
z:=0;
end;

procedure TMainFm.FrustBoxClick(Sender: TObject);
begin
Shadows1.FrustShadow:=FrustBox.Checked;
end;

procedure TMainFm.AsyncTimer1Timer(Sender: TObject);
begin
 if RotateBox.checked then begin
  Torus1.PitchAngle:=Torus1.PitchAngle+4;
  Teapot1.TurnAngle:=Teapot1.TurnAngle-4;
  Caster.refresh;
  Shadows1.CastShadow;
 end;

//  Caption:=Format('%.1f FPS', [Viewer.FramesPerSecond]);
  Caption:=Format('%.2f FPS', [Viewer.FramesPerSecond]);
  Viewer.ResetPerformanceMonitor;



end;

procedure TMainFm.RotateBoxClick(Sender: TObject);
begin
//AsyncTimer1.Enabled:=RotateBox.checked;
end;

procedure TMainFm.ShadowOnBoxClick(Sender: TObject);
begin
Shadows1.Visible:=ShadowOnBox.Checked;
end;

procedure TMainFm.XResBoxChange(Sender: TObject);
begin
 Shadows1.Xres:=StrToIntDef(TComboBox(sender).text,256);
end;

procedure TMainFm.YresBoxChange(Sender: TObject);
begin
 Shadows1.Yres:=StrToIntDef(TComboBox(sender).text,256);
end;

procedure TMainFm.SoftBoxClick(Sender: TObject);
begin
 Shadows1.Soft:=SoftBox.Checked;
end;

procedure TMainFm.SkyShadBoxClick(Sender: TObject);
begin
 Shadows1.SkyShadow:=SkyShadBox.Checked;
end;

procedure TMainFm.FocalChange(Sender: TObject);
begin
 GLCamera2.FocalLength:=Focal.Position;
 MemView.Render;
 Caster.refresh;
 Shadows1.CastShadow;
 Viewer.refresh;
end;

procedure TMainFm.dovBarChange(Sender: TObject);
begin
 GLCamera2.DepthOfView:=DovBar.Position;
 MemView.Render;
 Caster.refresh;
 Shadows1.CastShadow;
 Viewer.refresh;
end;

procedure TMainFm.AlphaBarChange(Sender: TObject);
begin
   Shadows1.Color.Alpha:=AlphaBar.Position/256;
end;

end.
