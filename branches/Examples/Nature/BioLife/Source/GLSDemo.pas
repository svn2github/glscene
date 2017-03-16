{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit GLSDemo;
interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLVectorFileObjects,
  GLSkydome, GLWin32Viewer, ExtCtrls, Buttons, StdCtrls, ComCtrls,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TAAADemoForm = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLSkyDome1: TGLSkyDome;
    GLBirdBody: TGLFreeForm;
    GLDummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    UpZTB: TTrackBar;
    UpYTB: TTrackBar;
    UpXTB: TTrackBar;
    PitchAngle: TTrackBar;
    DirectionZTB: TTrackBar;
    DirectionYTB: TTrackBar;
    DirectionXTB: TTrackBar;
    RollAngle: TTrackBar;
    UpXTBLabel: TLabel;
    UpXLabel: TLabel;
    UpYLabel: TLabel;
    UpYTBLabel: TLabel;
    UpZTBLabel: TLabel;
    UpZLabel: TLabel;
    PitchTBLabel: TLabel;
    PitchLabel: TLabel;
    RollLabel: TLabel;
    RollTBLabel: TLabel;
    DirectionXTBLabel: TLabel;
    DirectionXLabel: TLabel;
    DirectionYTBLabel: TLabel;
    DirectionYLabel: TLabel;
    DirectionZTBLabel: TLabel;
    DirectionZLabel: TLabel;
    StopBtn: TSpeedButton;
    Timer1: TTimer;
    DistanceBar: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure UpXTBChange(Sender: TObject);
    procedure UpYTBChange(Sender: TObject);
    procedure UpZTBChange(Sender: TObject);
    procedure PitchAngleChange(Sender: TObject);
    procedure RollAngleChange(Sender: TObject);
    procedure DirectionXTBChange(Sender: TObject);
    procedure DirectionYTBChange(Sender: TObject);
    procedure DirectionZTBChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DistanceBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     
    Mmx,Mmy,Mmx2,Mmy2:integer;{Mouse-Camera Movements}
    Going:Boolean;
  public
     
  end;

var
  AAADemoForm: TAAADemoForm;

implementation
uses nUGlobal;
{$R *.DFM}

procedure TAAADemoForm.FormCreate(Sender: TObject);
begin
  top := frmBoidsY;
  left := frmBoidsX;
  Going:=False;
  Timer1.Enabled:=False;
  If (not FileExists(ExtractFilePath(ParamStr(0))+'Glsboid.3ds'))
  then
  begin
    showmessage('Glsboid.3ds file is missing');
    Application.Terminate;
  end else
  begin
  GLBirdBody.LoadFromFile(ExtractFilePath(ParamStr(0))+'Glsboid.3ds');
  GLBirdBody.Scale.X:=2;
  GLBirdBody.Scale.Y:=2;
  GLBirdBody.Scale.Z:=2;
  end;
end;

procedure TAAADemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled:=False;
  frmBoidsY := AAADemoForm.top;
  frmBoidsX := AAADemoForm.left;
  if ReallyGone then Action := caFree else Action := caHide;
end;

procedure TAAADemoForm.FormShow(Sender: TObject);
begin
  Going:=True;
  Timer1.Enabled:=True;
  Timer1Timer(Sender);
end;

procedure TAAADemoForm.UpXTBChange(Sender: TObject);
begin
  UpXTBLabel.Caption:=Inttostr(UpXTB.Position);
  GLBirdBody.Up.X:= UpXTB.Position/100;
end;

procedure TAAADemoForm.UpYTBChange(Sender: TObject);
begin
  UpYTBLabel.Caption:=Inttostr(UpYTB.Position);
  GLBirdBody.Up.Y:= UpYTB.Position/100;
end;

procedure TAAADemoForm.UpZTBChange(Sender: TObject);
begin
  UpZTBLabel.Caption:=Inttostr(UpZTB.Position);
  GLBirdBody.Up.Z:= UpZTB.Position/100;
end;

procedure TAAADemoForm.PitchAngleChange(Sender: TObject);
begin
  PitchTBLabel.Caption:=Inttostr(PitchAngle.Position);
  GLBirdBody.PitchAngle:= PitchAngle.Position;
end;

procedure TAAADemoForm.RollAngleChange(Sender: TObject);
begin
  RollTBLabel.Caption:=Inttostr(RollAngle.Position);
  GLBirdBody.RollAngle:= RollAngle.Position;
end;

procedure TAAADemoForm.DirectionXTBChange(Sender: TObject);
begin
  DirectionXTBLabel.Caption:=Inttostr(DirectionXTB.Position);
  GLBirdBody.Direction.X:= DirectionXTB.Position/100;
end;

procedure TAAADemoForm.DirectionYTBChange(Sender: TObject);
begin
  DirectionYTBLabel.Caption:=Inttostr(DirectionYTB.Position);
  GLBirdBody.Direction.Y:= DirectionYTB.Position/100;
end;

procedure TAAADemoForm.DirectionZTBChange(Sender: TObject);
begin
  DirectionZTBLabel.Caption:=Inttostr(DirectionZTB.Position);
  GLBirdBody.Direction.Z:= DirectionZTB.Position/100;
end;

procedure TAAADemoForm.Timer1Timer(Sender: TObject);
begin
  if Going then
  begin
  UpXLabel.Caption:=Format('%.4f Up X', [GLBirdBody.Up.X]);
  UpYLabel.Caption:=Format('%.4f Up Y', [GLBirdBody.Up.Y]);
  UpZLabel.Caption:=Format('%.4f Up Z', [GLBirdBody.Up.Z]);
  PitchLabel.Caption:=Format('%.4f Pitch', [GLBirdBody.PitchAngle]);
  RollLabel.Caption:=Format('%.4f Roll', [GLBirdBody.RollAngle]);
  DirectionXLabel.Caption:=Format('%.4f Dir X', [GLBirdBody.Direction.X]);
  DirectionYLabel.Caption:=Format('%.4f Dir Y', [GLBirdBody.Direction.Y]);
  DirectionZLabel.Caption:=Format('%.4f Dir Z', [GLBirdBody.Direction.Z]);
  UpXTB.Position:=Round(GLBirdBody.Up.X*100);
  UpYTB.Position:=Round(GLBirdBody.Up.Y*100);
  UpZTB.Position:=Round(GLBirdBody.Up.Z*100);
  PitchAngle.Position:=Round(GLBirdBody.PitchAngle);
  RollAngle.Position:=Round(GLBirdBody.RollAngle);
  DirectionXTB.Position:=Round(GLBirdBody.Direction.X*100);
  DirectionYTB.Position:=Round(GLBirdBody.Direction.Y*100);
  DirectionZTB.Position:=Round(GLBirdBody.Direction.Z*100);
  end;
end;

procedure TAAADemoForm.StopBtnClick(Sender: TObject);
begin
  Going:=False;
  Timer1.Enabled:=False;
  Close;
end;

procedure TAAADemoForm.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   Mmx:=x; Mmy:=y;
   Mmx2:=x; Mmy2:=y;
end;

procedure TAAADemoForm.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
   begin
      Mmx2:=x;
      Mmy2:=y;
     if ((Mmx<>Mmx2)or(Mmy<>Mmy2)) then
     begin
       GLCamera1.MoveAroundTarget(Mmy-Mmy2, Mmx-Mmx2);
      //move the camera around the target if mouse was dragged
       Mmx:=Mmx2; Mmy:=Mmy2;
     end;
   end;
end;

procedure TAAADemoForm.DistanceBarChange(Sender: TObject);
var
   Dist, NewDist,cx,cy,cz :single;
begin
   Dist:=GLCamera1.DistanceToTarget;
   cx:=GLCamera1.Position.x;
   cy:=GLCamera1.Position.y;
   cz:=GLCamera1.Position.z;
   NewDist:=DistanceBar.position;
   GLCamera1.Position.x:=cx/dist*NewDist;
   GLCamera1.Position.y:=cy/dist*NewDist;
   GLCamera1.Position.z:=cz/dist*NewDist;
end;



end.
