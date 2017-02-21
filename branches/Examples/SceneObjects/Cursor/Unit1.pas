unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  //GLS
  GLScene,
  GLCadencer,
  GLObjects,
  GLGeomObjects,
  GLWin32Viewer,
  GLVectorGeometry,
  GLVectorTypes,
  GLFileTGA,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    Camera: TGLCamera;
    Player: TGLDummyCube;
    Actor: TGLCone;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    targ: TGLPlane;
    procedure vpMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
     
  public
     
  end;

var
  Form1: TForm1;

  _mx,_my:integer;
  _look:boolean=false;

implementation

{$R *.dfm}

procedure TForm1.vpMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
//var
  //d,c:single;
begin
  _mx:=x;
  _my:=y;
  _look:=true;
  {
  d:=round(form1.GLSceneViewer1.Width/2);
  c:=round(form1.GLSceneViewer1.Height/2);
  form1.actor.RollAngle:=round(NormalizeDegAngle(radtodeg(arctan2(x-d+32,y-c+32))));
  form1.Caption:=inttostr(round(NormalizeDegAngle(radtodeg(arctan2(x-d+32,y-c+32)))));
  }
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  glcadencer1.Enabled:=false;
  vp.Free;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var v:TVector4f;
begin
  targ.Roll(deltatime*50);

  if _look then begin
    vp.Buffer.ScreenVectorIntersectWithPlane(vectormake(_mx,vp.height-_my,0),player.AbsolutePosition,player.AbsoluteUp,v);
    targ.AbsolutePosition:=v;
    player.PointTo(v,player.AbsoluteUp);
    _look:=false;
    end;
end;

end.
