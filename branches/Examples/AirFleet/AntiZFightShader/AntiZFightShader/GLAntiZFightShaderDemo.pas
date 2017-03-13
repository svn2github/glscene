unit GLAntiZFightShaderDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Math, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Jpeg,
   
  GLScene, GLSkydome, GLWin32Viewer, GLObjects,
  GLTexture, GLAntiZFightShader, GLGeomObjects, GLMaterial,
  GLCoordinates, GLCrossPlatform, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLSkyDome1: TGLSkyDome;
    DummyTarget: TGLDummyCube;
    YellowSphere: TGLSphere;
    BlueSphere: TGLSphere;
    GLLightSource1: TGLLightSource;
    CheckBox1: TCheckBox;
    GLCylinder1: TGLCylinder;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLDummyCube3: TGLDummyCube;
    GLCube1: TGLCube;
    CheckBox2: TCheckBox;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CheckBox1Click(Sender: TObject);

    procedure CheckBox2Click(Sender: TObject);
  private
     
  public
     
    mx, my : integer;

    AntiZFightShader : TGLAntiZFightShader;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
        TempLibMaterial : TGLLibMaterial;
        i : integer;
begin
        form1.Top := round((screen.Height/2)-(form1.Height/2));
        form1.left := round((screen.width/2)-(form1.width/2));

        glskydome1.stars.addrandomstars(5000, RGB(255,255,255), false);

        { Run-time creation of the shader object }
        AntiZFightShader := TGLAntiZFightShader.Create(Self);
        { For now, this is simply a value that the user provides to compensate
        for the overall dimensions of a scene.  I have no easy answer for what
        numbers are good yet: use trial and error.  Basically, if your scene is
        very large, use a large number, and vice versa }
        AntiZFightShader.Modifier := 200000000;

        { Assign the shader to the appropriate materials }
        for i := 0 to 3 do
        begin
             GlMaterialLibrary1.Materials[i].Shader := AntiZFightShader;
        end;

        { Turn off GLScene's object depth sorting, as it doesn't take into
         account manipulations of the projection matrix }
        gldummycube2.ObjectStyle := gldummycube2.ObjectStyle+[osIgnoreDepthBuffer];
        gldummycube3.ObjectStyle := gldummycube3.ObjectStyle+[osIgnoreDepthBuffer];
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
        mx := x;
        my := y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	glCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/-300));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
    AntiZFightShader.Enabled := CheckBox1.Checked;

    glsceneviewer1.Refresh;
end;



procedure TForm1.CheckBox2Click(Sender: TObject);
begin
        if checkbox2.Checked then
        begin
                gldummycube2.ObjectStyle := gldummycube2.ObjectStyle+[osIgnoreDepthBuffer];
                gldummycube3.ObjectStyle := gldummycube3.ObjectStyle+[osIgnoreDepthBuffer];
        end
        else
        begin
                gldummycube2.ObjectStyle := gldummycube2.ObjectStyle-[osIgnoreDepthBuffer];
                gldummycube3.ObjectStyle := gldummycube3.ObjectStyle-[osIgnoreDepthBuffer];
        end;
        glsceneviewer1.refresh;
end;

end.



