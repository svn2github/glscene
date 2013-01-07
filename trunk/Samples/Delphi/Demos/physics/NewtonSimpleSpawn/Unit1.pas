unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLScene,
  GLObjects,
  GLCoordinates,
  GLSimpleNavigation,
  GLCadencer,
  GLWin32Viewer,
  GLCrossPlatform,
  BaseClasses,
  GLNGDManager,
  StdCtrls,
  GLGeomObjects,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects,
  ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    GLDummyCube1: TGLDummyCube;
    GLNGDManager1: TGLNGDManager;
    GLResolutionIndependantHUDText1: TGLResolutionIndependantHUDText;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLColor;

procedure TForm1.Button1Click(Sender: TObject);
var
  GLCube1: TGLCube;
  DynNGDBehav: TGLNGDDynamic;
begin
  GLCube1 := TGLCube.CreateAsChild(GLDummyCube1);
  GLCube1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCube1.GetOrCreateBehaviour(TGLNGDDynamic) as TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  GLSphere1: TGLSphere;
  DynNGDBehav: TGLNGDDynamic;
begin
  GLSphere1 := TGLSphere.CreateAsChild(GLDummyCube1);
  GLSphere1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLSphere1.GetOrCreateBehaviour(TGLNGDDynamic) as TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  GLCone1: TGLCone;
  DynNGDBehav: TGLNGDDynamic;
begin
  GLCone1 := TGLCone.CreateAsChild(GLDummyCube1);
  GLCone1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCone1.GetOrCreateBehaviour(TGLNGDDynamic) as TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  GLCylinder1: TGLCylinder;
  DynNGDBehav: TGLNGDDynamic;
begin
  GLCylinder1 := TGLCylinder.CreateAsChild(GLDummyCube1);
  GLCylinder1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCylinder1.GetOrCreateBehaviour(TGLNGDDynamic)
    as TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  GLCapsule1: TGLCapsule;
  DynNGDBehav: TGLNGDDynamic;
begin
  GLCapsule1 := TGLCapsule.CreateAsChild(GLDummyCube1);
  GLCapsule1.Material.FrontProperties.Diffuse.RandomColor;
  DynNGDBehav := GLCapsule1.GetOrCreateBehaviour(TGLNGDDynamic)
    as TGLNGDDynamic;
  DynNGDBehav.Manager := GLNGDManager1;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  GLDummyCube1.DeleteChildren;
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
  GLResolutionIndependantHUDText1.Text := 'Bodycount:=' + inttostr
    (GLNGDManager1.NewtonBodyCount);
end;

end.
