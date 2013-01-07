unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLSimpleNavigation, GLScene, GLCoordinates, GLNGDManager,
  GLBitmapFont, GLWindowsFont, GLCadencer, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLSimpleNavigation1: TGLSimpleNavigation;
    Trampoline: TGLCube;
    GLCube1: TGLCube;
    GLSphere1: TGLSphere;
    GLSphere2: TGLSphere;
    GLDummyCube1: TGLDummyCube;
    GLDummyCube2: TGLDummyCube;
    GLCube2: TGLCube;
    Friction: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLDummyCube3: TGLDummyCube;
    GLNGDManager1: TGLNGDManager;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  SurfaceTrampoline, SurfaceFriction: TNGDSurfaceItem;
  SurfaceCube2, SurfaceCube3, SurfaceCube4: TNGDSurfaceItem;
  SurfaceSphere1_Sphere2_Cube1: TNGDSurfaceItem;

  ObjectOnTrampoline: TNGDSurfacePair;
  FrictionOnCube2, FrictionOnCube3, FrictionOnCube4: TNGDSurfacePair;
begin
  // Get each SurfaceItem
  SurfaceTrampoline := GLNGDManager1.NewtonSurfaceItem.Items[0]
    as TNGDSurfaceItem;
  SurfaceFriction := GLNGDManager1.NewtonSurfaceItem.Items[1]
    as TNGDSurfaceItem;
  SurfaceCube2 := GLNGDManager1.NewtonSurfaceItem.Items[2] as TNGDSurfaceItem;
  SurfaceCube3 := GLNGDManager1.NewtonSurfaceItem.Items[3] as TNGDSurfaceItem;
  SurfaceCube4 := GLNGDManager1.NewtonSurfaceItem.Items[4] as TNGDSurfaceItem;
  SurfaceSphere1_Sphere2_Cube1 := GLNGDManager1.NewtonSurfaceItem.Items[5]
    as TNGDSurfaceItem;

  // Set them to Behaviours
  GetNGDStatic(Trampoline).NGDSurfaceItem := SurfaceTrampoline;
  GetNGDStatic(Friction).NGDSurfaceItem := SurfaceFriction;
  GetNGDDynamic(GLCube2).NGDSurfaceItem := SurfaceCube2;
  GetNGDDynamic(GLCube3).NGDSurfaceItem := SurfaceCube3;
  GetNGDDynamic(GLCube4).NGDSurfaceItem := SurfaceCube4;
  GetNGDDynamic(GLCube1).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere1).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;
  GetNGDDynamic(GLSphere2).NGDSurfaceItem := SurfaceSphere1_Sphere2_Cube1;

  // Get each SurfacePair
  ObjectOnTrampoline := GLNGDManager1.NewtonSurfacePair.Items[0]
    as TNGDSurfacePair;
  FrictionOnCube2 := GLNGDManager1.NewtonSurfacePair.Items[1]
    as TNGDSurfacePair;
  FrictionOnCube3 := GLNGDManager1.NewtonSurfacePair.Items[2]
    as TNGDSurfacePair;
  FrictionOnCube4 := GLNGDManager1.NewtonSurfacePair.Items[3]
    as TNGDSurfacePair;

  // Set SurfaceItems to SurfacePair
  ObjectOnTrampoline.SetMaterialItems(SurfaceTrampoline,
    SurfaceSphere1_Sphere2_Cube1);

  FrictionOnCube2.SetMaterialItems(SurfaceFriction, SurfaceCube2);
  FrictionOnCube3.SetMaterialItems(SurfaceFriction, SurfaceCube3);
  FrictionOnCube4.SetMaterialItems(SurfaceFriction, SurfaceCube4);

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

end.
