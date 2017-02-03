unit ShortestU;
{ This program shows how simple it is to set up a fractal landscape. I've commented
  the objects created at design time.
  Basically, you need a SceneViewer, a Scene, a Camera, a MaterialLibrary and
  a TerrainRenderer; make the latter child of a DummyCube if you want to be able
  to rescale it (to create wider perspective). All these objects must be linked
  properly as shown in other tutorials.

  This code just build and display a landscape. You can't navigate it nor rotate the angle
  of view.

  Alexandre Hirzel, July 2003
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  // GLS
  GLScene, GLTerrainRenderer, GLObjects, GLTexture, GLWin32Viewer,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses,
  // Unit where the random landscape are coded
  ahGLrandomHDS;

type
  TForm1 = class(TForm)
    // These are the minimum GLScene object needed.
    // Built àt design time
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary; // Linked to the terrain renderer
    GLCamera1: TGLCamera; // Linked to the Scene Viewer
    GLDummyCube1: TGLDummyCube;
    // Contains the terrain renderer. Allows to change its scale
    GLTerrainRenderer1: TGLTerrainRenderer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    hdsLandscape: TGLFractalHDS; // Declare the landscape manually
    mx, my: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Setting up terrain renderer. This could be done at design time but you have
    missed it. These transformations are needed because, in a HDS, the z vector
    is pointing upward. }
  GLTerrainRenderer1.Up.SetVector(0, 0, 1);
  GLTerrainRenderer1.Direction.SetVector(0, 1, 0);

  { Position the camera to have an interesting view }
  GLCamera1.Position.SetPoint(-64, 16, 64);

  { Creation and minimal set-up of a fractal landscape }
  hdsLandscape := TGLFractalHDS.Create(Self);
  with hdsLandscape do
  begin
    TerrainRenderer := GLTerrainRenderer1;
    Depth := 8;
    // 3 < Depth < 10,  Not needed but gives a nicer landscape (default depth is 4)
    Cyclic := True; // Not needed but give a "infinite" landscape
    BuildLandscape; // Build a landscape with the default parameters
  end; // with
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  if ssRight in Shift then
    GLCamera1.MoveTargetInEyeSpace((Y - my) * 0.05, (mx - X) * 0.05, 0);
  mx := X;
  my := Y;
end;

end.
