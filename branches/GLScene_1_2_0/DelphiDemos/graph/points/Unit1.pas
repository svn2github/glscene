{ : Demo of the TGLPoints component.<p>

  The component is specialized in rendering large numbers of points,
  with ability to adjust point style (from fast square point to smooth
  round points) and point parameters.<p>
  The point parameters define how point size is adjusted with regard
  to eye-point distance (to make farther points smaller, see ARB_point_parameters
  for more details).<p>
  The component is also suitable for particle systems, but offers less
  flexibility than the TGLParticleFX.
}
unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,

  GLScene.Material,
  GLScene.MaterialEx,
  GLScene.Cadencer,
  GLScene.Core,
  GLScene.Objects,
  GLScene.Base.Coordinates,
  GLScene.Platform,
  GLScene.Viewer.VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLPoints1: TGLPoints;
    GLCadencer1: TGLCadencer;
    GLPoints2: TGLPoints;
    Panel1: TPanel;
    CBPointParams: TCheckBox;
    CBAnimate: TCheckBox;
    Timer1: TTimer;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CBPointParamsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my: Integer end;

  var
    Form1: TForm1;

implementation

{$R *.DFM}

uses
  GLScene.Base.GLStateMachine,
  GLScene.Base.Vector.Geometry,
  GLScene.Base.Vector.Lists,
  GLScene.Base.Color;

const
  cNbPoints = 180;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with GLMaterialLibraryEx1.Materials[0].FixedFunction do
  begin
    MaterialOptions := [moNoLighting];
    PointProperties.Enabled := True;
    PointProperties.Smooth := True;
    PointProperties.Size := 10;
    PointProperties.DistanceAttenuation.SetVector(-1.5, 0.7, 0);
    BlendingMode := bmCustom;
    BlendingParams.UseAlphaFunc := True;
    BlendingParams.AlphaFunctType := cfNotEqual;
    BlendingParams.AlphaFuncRef := 0.0;
    BlendingParams.UseBlendFunc := True;
    BlendingParams.BlendFuncSFactor := bfSrcAlpha;
    BlendingParams.BlendFuncDFactor := bfOne;
    DepthProperties.DepthWrite := False;
  end;

  with GLMaterialLibraryEx1.Materials[1].FixedFunction do
  begin
    MaterialOptions := [moNoLighting];
    PointProperties.Enabled := True;
    PointProperties.Smooth := True;
    PointProperties.Size := 20;
    PointProperties.DistanceAttenuation.SetVector(-1.5, 0.7, 0);
    BlendingMode := bmCustom;
    BlendingParams.UseAlphaFunc := True;
    BlendingParams.AlphaFunctType := cfNotEqual;
    BlendingParams.AlphaFuncRef := 0.0;
    BlendingParams.UseBlendFunc := True;
    BlendingParams.BlendFuncSFactor := bfSrcAlpha;
    BlendingParams.BlendFuncDFactor := bfOne;
    DepthProperties.DepthWrite := False;
  end;

  GLPoints1.MaterialLibrary := GLMaterialLibraryEx1;
  GLPoints1.LibMaterialName := GLMaterialLibraryEx1.Materials[0].Name;
  GLPoints2.MaterialLibrary := GLMaterialLibraryEx1;
  GLPoints2.LibMaterialName := GLMaterialLibraryEx1.Materials[1].Name;

  // allocate points in the 1st point set
  GLPoints1.Positions.Count := cNbPoints;
  // specify white color for the 1st point set
  // (if a single color is defined, all points will use it,
  // otherwise, it's a per-point coloring)
  GLPoints1.Colors.Add(clrWhite);
  // specify blue color for the 2nd point set
  GLPoints2.Colors.Add(clrBlue);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i: Integer;
  f, a, ab, ca, sa: Single;
  p: TAffineVectorList;
  v: TAffineVector;
begin
  if CBAnimate.Checked then
  begin
    // update the 1st point set with values from a math func
    f := 1 + Cos(newTime);
    p := GLPoints1.Positions;
    ab := newTime * 0.1;
    for i := 0 to cNbPoints - 1 do
    begin
      a := DegToRad(4 * i) + ab;
      SinCos(a, sa, ca);
      v[0] := 2 * ca;
      v[1] := 2 * Cos(f * a);
      v[2] := 2 * sa;
      p[i] := v;
    end;
    // replicate points in second set
    GLPoints2.Positions := GLPoints1.Positions;
    GLPoints1.StructureChanged;
  end;
  GLSceneViewer1.Invalidate;
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
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.CBPointParamsClick(Sender: TObject);
begin
  case CBPointParams.Checked of
    True:
      begin
        with GLMaterialLibraryEx1.Materials[0].FixedFunction do
          PointProperties.DistanceAttenuation.SetVector(-1.5, 0.7, 0);
        with GLMaterialLibraryEx1.Materials[1].FixedFunction do
          PointProperties.DistanceAttenuation.SetVector(-1.5, 0.7, 0);
      end;

    False:
      begin
        with GLMaterialLibraryEx1.Materials[0].FixedFunction do
          PointProperties.DistanceAttenuation.SetVector(XVector);
        with GLMaterialLibraryEx1.Materials[1].FixedFunction do
          PointProperties.DistanceAttenuation.SetVector(XVector)
      end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
