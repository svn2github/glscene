{ : Tentacles demo, a weird use for TGLPipe.<p>

  Serves as a test for TGLPipe's ability to have a per-node color with smooth
  interpolation between nodes, and as some kind of sickening modern art...<br>
  Position of the nodes, radius and color are updated for each frame. Note that
  the TGLPipe's ObjectStyle is altered to make it "osDirectDraw": since the geometry
  is constantly altered, it's no use compiling/saving it for the next frame,
  setting the style to "osDirectDraw" tells GLScene the object should be rendered
  directly (faster when geometry constantly changes, slower  when geometry is
  static from one frame to the other).<br>
  Try commenting out that line and see for yourself ;).
}
unit Unit1;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,

  GLScene_Material,
  GLScene_MaterialEx,
  GLScene_Cadencer,
  GLScene_Core,
  GLScene_Objects,
  GLScene_Objects_Extrusion,
  GLScene_Base_Coordinates,
  GLScene_Platform,
  GLScene_Viewer_VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DCBase: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    DCTarget: TGLDummyCube;
    Timer1: TTimer;
    Pipe1: TGLPipe;
    Pipe2: TGLPipe;
    Pipe3: TGLPipe;
    Pipe4: TGLPipe;
    Pipe5: TGLPipe;
    Sphere1: TGLSphere;
    GLMaterialLibraryEx1: TGLMaterialLibraryEx;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  GLScene_Base_Vector_Geometry, GLScene_Base_Vector_Types,
  GLScene_Base_Color;

const
  cNbNodes = 32;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, k: Integer;
  pipe: TGLPipe;
begin
  GLMaterialLibraryEx1.Materials[0].FixedFunction.VertexColorMode := vcmDiffuse;
  with Sphere1 do
    ObjectStyle := ObjectStyle + [osStreamDraw];
  // prepare the TGLPipe objects (add node, set props...)
  for k := 0 to DCBase.Count - 1 do
    if DCBase[k] is TGLPipe then
    begin
      pipe := TGLPipe(DCBase[k]);
      with pipe do
      begin
        MaterialLibrary := GLMaterialLibraryEx1;
        LibMaterialName := GLMaterialLibraryEx1.Materials[0].Name;
        Nodes.Clear;
        for i := 1 to cNbNodes do
          Nodes.AddNode(0, i / 8, 0);
        Radius := 0.1;
        // enable per-node coloring in the TGLPipe
        NodesColorMode := pncmDiffuse;
        // divisions between nodes (for spline interpolation)
        Division := 3;
        // No geometry compilation/cacheing, render directly
        // (geometry changes completely from frame to frame)
        ObjectStyle := ObjectStyle + [osStreamDraw];
      end;
    end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i, k: Integer;
  t, t1, t2, r: Double;
  pipe: TGLPipe;
begin
  t := newTime;
  for k := 0 to DCBase.Count - 1 do
    if DCBase[k] is TGLPipe then
    begin
      pipe := TGLPipe(DCBase[k]);
      with pipe.Nodes do
      begin
        BeginUpdate;
        for i := 0 to Count - 1 do
          with Items[i] as TGLPipeNode do
          begin
            // don't search any hidden logic behind the formulaes below:
            // they're just here to induce this sickening weirdo movement
            t1 := -t + i * 0.1 + k * c2PI / 5;
            r := (Sin(3 * t + k) + 2) * 0.5 * ((2 * i + Count) / Count);
            X := Cos(t1) * r;
            Z := Sin(t1) * r;
            t2 := 2 * (t + i / (Count - 1) + k);
            Color.Color := VectorLerp(clrSeaGreen, clrYellow, Sin(t2));
            RadiusFactor := (1 + (Sin(t2) * 0.5)) * ln((Count - i)) * 0.5;
          end;
        EndUpdate;
      end;
    end;
  Sphere1.Radius := 1.1 + Sin(2 * t) * 0.1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // standard FPS counter
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
