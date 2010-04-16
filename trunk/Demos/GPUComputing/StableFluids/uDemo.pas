unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Graphics,
  // GLScene
  GLScene, GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses,
  GLCoordinates, GLContext, GLGui, GLBitmapFont, GLWindowsFont, GLCustomShader,
  GLRenderContextInfo, GLState,
  GLMaterial, GLSLShader, GLWindows, GL3xObjects,
  // CUDA
  GLSCUDAContext, GLSCUDA, GLSCUDACompiler, GLSCUDAFFTPlan, GLSCUDAGraphics,
  GL3xFactory;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLSCUDADevice1: TGLSCUDADevice;
    GLSCUDA1: TGLSCUDA;
    GLSCUDACompiler1: TGLSCUDACompiler;
    MainModule: TCUDAModule;
    ArrayOfTexture: TCUDAMemData;
    addForces: TCUDAFunction;
    advectVelocity: TCUDAFunction;
    diffuseProject: TCUDAFunction;
    updateVelocity: TCUDAFunction;
    advectParticles: TCUDAFunction;
    TextureOfVelocityField: TCUDATexture;
    VelocityField: TCUDAMemData;
    ComplexVXField: TCUDAMemData;
    ComplexVYField: TCUDAMemData;
    InitialPosition: TCUDAMemData;
    FluidShader: TGLSLShader;
    GLMaterialLibrary1: TGLMaterialLibrary;
    ForwardFFT: TCUDAFFTPlan;
    InverseFFT: TCUDAFFTPlan;
    SceneParticles: TGL3xFeedBackMesh;
    ParticleMapper: TCUDAGLGeometryResource;
    ResetButton: TGLButton;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLSCUDAFactory1: TGLSCUDAFactory;
    GLGuiLayout1: TGLGuiLayout;
    procedure GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure addForcesParameterSetup(Sender: TObject);
    procedure advectParticlesParameterSetup(Sender: TObject);
    procedure advectVelocityParameterSetup(Sender: TObject);
    procedure updateVelocityParameterSetup(Sender: TObject);
    procedure diffuseProjectParameterSetup(Sender: TObject);
    procedure BeforeProduce(Sender: TObject;
      AttrIndex: Integer);
    procedure FluidShaderApply(Shader: TGLCustomGLSLShader);
    procedure ResetButtonButtonClick(Sender: TObject);
  private
    { Private declarations }
    clicked: Boolean;
    lastx: Integer;
    lasty: Integer;

    ComplexPadWidth: Integer;
    RealPadWidth: Integer;
    PaddedDomainSize: Integer;

    SpeedX: Integer;
    SpeedY: Integer;
    ForceX: Single;
    ForceY: Single;
  public
    { Public declarations }
    ParticlesDim: Integer;
    DeltaTime: Single;
    ViscosityConst: Single;
    ForceScaleFactor: Single;
    ForceUpdateRadius: Integer;
    ParticlesPerThread: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  VectorGeometry, VectorTypes;

var
  InitPosition : Boolean = False;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
  pos: TVector2f;
begin
  ParticlesDim := 512;
  ComplexPadWidth := ParticlesDim div 2 + 1;
  RealPadWidth := 2 * (ParticlesDim div 2 + 1);
  PaddedDomainSize := ParticlesDim * ComplexPadWidth;
  DeltaTime := 0.09;
  ViscosityConst := 0.0025;
  ForceScaleFactor := 5.8 * ParticlesDim;
  ForceUpdateRadius := 4;
  ParticlesPerThread := 16;

  // Create initial position data at host side
  InitialPosition.Data;
  for i := 0 to InitialPosition.Height - 1 do
    for j := 0 to InitialPosition.Width - 1 do
    begin
      pos[0] := ((j + 0.5) / InitialPosition.Width) + (random - 0.5) /
        InitialPosition.Width;
      pos[1] := ((i + 0.5) / InitialPosition.Height) + (random - 0.5) /
        InitialPosition.Height;
      InitialPosition.SetElement(pos, j, i);
    end;

  SceneParticles.VertexNumber := InitialPosition.Width * InitialPosition.Height;
  clicked := false;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  lastX := X;
  lastY := Y;
  clicked := true;
  ResetButton.MouseDown(Sender, TGLMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  nx, ny: Integer;
  ddx, ddy: Integer;
begin
  // Convert motion coordinates to domain
  ForceX := lastx / GLSceneViewer1.Width;
  ForceY := lasty / GLSceneViewer1.Height;
  nx := Round(ForceX * InitialPosition.Width);
  ny := Round(ForceY * InitialPosition.Height);

  if clicked and (nx < InitialPosition.Width - ForceUpdateRadius)
    and (nx > ForceUpdateRadius - 1)
    and (ny < InitialPosition.Height - ForceUpdateRadius)
    and (ny > ForceUpdateRadius - 1) then
  begin
    ddx := X - lastx;
    ddy := Y - lasty;
    SpeedX := nx - ForceUpdateRadius;
    SpeedY := ny - ForceUpdateRadius;
    ForceX := DeltaTime * ForceScaleFactor * (ddx / GLSceneViewer1.Width);
    ForceY := DeltaTime * ForceScaleFactor * (ddy / GLSceneViewer1.Height);
    addForces.Launch(false);
    lastx := X;
    lasty := Y;
  end;

  ResetButton.MouseMove(Sender, Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clicked := false;
  ResetButton.MouseUp(Sender, TGLMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.addForcesParameterSetup(Sender: TObject);
begin
  with addForces do
  begin
    BlockShape.SizeX := 2 * ForceUpdateRadius + 1;
    BlockShape.SizeY := 2 * ForceUpdateRadius + 1;
    SetParam(VelocityField);
    SetParam(ParticlesDim);
    SetParam(ParticlesDim);
    SetParam(SpeedX);
    SetParam(SpeedY);
    SetParam(ForceX);
    SetParam(ForceY);
    SetParam(ForceUpdateRadius);
    SetParam(VelocityField.Pitch);
  end;
end;

procedure TForm1.advectVelocityParameterSetup(Sender: TObject);
begin
  VelocityField.CopyTo(ArrayOfTexture);
  with advectVelocity do
  begin
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ParticlesDim);
    SetParam(RealPadWidth);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ParticlesPerThread);
    // For texture parameter order does not matter
    SetParam(TextureOfVelocityField);
  end;
end;

procedure TForm1.diffuseProjectParameterSetup(Sender: TObject);
begin
  with diffuseProject do
  begin
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ComplexPadWidth);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ViscosityConst);
    SetParam(ParticlesPerThread);
  end;
end;

procedure TForm1.updateVelocityParameterSetup(Sender: TObject);
begin
  with updateVelocity do
  begin
    SetParam(VelocityField);
    SetParam(ComplexVXField);
    SetParam(ComplexVYField);
    SetParam(ParticlesDim);
    SetParam(RealPadWidth);
    SetParam(ParticlesDim);
    SetParam(ParticlesPerThread);
    SetParam(VelocityField.Pitch);
    SetParam(1.0 / (ParticlesDim * ParticlesDim));
  end;
end;

procedure TForm1.advectParticlesParameterSetup(Sender: TObject);
begin
  with advectParticles do
  begin
    SetParam(ParticleMapper.AttributeDataAddress[0]);
    SetParam(VelocityField);
    SetParam(ParticlesDim);
    SetParam(ParticlesDim);
    SetParam(DeltaTime);
    SetParam(ParticlesPerThread);
    SetParam(VelocityField.Pitch);
  end;
end;

procedure TForm1.BeforeProduce(Sender: TObject; AttrIndex: Integer);
begin
  if not InitPosition then
  begin
    InitialPosition.CopyTo(ParticleMapper, 0);
    VelocityField.FillMem(NullVector);
    InitPosition := true;
  end;
  {: simulate fluid }
  advectVelocity.Launch;
  ForwardFFT.Execute(ComplexVXField, ComplexVXField);
  ForwardFFT.Execute(ComplexVYField, ComplexVYField);
  diffuseProject.Launch;
  InverseFFT.Execute(ComplexVXField, ComplexVXField);
  InverseFFT.Execute(ComplexVYField, ComplexVYField);
  updateVelocity.Launch;
  {: advectParticles will be launched by GLSCUDAPrimitiveFactory1 automaticaly }
end;

procedure TForm1.ResetButtonButtonClick(Sender: TObject);
begin
  InitPosition := false;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  SceneParticles.StructureChanged;
end;

procedure TForm1.FluidShaderApply(Shader: TGLCustomGLSLShader);
begin
  with CurrentGLContext.GLStates do
  begin
    Enable(stPointSmooth);
    Enable(stBlend);
    Disable(stCullFace);
    Disable(stDepthTest);
    PointSize := 1;
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
  end;
end;

end.

