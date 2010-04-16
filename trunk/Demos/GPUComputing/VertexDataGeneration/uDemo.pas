unit uDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, GLWin32Viewer, GLCrossPlatform, BaseClasses, GLScene,
  GLSimpleNavigation, GLObjects, GL3xObjects, GLCoordinates, GLSCUDA, GLSCUDACompiler,
  GLSCUDAContext, GLContext, GLSCUDAGraphics, GLMaterial, GLCustomShader,
  GLSLShader, GL3xFactory;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLSCUDADevice1: TGLSCUDADevice;
    GLSCUDA1: TGLSCUDA;
    GLSCUDACompiler1: TGLSCUDACompiler;
    MainModule: TCUDAModule;
    DotFieldMapper: TCUDAGLGeometryResource;
    GL3xFeedbackMesh1: TGL3xFeedBackMesh;
    GLSLShader1: TGLSLShader;
    MakeDotField: TCUDAFunction;
    GLSCUDAFactory1: TGLSCUDAFactory;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure MakeVertexBufferParameterSetup(Sender: TObject);
    procedure GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
    procedure FormCreate(Sender: TObject);
    procedure GLSLShader1Apply(Shader: TGLCustomGLSLShader);
  private
    { Private declarations }
  public
    { Public declarations }
    FieldWidth: Integer;
    FieldHeight: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  OpenGL1x, VectorGeometry;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FieldWidth := 256;
  FieldHeight := 256;
  GL3xFeedbackMesh1.VertexNumber := FieldWidth * FieldHeight;
  MakeDotField.Grid.SizeX := FieldWidth div MakeDotField.BlockShape.SizeX;
  MakeDotField.Grid.SizeY := FieldWidth div MakeDotField.BlockShape.SizeY;
end;

procedure TForm1.GLSCUDA1OpenGLContextNeeded(out Context: TGLContext);
begin
  Context := GLSceneViewer1.Buffer.RenderingContext;
end;

procedure TForm1.GLSLShader1Apply(Shader: TGLCustomGLSLShader);
begin
  with GLSceneViewer1.Buffer do
    Shader.Param['ModelViewProjectionMatrix'].AsMatrix4f :=
      MatrixMultiply(ViewMatrix, ProjectionMatrix);
end;

procedure TForm1.MakeVertexBufferParameterSetup(Sender: TObject);
begin
  with MakeDotField do
  begin
    SetParam(DotFieldMapper.AttributeDataAddress[0]);
    SetParam(FieldWidth);
    SetParam(FieldHeight);
    SetParam(GLCadencer1.CurrentTime);
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
  GL3xFeedbackMesh1.StructureChanged;
end;

end.
