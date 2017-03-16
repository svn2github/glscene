unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.OpenGL,
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.Jpeg,
  Vcl.ExtCtrls, Vcl.Menus,
   
  OpenGLAdapter,
  GLWin32Viewer, GLScene, GLTexture, GLObjects, GLUtils,
  GLContext, GLVectorGeometry, GLCadencer, OpenGLTokens,
  GLMaterial, GLCoordinates, GLCrossPlatform, GLBaseClasses,
  GLFileJPEG, GLRenderContextInfo;

type
  TFBumpEarth = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    MatLib: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    DOInitialize: TGLDirectOpenGL;
    DORender: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    PopupMenu: TPopupMenu;
    MIDot3: TMenuItem;
    MIParallax: TMenuItem;
    N1: TMenuItem;
    CBWireFrame: TMenuItem;
    GLLightSource2: TGLLightSource;
    procedure DOInitializeRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure DORenderRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MIDot3Click(Sender: TObject);
    procedure MIParallaxClick(Sender: TObject);
  private
     
  public
    { Publi declarations }
    mx, my, dmx, dmy: Integer;
    parallaxBumpMapping: Boolean;
  end;

var
  FBumpEarth: TFBumpEarth;

implementation

{$R *.dfm}
// ---------------------

var
  programObject: TGLProgramHandle;
  tangentBasisAttrib: Integer;
  sphereList: TGLListHandle;

function sphere_position(theta, phi: Single): TAffineVector;
begin
  Result.X := cos(phi) * cos(theta);
  Result.Y := sin(phi);
  Result.Z := -cos(phi) * sin(theta);
end;

function sphere_tangent(theta, phi: Single): TAffineVector;
begin
  Result.X := -sin(theta);
  Result.Y := 0;
  Result.Z := -cos(theta);
end;

function sphere_binormal(theta, phi: Single): TAffineVector;
begin
  Result.X := -sin(phi) * cos(theta);
  Result.Y := cos(phi);
  Result.Z := sin(phi) * sin(theta);
end;

function sphere_normal(theta, phi: Single): TAffineVector;
begin
  Result := sphere_position(theta, phi);
end;

procedure draw_sphere;
  procedure myVertexAttrib3fv(attrib: Cardinal; const v: TAffineVector);
  begin
    GL.VertexAttrib3fv(attrib, @v);
  end;

  procedure myVertex3fv(const v: TAffineVector);
  begin
    GL.Vertex3fv(@v);
  end;

const
  stacks = 20;
  slices = 40;
var
  i, j: Integer;
  t, t2, phi, phi2, s, theta: Single;
begin
  for i := 0 to stacks - 1 do
  begin
    t := i / (stacks - 1);
    t2 := (i + 1) / (stacks - 1);
    phi := pi * t - pi / 2;
    phi2 := pi * t2 - pi / 2;

    glBegin(GL_QUAD_STRIP);
    for j := 0 to slices - 1 do
    begin
      s := j / (slices - 1);
      theta := 2 * pi * s;

      myVertexAttrib3fv(tangentBasisAttrib, sphere_tangent(theta, phi2));
      myVertexAttrib3fv(tangentBasisAttrib + 1, sphere_binormal(theta, phi2));
      myVertexAttrib3fv(tangentBasisAttrib + 2, sphere_normal(theta, phi2));
      GL.TexCoord2f(s, t2);
      myVertex3fv(sphere_position(theta, phi2));

      myVertexAttrib3fv(tangentBasisAttrib, sphere_tangent(theta, phi));
      myVertexAttrib3fv(tangentBasisAttrib + 1, sphere_binormal(theta, phi));
      myVertexAttrib3fv(tangentBasisAttrib + 2, sphere_normal(theta, phi));
      GL.TexCoord2f(s, t);
      myVertex3fv(sphere_position(theta, phi));
    end;
    glEnd;
  end;
end;

procedure TFBumpEarth.DOInitializeRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  shaderData: String;
begin
  if DOInitialize.Tag <> 0 then
    Exit;
  DOInitialize.Tag := 1;

  if not(GL.ARB_shader_objects and GL.ARB_vertex_program and
    GL.ARB_vertex_shader and GL.ARB_fragment_shader) then
  begin
    ShowMessage('Your driver/hardware does not support GLSL!');
    GLSceneViewer1.Visible := False;
    Application.Terminate;
  end;

  SetCurrentDir(ExtractFilePath(Application.ExeName));

  MatLib.TextureByName('decal').Image.LoadFromFile('earth.jpg');
  MatLib.TextureByName('heightmap').Image.LoadFromFile('earth_bump.bmp');

  programObject.Free;
  programObject := TGLProgramHandle.CreateAndAllocate;

  programObject.AddShader(TGLVertexShaderHandle,
    LoadAnsiStringFromFile('bump_mapping_vertex.glsl'));

  if parallaxBumpMapping then
    shaderData := LoadAnsiStringFromFile('parallax_bump_mapping_fragment.glsl')
  else
    shaderData := LoadAnsiStringFromFile('dot3_bump_mapping_fragment.glsl');
  programObject.AddShader(TGLFragmentShaderHandle, shaderData);

  // make sure position attrib is bound to location 0
  programObject.BindAttribLocation(0, 'position');

  if not programObject.LinkProgram then
    raise EGLShader.Create(programObject.InfoLog);

  if not programObject.ValidateProgram then
    raise EGLShader.Create(programObject.InfoLog);

  // get attribute locations
  tangentBasisAttrib := programObject.GetAttribLocation('tangentBasis');

  // initialize the decal
  with MatLib.LibMaterialByName('decal') do
  begin
    GL.ActiveTexture(GL_TEXTURE0_ARB);
    GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end;

  // initialize the heightmap
  with MatLib.LibMaterialByName('normalmap') do
  begin
    GL.ActiveTexture(GL_TEXTURE2_ARB);
    GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
    GL.ActiveTexture(GL_TEXTURE0_ARB);
  end;

  // initialize the heightmap
  if parallaxBumpMapping then
    with MatLib.LibMaterialByName('heightmap') do
    begin
      GL.ActiveTexture(GL_TEXTURE1_ARB);
      GL.BindTexture(GL_TEXTURE_2D, Material.Texture.Handle);
      GL.ActiveTexture(GL_TEXTURE0_ARB);
    end;

  programObject.UseProgramObject;

  programObject.Uniform1i['decalMap'] := 0;
  if parallaxBumpMapping then
    programObject.Uniform1i['heightMap'] := 1;
  programObject.Uniform1i['normalMap'] := 2;

  sphereList := TGLListHandle.Create;
  sphereList.AllocateHandle;
  sphereList.NewList(GL_COMPILE);
  draw_sphere;
  sphereList.EndList;

  programObject.EndUseProgramObject;

  // CheckOpenGLError;
end;

procedure TFBumpEarth.DORenderRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  light, eye: TAffineVector;
  mat: TMatrix;
begin
  programObject.UseProgramObject;

  SetVector(light, GLLightSource1.AbsolutePosition);
  NormalizeVector(light);

  SetVector(eye, GLCamera1.AbsolutePosition);

  // set vertex program inputs
  programObject.Uniform3f['light'] := light;

  GL.GetFloatv(GL_MODELVIEW_MATRIX, @mat);
  InvertMatrix(mat);
  programObject.UniformMatrix4fv['modelViewI'] := mat;

  if CBWireFrame.Checked then
    glPolygonMode(GL_FRONT, GL_LINE)
  else
    glPolygonMode(GL_FRONT, GL_FILL);

  sphereList.CallList;

  programObject.EndUseProgramObject;
  // CheckOpenGLError;
end;

procedure TFBumpEarth.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Inc(dmx, mx - X);
    Inc(dmy, my - Y);
  end;
  mx := X;
  my := Y;
end;

procedure TFBumpEarth.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if (dmx <> 0) or (dmy <> 0) then
  begin
    GLCamera1.MoveAroundTarget(dmy * 0.5, dmx * 0.5);
    dmx := 0;
    dmy := 0;
  end;
  GLSceneViewer1.Invalidate;
end;

procedure TFBumpEarth.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFBumpEarth.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := ClientWidth * 0.25;
end;

procedure TFBumpEarth.MIDot3Click(Sender: TObject);
begin
  parallaxBumpMapping := False;
  DOInitialize.Tag := 0;
end;

procedure TFBumpEarth.MIParallaxClick(Sender: TObject);
begin
  parallaxBumpMapping := True;
  DOInitialize.Tag := 0;
end;

end.
