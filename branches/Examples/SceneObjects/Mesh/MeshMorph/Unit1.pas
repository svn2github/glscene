unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  //GLS
  GLObjects, GLGeomObjects, GLScene, GLWin32Viewer, GLCrossPlatform, GLUtils,
  GLCadencer, GLMesh, GLVectorGeometry, GLVectorTypes, GLVectorLists, GLTexture,
  GLCoordinates, GLBaseClasses, GLAsyncTimer, GLRenderContextInfo, GLContext,
  OpenGLAdapter, OpenGLTokens;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    vp: TGLSceneViewer;
    GLCube1: TGLCube;
    cam: TGLCamera;
    light: TGLLightSource;
    cad: TGLCadencer;
    GLPlane1: TGLPlane;
    mesh: TGLMesh;
    at: TGLAsyncTimer;
    dogl: TGLDirectOpenGL;
    dc_world: TGLDummyCube;
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure atTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure doglRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public

    procedure createCyl;
    procedure redrawCyl( time:single = -1 );
    procedure toggle;

  end;

var
  Form1: TForm1;

  cmesh: array of array of TGLVertexData;
  useGPUmorph: boolean;

  glsl: TGLProgramHandle;
  initDGL: boolean;


implementation

{$R *.dfm}


//
// setup
//
procedure TForm1.FormCreate;
begin
  createCyl;
end;


//
// create cylinder
//
procedure TForm1.createCyl;
const
    rseg_cnt = 50;
    hseg_cnt = 20;
    H = 20;
    R = 2;
var
    i,j: integer;
    da,dh: double;
    s: single;
    v: TVector3f;
begin
  setlength( cmesh, hseg_cnt + 1, rseg_cnt );

  da := 2 * pi / rseg_cnt;
  dh := H / hseg_cnt;

  for i := 0 to rseg_cnt - 1 do begin
    setvector( v, 0, R * sin( da * i ), R * cos( da * i ));
    s := i / rseg_cnt;
    for j := 0 to hseg_cnt do
      with cmesh[j, i] do begin
        SetVector( coord, j * dh, v.Y, v.Z );
        textCoord.S := s;
        textCoord.T := 1 - j / hseg_cnt;
        end;
    end;
  redrawCyl;
end;


//
// redraw cylinder
//
procedure TForm1.redrawCyl( time:single = -1 );
var
    i,j: integer;
    vd: TGLVertexData;
    cd: array of single;
begin
  // modify
  setlength( cd, length( cmesh ));
  for i := 0 to high( cd ) do
    if time < 0 then cd[ i ] := 0
      else cd[ i ] := sin( time + i ) / 2;

  // draw
  mesh.BeginUpdate;
  with mesh.Vertices do begin
    Clear;
    for i := 0 to high( cmesh ) - 1 do begin
      // ring
      for j := 0 to high( cmesh[ 0 ] ) do begin
        vd := cmesh[ i, j ];
        vd.coord.Y := vd.coord.Y + cd[ i ];
        AddVertex( vd );
        vd := cmesh[ i + 1, j ];
        vd.coord.Y := vd.coord.Y + cd[ i + 1 ];
        AddVertex( vd );
      end;
      // removing seam
      vd := cmesh[ i, 0 ];
      vd.coord.Y := vd.coord.Y + cd[ i ];
      vd.textCoord.s := vd.textCoord.s + 1;
      AddVertex( vd );
      vd := cmesh[ i + 1, 0 ];
      vd.coord.Y := vd.coord.Y + cd[ i + 1 ];
      vd.textCoord.s := vd.textCoord.s + 1;
      AddVertex( vd );
      AddVertex( vd );
      AddVertex( vd );
    end;
  end;
  mesh.EndUpdate;
end;

//
// doglRender
//
procedure TForm1.doglRender;
begin
  if not initDGL then begin
    if not(gl.ARB_shader_objects and gl.ARB_fragment_shader) then begin
      ShowMessage('shader not supported by your hardware');
      Halt;
      end;
    glsl := TGLProgramHandle.CreateAndAllocate;
    glsl.AddShader( TGLVertexShaderHandle, LoadAnsiStringFromFile('vp'));
    glsl.AddShader( TGLFragmentShaderHandle, LoadAnsiStringFromFile('fp'));
    if not glsl.LinkProgram then raise Exception.Create( glsl.InfoLog );
    if not glsl.ValidateProgram then raise Exception.Create( glsl.InfoLog );
    initDGL := True;
    end;
  if initDGL then begin
    glsl.UseProgramObject;
    glsl.Uniform1i['BaseTex'] := 0;
    if useGPUmorph then
      glsl.Uniform1f['time'] := cad.CurrentTime
    else begin
      glsl.Uniform1f['time'] := -1;
      redrawCyl( cad.CurrentTime );
    end;
    mesh.Render( rci );
    glsl.EndUseProgramObject;
  end;
end;

//
// cadProgress
//
procedure TForm1.cadProgress;
begin
  cam.MoveAroundTarget( deltatime * 3, deltaTime * 5 );
end;


//
// fps
//
procedure TForm1.atTimer;
var
    s: string;
begin
  if useGPUmorph then s := 'GPU'
    else s := 'CPU';
  Caption := 'MeshAnimation[' + s + ']: ' + vp.FramesPerSecondText(2) +
    ' / press any key to toggle CPU/GPU';
  vp.ResetPerformanceMonitor;
end;


//
// activate
//
procedure TForm1.FormShow;
begin
  cad.Enabled := true;
end;


//
// toggle CPU/GPU by keyboard
//
procedure TForm1.FormKeyDown;
begin
  toggle;
end;


//
// toggle CPU/GPU by mouse
//
procedure TForm1.vpMouseDown;
begin
  toggle;
end;


//
// toggle
//
procedure TForm1.toggle;
begin
  useGPUmorph := not useGPUmorph;
  if useGPUmorph then
    redrawCyl;
end;


end.
