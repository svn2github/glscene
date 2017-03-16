unit uMain;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ComCtrls, Vcl.ExtCtrls,
   
  GLObjects, GLHUDObjects, GLMaterial, GLScene, GLFBORenderer, GLCoordinates,
  GLCadencer, GLCrossPlatform, GLBaseClasses, GLWin32Viewer, GLSimpleNavigation,
  GLSLShader, GLCustomShader, GLPolyhedron, GLSpaceText, GLRenderContextInfo;

type
  TForm1 = class(TForm)
    vp: TGLSceneViewer;
    GLScene1: TGLScene;
    cad: TGLCadencer;
    matlib: TGLMaterialLibrary;
    cam: TGLCamera;
    light: TGLLightSource;
    hud_res2: TGLHUDSprite;
    glsl_pass1: TGLSLShader;
    GLSimpleNavigation1: TGLSimpleNavigation;
    fbo1: TGLFBORenderer;
    fbo2: TGLFBORenderer;
    hud_res1: TGLHUDSprite;
    glsl_pass2: TGLSLShader;
    Panel1: TPanel;
    tb: TTrackBar;
    GLDodecahedron1: TGLDodecahedron;
    dc_pass1: TGLDummyCube;
    GLSpaceText1: TGLSpaceText;
    dc_cam: TGLDummyCube;
    hud_back: TGLHUDSprite;
    procedure glsl_pass1Apply(Shader: TGLCustomGLSLShader);
    procedure cadProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure glsl_pass2Apply(Shader: TGLCustomGLSLShader);
    procedure tbChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fbo2BeforeRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure fbo2AfterRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
  end;

var
  Form1: TForm1;

  bsize: single = 1 / 512;
  

implementation

{$R *.dfm}


//
// setup
//
procedure TForm1.FormCreate;
begin

  vp.BoundsRect := rect(-512, 0, 768, 512);

  clientWidth := 768;
  clientHeight := 512 + panel1.Height;

end;


//
// horizontal pass
//
procedure TForm1.glsl_pass1Apply;
begin

  with Shader do begin
    Param['RT'].AsTexture2D[0] := matlib.TextureByName('pass1');
    Param['bsize'].AsFloat := bsize;
  end;

end;


//
// vertical pass
//
procedure TForm1.glsl_pass2Apply;
begin

  with Shader do begin
    Param['RT'].AsTexture2D[0] := matlib.TextureByName('pass2');
    Param['bsize'].AsFloat := bsize;
  end;

end;


//
// cadProgress
//
procedure TForm1.cadProgress;
begin

  vp.invalidate;

end;


//
// value
//
procedure TForm1.tbChange;
begin

  bsize := tb.Position / 2048;

end;


//
// show
//
procedure TForm1.FormShow;
begin

  cad.Enabled := true;

end;


//
// before pass2
//
procedure TForm1.fbo2BeforeRender;
begin

  hud_res1.Position.X := hud_res1.Position.X - 256;
  hud_res1.Width := vp.Width; // ?? for GLSscene ver 1.1+

end;


//
// after pass2
//
procedure TForm1.fbo2AfterRender;
begin

  hud_res1.Position.X := hud_res1.Position.X + 256;
  hud_res1.Width := 256;  

end;

end.
