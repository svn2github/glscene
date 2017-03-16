unit main;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
   
  GLScene,
  GLObjects,
  GLCadencer,
  GLUtils,
  GLTexture,
  GLUserShader,
  GLContext,
  GLGraph,
  GLVectorGeometry,
  GLVectorTypes,
  GLVectorLists,
  GLFileTGA,
  GLWin32Viewer,
  GLAsyncTimer,
  GLHUDObjects,
  GLBitmapFont,
  GLWindowsFont,
  GLVectorFileObjects,
  GLFile3DS,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLRenderContextInfo;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLSceneViewer: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    Render1: TGLDirectOpenGL;
    AsyncTimer1: TGLAsyncTimer;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    GLFreeForm1: TGLFreeForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    BrightnessTB: TTrackBar;
    ContrastTB: TTrackBar;
    Label1: TLabel;
    SaturationTB: TTrackBar;
    Label2: TLabel;
    EnabledCB: TCheckBox;
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure Render1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EnabledCBClick(Sender: TObject);
  private
    mx, my : Integer;

    procedure initialize;
  public
    ShaderProgram: TGLProgramHandle;
    ShaderTexture: TGLTextureHandle;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLMaterialLibrary1.TexturePaths:='.\Models';
  GLFreeForm1.LoadFromFile('.\Models\model.3ds');
end;

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if shift = [ssLeft]
  then GLCamera1.MoveAroundTarget(my-y, mx-x);

  mx:=x; my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
 GLSceneViewer.Invalidate;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLCadencer1.Enabled:=false;
  AsyncTimer1.Enabled:=false;
  if Assigned(ShaderProgram)
  then ShaderProgram.Free;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Caption:=Format('GLSL Video Effects [%.2f] FPS',
                  [GLSceneViewer.FramesPerSecond]);
                  GLSceneViewer.ResetPerformanceMonitor;
end;


      var initialized : boolean = false;
      procedure TForm1.initialize;
      begin
            ShaderTexture := TGLTextureHandle.Create;
            ShaderTexture.AllocateHandle;

            glEnable(GL_TEXTURE_2D);
            glBindTexture(GL_TEXTURE_2D, ShaderTexture.Handle);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S    , GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T    , GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

            glCopyTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA8,
                             0, 0,
                             GLSceneViewer.ClientWidth,
                             GLSceneViewer.ClientHeight, 0);
            glDisable(GL_TEXTURE_2D);

            initialized := True;
      end; // initialize




procedure TForm1.Render1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  // shader init
  if not Assigned(ShaderProgram) then begin
    ShaderProgram:=TGLProgramHandle.CreateAndAllocate;
    ShaderProgram.AddShader(TGLFragmentShaderHandle,
                            LoadAnsiStringFromFile('Shaders\video.frag'),
                            True);

    if not ShaderProgram.LinkProgram then raise Exception.Create(ShaderProgram.InfoLog);
    if not ShaderProgram.ValidateProgram then raise Exception.Create(ShaderProgram.InfoLog);
  end;

  // allocating texture handle
  // occurs on form creation and on every form resize
  // needed to create texture of exact size of form
  if not initialized then initialize;

  // Create snapshot
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, shaderTexture.Handle);
  glCopyTexSubImage2D(GL_TEXTURE_2D,
                      0, 0, 0, 0, 0,
                      GLSceneViewer.ClientWidth,
                      GLSceneViewer.ClientHeight);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;

  ShaderProgram.UseProgramObject;
  ShaderProgram.Uniform1i['u_texture']:=0;
  ShaderProgram.Uniform1f['saturation']:=SaturationTB.Position/100;
  ShaderProgram.Uniform1f['brightness']:=BrightnessTB.Position/100;
  ShaderProgram.Uniform1f['contrast'] := ContrastTB.Position/100;

  // drawing rectangle over screen
  glDisable(GL_DEPTH_TEST);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);   glVertex2f (-1, -1);
    glTexCoord2f(1.0, 0.0);   glVertex2f (1, -1);
    glTexCoord2f(1.0, 1.0);   glVertex2f (1, 1);
    glTexCoord2f(0.0, 1.0);   glVertex2f (-1, 1);
  glEnd;
  glEnable(GL_DEPTH_TEST);

  ShaderProgram.EndUseProgramObject;
  glDisable(GL_TEXTURE_2D);

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

///  CheckOpenGLError;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  // texture object handle is set to 0
  // for texture to be recreated on Shader rendering
  initialized := False;
  if Assigned(ShaderTexture)
  then ShaderTexture.DestroyHandle;
end;

procedure TForm1.EnabledCBClick(Sender: TObject);
begin
  Render1.Visible:=EnabledCB.Checked;
end;

end.
