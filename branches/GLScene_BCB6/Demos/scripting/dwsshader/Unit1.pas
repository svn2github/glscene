{: Scripting a Shader with DelphiWebScriptII<p>

   A very simple example of how the GLUserShader and DWS2 components
   can be used to build a scripted material shader.<p>
   
   The Tdws2OpenGL1xUnit requires the Tdws2VectorGeometryUnit to be 
   associated with the script.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCadencer, StdCtrls, GLTexture, GLUserShader, ExtCtrls,
  GLWin32Viewer, GLMisc, GLScene, GLObjects, dws2Exprs, dws2Comp, dws2OpenGL1x,
  dws2VectorGeometry, AsyncTimer, GLDWS2Objects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    dws2OpenGL1xUnit1: Tdws2OpenGL1xUnit;
    GLUserShader1: TGLUserShader;
    ShaderScript: TMemo;
    Recompile: TButton;
    Enabled: TCheckBox;
    Label1: TLabel;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit;
    AsyncTimer1: TAsyncTimer;
    GLDelphiWebScriptII1: TGLDelphiWebScriptII;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure RecompileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLUserShader1DoApply(Sender: TObject;
      var rci: TRenderContextInfo);
    procedure GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
      var rci: TRenderContextInfo; var Continue: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure EnabledClick(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
    ShaderProgram : TProgram;
    Compiled : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Compile the program when the form is created
  RecompileClick(nil);
end;

procedure TForm1.RecompileClick(Sender: TObject);
begin
  // Free the compiled program if it already exists
  FreeAndNil(ShaderProgram);

  // Compile the script
  ShaderProgram:=GLDelphiWebScriptII1.Compile(ShaderScript.Lines.Text);
  Compiled:=(ShaderProgram.ProgramState = psReadyToRun);

  // Start the program
  if Compiled then
    ShaderProgram.BeginProgram(False)
  else
    Application.MessageBox('The script failed to compile.', 'Error', MB_OK);
end;

procedure TForm1.GLUserShader1DoApply(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  // Call the scripted DoApply procedure to handle the shader application
  if Compiled then
    try
      ShaderProgram.Info.Func['DoApply'].Call;
    except
      on E:Exception do begin
        Application.MessageBox(PChar(E.Message), 'Error', MB_OK);
        Compiled:=False;
      end;
    end;
end;

procedure TForm1.GLUserShader1DoUnApply(Sender: TObject; Pass: Integer;
  var rci: TRenderContextInfo; var Continue: Boolean);
begin
  Continue:=False;

  // Call the scripted DoUnApply function to handle the shader unapplication
  // pass the result of the scripted function to the Continue variable
  if Compiled then
    try
      Continue:=ShaderProgram.Info.Func['DoUnApply'].Call([Pass]).Value;
    except
      on E:Exception do begin
        Application.MessageBox(PChar(E.Message), 'Error', MB_OK);
        Compiled:=False;
      end;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Cleanup
  FreeAndNil(ShaderProgram);
end;

procedure TForm1.EnabledClick(Sender: TObject);
begin
  GLUserShader1.Enabled:=Enabled.Checked;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:=GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx:=x;
  my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my-y, mx-x);
  mx:=x;
  my:=y;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
