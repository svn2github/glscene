{: Setting up a simple scripted scene.<p>

   This demo shows how to manipulate GLScene objects though
   a couple of basic scripts. The TGLDWS2ActiveBehaviour is
   a behaviour style component that wraps a DelphiWebScriptII
   program and script. Once compiled and executed it will
   remain active until it is deactivated, destroyed or 
   recompiled.<p>
   
   There are a couple of procedures that the active script will
   recognize and execute while active. The OnBeginProgram and
   OnProgress procedures. If these procedures are found in the
   compiled program it will call them from the behaviour. The
   OnBeginProgram is called when the program starts, just after
   compiling. The OnProgress procedure is called when the
   behaviour progresses. The OnBeginProgram event is used here
   to grab the instance of the object being scripted and the
   OnProgress is used to manipulate the GLScene object.<p>
   
   The DWS2Program property is the compiled program, this can
   be used to call on internal variables or functions from
   Delphi. This can be used to create other custom events.<p>
   
   InvalidateScript is called after the script text is altered
   to alert the behaviour that the program needs to be 
   recompiled. OnBeginProgram will be called again once the
   program is compiled and restarted. The Active property can
   be used to halt and start DWS2Program's execution.<p>
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GLScene, GLMisc, GLObjects, GLCadencer,
  GLWin32Viewer, AsyncTimer, dws2Classes, dws2VectorGeometry,
  dws2GLScene, GLDWS2Objects, dws2Comp;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLDelphiWebScriptII1: TGLDelphiWebScriptII;
    dws2VectorGeometryUnit1: Tdws2VectorGeometryUnit;
    GLSphere1: TGLSphere;
    Panel1: TPanel;
    Button3: TButton;
    GLSphere1Script: TMemo;
    CheckBox1: TCheckBox;
    GLCube1Script: TMemo;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLCube1: TGLCube;
    dws2ClassesUnit1: Tdws2ClassesUnit;
    dws2GLSceneUnit1: Tdws2GLSceneUnit;
    Label1: TLabel;
    Label2: TLabel;
    AsyncTimer1: TAsyncTimer;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Set the initial scripts
  Button3Click(Self);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // Set GLSphere1's script and force a recompile
  with TGLDWS2ActiveBehaviour(GLSphere1.Behaviours[0]) do begin
    Script.Text:=GLSphere1Script.Lines.Text;
    InvalidateScript;
  end;

  // Set GLCube1's script and force a recompile
  with TGLDWS2ActiveBehaviour(GLCube1.Behaviours[0]) do begin
    Script.Text:=GLCube1Script.Lines.Text;
    InvalidateScript;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  GLCadencer1.Enabled:=CheckBox1.Checked;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption:='GLScene DWS2 Scripting Basics - '+GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
