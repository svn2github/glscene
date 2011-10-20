{ : A sample for the Gui interface system.<p>

  To use the Gui you must place a TGLGUILayout component on your form,
  this is a storage for Gui layouts within a single texture, each layout
  is identified by a name. All Layouts can be used with any Gui Component,
  however features like a forms title might look awkward on a checkbox...

  For correct usage a Material Library should be used.
  Both the TGLGUILayout and each gui component should have the same
  material from the material library.

  A BitmapFont or WindowsFont should also be used and applied to both
  layout and components.

  To Interface correctly, handlers must send the mouse and key events
  to a root gui component, this can be any of the keyboard aware gui
  components or a RootControl(a tramsparent component with no other purpose)
  Plus the root gui component should have its DoChanges invoked, this
  makes form movement and other mouse/key events changes fluintly
  match the rendering process, eg no flicker.

  All other Gui component must be below the root control in the GLScene
  hierachy, this is a rule which allows you to seperate gui from the
  rest of glscene and save some clock cycles on <tab> operations.
  Plus mouse events...

  For a more thorough look on the gui please check out my article on
  the glscene back bone: http://www.ting.it/gls.dll
  in the "Source" - "Core" Section called "An Overview: GLScene GUI"

  <b>History : </b><font size=-1><ul>
  <li>27/09/02 - JAJ - Creation
  </ul></font>
}
unit Unit1;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
  GLScene.BitmapFont,
  GLScene.BitmapFont.System,
  GLScene.Material,
  GLScene.GUI,
  GLScene.Cadencer,
  GLScene.Core,
  GLScene.GUI.Windows,
  GLScene.Base.Coordinates,
  GLScene.Platform,
  GLScene.Viewer.VCL;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLLightSource1: TGLLightSource;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    Font1: TMenuItem;
    WindowsFont1: TMenuItem;
    FontDialog1: TFontDialog;
    GLGuiLayout1: TGLGuiLayout;
    GLForm1: TGLForm;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLButton1: TGLButton;
    GLEdit1: TGLEdit;
    GLLabel1: TGLLabel;
    GLSystemBitmapFont1: TGLSystemBitmapFont;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure WindowsFont1Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GLButton1ButtonClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLForm1.DoChanges;
  // make things move a little
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.WindowsFont1Click(Sender: TObject);
begin
  FontDialog1.Font := GLSystemBitmapFont1.Font;
  if FontDialog1.Execute then
    GLSystemBitmapFont1.Font := FontDialog1.Font;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GLForm1.MouseDown(Sender, TGLMouseButton(Button), Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  GLForm1.MouseMove(Sender, Shift, X, Y);
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GLForm1.MouseUp(Sender, TGLMouseButton(Button), Shift, X, Y);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  GLForm1.Caption := 'Unicode caption...'#$0699#$069A#$963f#$54c0;
  GLSystemBitmapFont1.EnsureString(GLForm1.Caption);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  GLForm1.KeyDown(Sender, Key, Shift);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  GLForm1.KeyPress(Sender, Key);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  GLForm1.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.GLButton1ButtonClick(Sender: TObject);
var
  OldCaption: string;
begin
  OldCaption := GLForm1.Caption;
  GLForm1.Caption := GLEdit1.Caption;
  GLEdit1.Caption := OldCaption;
end;

end.
