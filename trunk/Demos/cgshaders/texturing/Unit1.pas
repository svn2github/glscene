{
  Cg Multi-Texturing Demo

  Shows how to do texture coordinate shifting with a VP and blending with a FP.

  08/07/03 - Nelson Chu
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLObjects, GLMisc, GLWin32Viewer, GLTexture, OpenGL12,
  GLCgShader, Cg, cgGL, StdCtrls, Geometry, GLCadencer, ExtCtrls, ComCtrls,
  GLGraph, jpeg;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    CgShader1: TCgShader;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Splitter1: TSplitter;
    Panel2: TPanel;
    CBVertexProgram: TCheckBox;
    LabelVertProfile: TLabel;                       
    Panel4: TPanel;
    LabelFragProfile: TLabel;
    CheckBox1: TCheckBox;
    Splitter2: TSplitter;
    Panel6: TPanel;
    Panel7: TPanel;
    MemoFragCode: TMemo;
    Panel8: TPanel;
    Memo3: TMemo;
    Panel3: TPanel;
    ButtonApplyFP: TButton;
    Panel11: TPanel;
    Panel12: TPanel;
    MemoVertCode: TMemo;
    Panel13: TPanel;
    ButtonApplyVP: TButton;
    Splitter3: TSplitter;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    Button4: TButton;
    Panel9: TPanel;
    Panel10: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    Timer1: TTimer;
    GLXYZGrid1: TGLXYZGrid;
    GLPlane1: TGLPlane;
    GLMatLib: TGLMaterialLibrary;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    TrackBar2: TTrackBar;
    Label5: TLabel;
    TrackBar3: TTrackBar;
    Label6: TLabel;
    TrackBar4: TTrackBar;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    TrackBar8: TTrackBar;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure CgShader1ApplyVertexProgram(Sender: TCgProgram);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure CBVertexProgramClick(Sender: TObject);
    procedure CBFragmentProgramClick(Sender: TObject);
    procedure ButtonApplyFPClick(Sender: TObject);
    procedure MemoFragCodeChange(Sender: TObject);
    procedure MemoVertCodeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonApplyVPClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CgShader1ApplyFragmentProgram(Sender: TCgProgram);
    procedure CgShader1UnApplyFragmentProgram(Sender: TCgProgram);
    procedure CgShader1Initialize(Sender: TCustomCgShader);
  private
    { Private declarations }
  public
    { Public declarations }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // load Cg proggy
  with CgShader1 do begin
    VertexProgram.LoadFromFile('cg_texture_vp.cg');
    MemoVertCode.Lines.Assign(VertexProgram.Code);

    FragmentProgram.LoadFromFile('cg_texture_fp.cg');
    MemoFragCode.Lines.Assign(FragmentProgram.Code);
  end;

  with GLMatLib do begin
    Materials[0].Material.Texture.Image.LoadFromFile('moon.bmp');
    Materials[1].Material.Texture.Image.LoadFromFile('clover.jpg');
    Materials[2].Material.Texture.Image.LoadFromFile('marbletiles.jpg');
    Materials[3].Material.Texture.Image.LoadFromFile('chrome_buckle.bmp');
  end;
end;

procedure TForm1.CgShader1Initialize(Sender: TCustomCgShader);
// Set parameters that need to be set once
begin
  with Sender.FragmentProgram, GLMatLib do begin
    ParamByName('Map0').SetAsTexture2D(Materials[0].Material.Texture.Handle);
    ParamByName('Map1').SetAsTexture2D(Materials[1].Material.Texture.Handle);
    ParamByName('Map2').SetAsTexture2D(Materials[2].Material.Texture.Handle);
    ParamByName('Map3').SetAsTexture2D(Materials[3].Material.Texture.Handle);
  end;

  LabelVertProfile.Caption:='Using profile: ' + Sender.VertexProgram.GetProfileString;
  LabelFragProfile.Caption:='Using profile: ' + Sender.FragmentProgram.GetProfileString;
end;

procedure TForm1.CgShader1ApplyVertexProgram(Sender: TCgProgram);
var v : TVector;

  function conv(TrackBar : TTrackBar): single;
  var half : integer;
  begin
    half:=TrackBar.max div 2;
    result:= (TrackBar.Position-half) / half;
  end;

begin
  with Sender.ParamByName('ModelViewProj') do
    SetAsStateMatrix( CG_GL_MODELVIEW_PROJECTION_MATRIX, CG_GL_MATRIX_IDENTITY);

  v:= vectormake( conv(TrackBar1), conv(TrackBar2), conv(TrackBar3), conv(TrackBar4) );

  Sender.ParamByName('shifts').SetAsVector(v);
end;

procedure TForm1.CgShader1ApplyFragmentProgram(Sender: TCgProgram);
var v : TVector;

  function conv(TrackBar : TTrackBar): single;
  var half : integer;
  begin
    half:=TrackBar.max div 2;
    result:= (TrackBar.Position-half) / half;
  end;

begin
  Sender.ParamByName('Map0').EnableTexture;
  Sender.ParamByName('Map1').EnableTexture;
  Sender.ParamByName('Map2').EnableTexture;
  Sender.ParamByName('Map3').EnableTexture;

  v:= vectormake( conv(TrackBar5), conv(TrackBar6), conv(TrackBar7), conv(TrackBar8) );

  Sender.ParamByName('weights').SetAsVector(v);
end;

procedure TForm1.CgShader1UnApplyFragmentProgram(Sender: TCgProgram);
begin
  with Sender do begin
    ParamByName('Map0').DisableTexture;
    ParamByName('Map1').DisableTexture;
    ParamByName('Map2').DisableTexture;
    ParamByName('Map3').DisableTexture;
  end;
end;

// Code below takes care of the UI

procedure TForm1.CBVertexProgramClick(Sender: TObject);
begin
   CgShader1.VertexProgram.Enabled:=(sender as TCheckBox).checked;
end;

procedure TForm1.CBFragmentProgramClick(Sender: TObject);
begin
   CgShader1.FragmentProgram.Enabled:=(sender as TCheckBox).checked;
end;

procedure TForm1.ButtonApplyFPClick(Sender: TObject);
begin
  CgShader1.FragmentProgram.Code:=MemoFragCode.Lines;
  (Sender as TButton).Enabled:=false;
end;

procedure TForm1.ButtonApplyVPClick(Sender: TObject);
begin
  CgShader1.VertexProgram.Code:=MemoVertCode.Lines;
  (Sender as TButton).Enabled:=false;
end;

procedure TForm1.MemoFragCodeChange(Sender: TObject);
begin
  ButtonApplyFP.Enabled:=true;
end;

procedure TForm1.MemoVertCodeChange(Sender: TObject);
begin
  ButtonApplyVP.Enabled:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListParameters(Memo1.Lines);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListParameters(Memo3.Lines);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CgShader1.FragmentProgram.ListCompilation(Memo3.Lines);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CgShader1.VertexProgram.ListCompilation(Memo1.Lines);
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
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with GLSceneViewer1 do
    if PtInRect(ClientRect, ScreenToClient(MousePos)) then begin
      GLCamera1.SceneScale:=GLCamera1.SceneScale * (1000 - WheelDelta) / 1000;
      Handled:=true;
    end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  with GLSceneViewer1 do begin
    caption:=Format('Cg Shader Demo - %.1f fps', [FramesPerSecond]);
    ResetPerformanceMonitor;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#27 then close;
end;

end.
