unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,

  GLBaseClasses,
  GLScene,
  GLCadencer,
  GLCrossPlatform,
  GLSound,
  GLSMBASS,
  GLSMFMOD,
  GLCoordinates,
  GLObjects,
  GLWin32Viewer,
  GLMPlayer;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GLCadencer1: TGLCadencer;
    GLSMBASS1: TGLSMBASS;
    SoundLib: TGLSoundLibrary;
    Button3: TButton;
    GLSMFMOD1: TGLSMFMOD;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ListBox1: TListBox;
    GroupBox2: TGroupBox;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    GLScene1: TGLScene;
    Button5: TButton;
    Button6: TButton;
    Label5: TLabel;
    ScrollBar1: TScrollBar;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    OpenDialog1: TOpenDialog;
    GroupBox3: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure N3Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    Music: TGLMusicPlayer;
    procedure AddSound;
    procedure Setmusicstart(Sender: TGLMusicPlayer);
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Setmusicstart(Sender: TGLMusicPlayer);
begin
  ListBox1.ItemIndex := Music.IndexMusic
end;

procedure TForm1.AddSound;
var
  searchRec: TSearchRec;
begin
  SetCurrentDir(ExtractFilePath(Application.ExeName));
{$WARNINGS OFF}
  FindFirst('*.*', faAnyFile, searchRec);
{$WARNINGS ON}
  repeat
    if (lowercase(extractfileext(searchRec.Name)) = '.mp3') or
      (lowercase(extractfileext(searchRec.Name)) = '.wav') then
    begin
      Music.Add(searchRec.Name);
    end;
  until FindNext(searchRec) <> 0;
  FindClose(searchRec);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  Music := TGLMusicPlayer.Create(self);
  // Music.SoundLibrary:=soundlib;
  AddSound;
  ListBox1.items := Music.PlayList;
  Music.Cadencer := GLCadencer1;
  Music.Manager := GLSMBASS1;
  Music.OnMusicStart := Setmusicstart;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Music.Destroy;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  Label2.Caption := inttostr(Music.Position);
  Label1.Caption := inttostr(Music.GetLength);
  ScrollBar1.Position := round(Music.Position * 100 / Music.GetLength);
end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Music.Position := round(Music.GetLength * ScrollBar1.Position / 100);
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  Music.IndexMusic := ListBox1.ItemIndex;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
  OpenDialog1.Execute();
  if OpenDialog1.FileName <> '' then
  begin
    Music.Add(OpenDialog1.FileName);
    ListBox1.items := Music.PlayList;
  end;
end;

procedure TForm1.N3Click(Sender: TObject);
begin
  Music.ClearLib;
  ListBox1.items := Music.PlayList;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  GLSMBASS1.Active := true;
  Music.Manager := GLSMBASS1;
  GLSMFMOD1.Active := false;
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  GLSMBASS1.Active := false;
  Music.Manager := GLSMFMOD1;
  GLSMFMOD1.Active := true;
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  Music.PlayingMode := pmFromLib;
end;

procedure TForm1.RadioButton4Click(Sender: TObject);
begin
  Music.PlayingMode := pmStream;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Music.Play;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Music.Pause;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Music.Stop;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Music.MoveUp(ListBox1.ItemIndex);
  ListBox1.items := Music.PlayList
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Music.MoveDown(ListBox1.ItemIndex);
  ListBox1.items := Music.PlayList
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Music.Delete(ListBox1.ItemIndex);
  ListBox1.items := Music.PlayList
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Music.AutoPlay := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Music.RepeatPlaying := CheckBox2.Checked;
end;

end.
