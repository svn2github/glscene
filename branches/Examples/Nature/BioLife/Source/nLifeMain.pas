{*******************************************************}
{                                                       }
{                      Tachyon Unit                     }
{    Vector Raster Geographic Information Synthesis     }
{                     VOICE  ..  Tracer                 }
{                     GRIP ICE .. Tongs                 }
{                Digital Terrain Mapping                }
{               Image Locatable Holographics            }
{                          SOS MAP                      }
{  Surreal Object Synthesis Multimedia Analysis Product }
{                   Fractal3D  Life MOW                 }
{       Copyright (c) 1995,2006  Ivan Lee Herring       }
{                                                       }
{*******************************************************}
unit nLifeMain;
{renamed neuralife 26mar01
59417
733772
17557
933376

16618   60857
241028   748528
3061      17633
546816    1110016
}

(*MATFA BOIDS
  ufrmBoids in 'ufrmBoids.pas' {frmBoids},
  uBoids in 'uBoids.pas',
  uBoidEngine in 'uBoidEngine.pas',
  ufrmAbout in 'ufrmAbout.pas' {frmAbout},
  StrFunctions in 'StrFunctions.pas',
  uTMovable in 'uTMovable.pas',
  uTMovableEngine in 'uTMovableEngine.pas',
  ufrmSettings in 'ufrmSettings.pas' {frmSettings};
  TV,{:Array3924;}
  TV7848,{:Array7848;}
  TV15696,{:Array15696;
  SetLength(Aa,156,96);
  SetLength(Image,156,96);
  SetLength(Image78,78,48);
  SetLength(TV15696,156,96);
  ImageByte:Byte;
        FOR I := 0 TO 155 DO FOR J := 0 TO 95 DO
        begin
         read( fil, ImageByte );
         Image[I,J]:=ImageByte;
        end;

        FOR I := 0 TO 155 DO FOR J := 0 TO 95 DO
        begin
         ImageByte:=Image[I,J];
         write( fil, ImageByte );
        end; }  *)
interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls;

type
  TLifeMainForm = class(TForm)
    ButtonPanel: TPanel;
    LifeBtn: TSpeedButton;
    GRFBtn: TSpeedButton;
    TermitesBtn: TSpeedButton;
    Birds: TSpeedButton;
    HelpBtn: TSpeedButton;
    AboutBtn: TSpeedButton;
    AntsBtn: TSpeedButton;
    LifeHints: TPanel;
    Fish: TSpeedButton;
    LifeEditorBtn: TSpeedButton;
    LifeColorChoiceBtn: TSpeedButton;
    TorsoBtn: TSpeedButton;
    MainMenu1: TMainMenu;
    Life1: TMenuItem;
    Edit1: TMenuItem;
    Colors1: TMenuItem;
    GRF1: TMenuItem;
    Term1: TMenuItem;
    Antz1: TMenuItem;
    Bird1: TMenuItem;
    Fish1: TMenuItem;
    Bod1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ExitBtn: TSpeedButton;
    Tools1: TMenuItem;
    ToolsBtn: TSpeedButton;
procedure DisplaySplashScreen;
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure LifeBtnClick(Sender: TObject);
    procedure GRFBtnClick(Sender: TObject);
    procedure TermitesBtnClick(Sender: TObject);
    procedure AntsBtnClick(Sender: TObject);
    procedure BirdsClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure LifeEditorBtnClick(Sender: TObject);
    procedure LifeColorChoiceBtnClick(Sender: TObject);
    procedure TorsoBtnClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ToolsBtnClick(Sender: TObject);
    procedure Bod1Click(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure Bird1Click(Sender: TObject);
    procedure FishClick(Sender: TObject);
    procedure Fish1Click(Sender: TObject);
    procedure GRF1Click(Sender: TObject);
    procedure Antz1Click(Sender: TObject);
    procedure Term1Click(Sender: TObject);
    procedure Life1Click(Sender: TObject);
  private
     
  public
     
  end;

var
  LifeMainForm: TLifeMainForm;

implementation

uses
  nUGlobal,
  AllSplash, {Life Main 0 } {SystemInfoForm  13000.... }
  GlsAbout,{AboutBoids 222 }
  ncLife,{1000} nColors,{3000} nEditor,{4000} nEP,{4500}
  nlife3d,{ 1800}  nLifeFiles,{2001}{aUniversesForm 1856 }
  nGRF,{5000} nGSRShow,
  nTermite,{ 6000 }
  nAntsFarm,{ 7000}
  nTorso,{ 9000 }
  {nDolphin,}
  GlsUvMapFrm,
  GlsTreeFrm, {  9900 }
  GlsBirdFrm, {8000, but uses its own help file}
              {AAADemoForm  8050 }
  nUBirdDX,{ 8000 also } GlsFrenzy{ 8100 };
{$R *.DFM}

procedure TLifeMainForm.DisplaySplashScreen;
begin
  SplashScreen := TSplashScreen.Create(Application);
  SplashScreen.SplashSet(5,2000);
  SplashScreen.Show;
  SplashScreen.Refresh;
end;

procedure TLifeMainForm.FormCreate(Sender: TObject);
begin {Menu}
///  DisplaySplashScreen;
{LOAD FRACTALS.F3D and SET these}
{  if FileExists(ExtractFilePath(ParamStr(0)) + 'nlife.pof') then
  begin
    DoLoader;
    LifeHints.Caption := HiddenString;
  end else
}
  begin
    nLifeDir := ExtractFilePath(ParamStr(0));
    ProLoaded := ExtractFilePath(ParamStr(0));
    Desolate := clSilver;
    Growing := clYellow;
    Full := clGreen;
    Overpopulated := clRed;
    HiddenString := 'Life Copyright, 2003: Ivan Lee Herring';
    StartedNameNumber := 'Not Registered'; {F3197432-460875}
    Started := Now;
    Colorreg := 3;
    NumberEditStored:=427;
    MaxRandomCellsEditStored:=1000;
    LifeMainFormX := 10;
    life3dFormX:=123;
    life3dFormY:=123;
    aUniversesFormX:=123;
    aUniversesFormY:=123;
    LifeFormX := 123;
    EPFormX := 123;
    ColorsFormX := 123;
    EditorFormX := 123;
    AboutFormX := 123;
    GRFFormX := 123;
    TermiteFormX := 123;
    AntsFarmX := 123;
    BirdsFormX := 123;
    DXTorsoFormX := 123;
    NewRulesFormX := 123;
    FamilyFormY := 123;
    LifeFilesFormX := 123;
    GSRShowFormX := 123;
    BirdDXFormX := 123;
    FrenzyFormY := 123;
    frmBoidsY := 123;
    frmBoidsX := 123;
    SystemInfoFormX := 123;
    MessageX := 123;
    MessageY := 123;
    SystemInfoFormY := 123;
    FrenzyFormX := 123;
    BirdDXFormY := 123;
    GSRShowFormY := 123;
    LifeFilesFormY := 123;
    FamilyFormX := 123;
    LifeMainFormY := 10;
    LifeFormY := 123;
    EPFormY := 123;
    ColorsFormY := 123;
    EditorFormY := 123;
    AboutFormY := 123;
    GRFFormY := 123;
    TermiteFormY := 123;
    AntsFarmY := 123;
    BirdsFormY := 123;
    DXTorsoFormY := 123;
    NewRulesFormY := 123;
{  DisplayButtonsOn:=True;
  DisplayBoth:=True;}
    GetPreferences;
///    DoSaver;
  end;

  top := LifeMainFormY;
  left := LifeMainFormX;
  BirdLifeDir := ExtractFilePath(ParamStr(0));
{  Height:= 88;
  Width:= 400; }
  ReallyGone := False;
{  FrenzyFilesLoaded := False; }
  Application.OnHint := ShowHint;
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'Nlife.hlp';
  HasTrueColors := myLoadImage;
{If (Not myLoadImage) then Application.Terminate;}
  SetLength(Universal,2,26);
end;
procedure TLifeMainForm.FormShow(Sender: TObject);
begin
Application.OnHint := LifeMainForm.ShowHint;
end;
procedure TLifeMainForm.ShowHint(Sender: TObject);
begin
  LifeHints.Caption := Application.Hint;
end;

procedure TLifeMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{  If ColorFormInitialized then  DColorForm.Scene.free;}
{Get the X,Y locations}
  LifeMainFormY := LifeMainForm.top;
  LifeMainFormX := LifeMainForm.left;
  bIamDone := True;
  RabbitsRunning := False;
  BirdLandDX := True;
  OrthoClor := True;
  AntiOrtho := True;
  ReallyGone := True;
  Application.ProcessMessages;
  Application.ProcessMessages;
  AAABirdForm.ReallyClose;
  BirdDXForm.ReallyClose;
  FrenzyForm.ReallyClose;  {BirdsForm.ReallyClose;}{frmBoids.ReallyClose; }
  GSRShowForm.ReallyClose;
  GRFForm.ReallyClose;

  LifeForm.ReallyClose;
  ColorsForm.ReallyClose;
  EditorPlacer.ReallyClose;
  EditorForm.ReallyClose; {NewRulesForm.ReallyClose;}
  LifeFilesForm.ReallyClose;{AboutLife.ReallyClose;}

{TermiteMoundForm.ReallyClose;}
  TermitesForm.ReallyClose;
{AntMoundForm.ReallyClose; }
  AntsForm.ReallyClose;

{DXPDXTorsoFormYForm.ReallyClose;}
  DoSaver;
  Application.ProcessMessages;
  Application.ProcessMessages;
  Action := caFree;
{CanClose:=True;}
end;

procedure TLifeMainForm.ExitBtnClick(Sender: TObject);
begin
  bIamDone := True;
  Application.ProcessMessages;
  RabbitsRunning := False;
  Application.ProcessMessages;
  BirdLandDX := True;
  Application.ProcessMessages;
  OrthoClor := True;
  Application.ProcessMessages;
  AntiOrtho := True;
  Application.ProcessMessages;
  Close;
end;

procedure TLifeMainForm.LifeBtnClick(Sender: TObject);
begin
  alife3dForm.Show;{1800}
end;

procedure TLifeMainForm.Life1Click(Sender: TObject);
begin
  LifeForm.Show;{1000}
end;

procedure TLifeMainForm.LifeEditorBtnClick(Sender: TObject);
begin
  EditorForm.Show;
end;

procedure TLifeMainForm.LifeColorChoiceBtnClick(Sender: TObject);
begin
  ColorsForm.Show;{2 and 3 D}
end;

procedure TLifeMainForm.TermitesBtnClick(Sender: TObject);
begin
  TermitesForm.Show;  {3 D not done}
end;
procedure TLifeMainForm.Term1Click(Sender: TObject);
begin
 TermitesForm.Show;
end;

procedure TLifeMainForm.AntsBtnClick(Sender: TObject);
begin
  AntsForm.Show;      {3 D not done}
end;
procedure TLifeMainForm.Antz1Click(Sender: TObject);
begin
  AntsForm.Show;
end;

procedure TLifeMainForm.GRFBtnClick(Sender: TObject);
begin
  GRFForm.Show;  //3 D not done
end;
procedure TLifeMainForm.GRF1Click(Sender: TObject);
begin
  GRFForm.Show;
end;


procedure TLifeMainForm.BirdsClick(Sender: TObject);
begin
  AAABirdForm.Show;{8000 in its own help file}
end;
procedure TLifeMainForm.Bird1Click(Sender: TObject);
begin  //old ~2D birds 8000
  BirdDXForm.Show;
end;


procedure TLifeMainForm.FishClick(Sender: TObject);
begin
   //Fish 3d   16000   Tropical Fish tank
end;
procedure TLifeMainForm.Fish1Click(Sender: TObject);
begin
  //2d   Fish  herring / Dolphins / Whales  15000
end;

procedure TLifeMainForm.TorsoBtnClick(Sender: TObject);
begin
  DXTorsoForm.Show; //3d PP Mazed Mobiles  10000
end;
procedure TLifeMainForm.Bod1Click(Sender: TObject);
begin
  DXTorsoForm.Show; //3d PP Torso... 9000
end;

procedure TLifeMainForm.ToolsBtnClick(Sender: TObject);
begin
   GLS3dUvForm.Show; // Tools 3D Editor / Texture Painter Help 14000
end;
procedure TLifeMainForm.Tools1Click(Sender: TObject);
begin
  ATreeForm.Show;  //9900 Trees for Birds
end;

procedure TLifeMainForm.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(0);
end;

procedure TLifeMainForm.AboutBtnClick(Sender: TObject);
begin
  AboutBoids.Show;
end;


end.
