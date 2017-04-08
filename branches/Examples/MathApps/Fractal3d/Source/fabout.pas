unit fabout;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ShellAPI,//ShellExecute Internet launch
  StdCtrls, Buttons, pngimage, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    DelphiButton: TSpeedButton;
    OKButton: TButton;
    Label12: TLabel;
    PhysMem: TLabel;
    Label13: TLabel;
    FreeRes: TLabel;
    Label16: TLabel;
    WindowsVersion: TLabel;
    HelpBtn: TSpeedButton;
    BuildInfo: TLabel;
    ProgramInformationBtn: TSpeedButton;
    SystemInformationBtn: TSpeedButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GetBuildInfo(var v1, v2, v3, v4: Word);
    procedure oglImageClick(Sender: TObject);
    procedure glsImageClick(Sender: TObject);
    procedure DelphiButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SystemInformationBtnClick(Sender: TObject);
    procedure ProgramInformationBtnClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation      //fabout
uses  fUGlobal, fMain, fSysInfo;

{$R *.DFM}

procedure TAboutBox.FormCreate(Sender: TObject);
var  MS: TMemoryStatus;
  OsVersionInfo: TOsVersionInfo;
begin
  left := AboutFormX;
  top := AboutFormY;
  GlobalMemoryStatus(MS);
  PhysMem.Caption := FormatFloat('#,###" KB"', MS.dwTotalPhys /
    1024);
  FreeRes.Caption := Format('%d %%', [MS.dwMemoryLoad]);
  OsVersionInfo.dwOSVersionInfoSize := sizeof(OsVersionInfo);
  GetVersionEx(OsVersionInfo);
  with OsVersionInfo do
  begin
    WindowsVersion.Caption := (IntToStr(dwMajorVersion) + '.'
      + IntToStr(dwMinorVersion));
  end;
end;

procedure TAboutBox.GetBuildInfo(var v1, v2, v3, v4: Word);
var
  VerInfoSize: DWord;
  VerInfo: Pointer;
  VerValueSize: DWord;
  VerValue: PVSFixedFileInfo;
  Dummy: DWord;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(Application.ExeName),
    dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(Application.ExeName), 0, VerInfoSize,
    VerInfo);
  VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
  with VerValue^ do
  begin
    v1 := dwFileVersionMS shr 16;
    v2 := dwFileVersionMS and $FFFF;
    v3 := dwFileVersionLS shr 16;
    v4 := dwFileVersionLS and $FFFF;
  end;
  FreeMem(VerInfo, VerInfoSize);
end;

procedure TAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AboutFormX := AboutBox.left;
  AboutFormY := AboutBox.top;
  DoSaver;
  ModalResult := mrOK;
end;

procedure TAboutBox.oglImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.opengl.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.glsImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.glscene.org/', '', '', SW_SHOW);
end;

procedure TAboutBox.DelphiButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.borland.com/delphi/', '', '', SW_SHOW);
end;

procedure TAboutBox.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(222);
end;


procedure TAboutBox.SystemInformationBtnClick(Sender: TObject);
begin
  ExecuteFile('MSINFO32.EXE', '', '', SW_SHOW);
end;

procedure TAboutBox.ProgramInformationBtnClick(Sender: TObject);
begin
//  SystemInfoForm.Show;
end;

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.
