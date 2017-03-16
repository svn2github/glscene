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
unit GlsAbout;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ShellAPI,//ShellExecute Internet launch
  StdCtrls, Buttons, pngimage, ExtCtrls;

type
  TAboutBoids = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    glsImage: TImage;
    oglImage: TImage;
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
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GetBuildInfo(var v1, v2, v3, v4: Word);
    function GetBuildInfoString: string;    
    procedure oglImageClick(Sender: TObject);
    procedure glsImageClick(Sender: TObject);
    procedure DelphiButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CommentsClick(Sender: TObject);
    procedure SystemInformationBtnClick(Sender: TObject);
    procedure ProgramInformationBtnClick(Sender: TObject);

  private
     
  public
     
  end;

var
  AboutBoids: TAboutBoids;

implementation
uses nSysInfo, nUGlobal,GlsBirdFrm;

{$R *.DFM}

procedure TAboutBoids.FormCreate(Sender: TObject);
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
{WORD HIWORD(DWORD dwValue ); // value from which high-order word is retrieved
LOWORD...}
  end;
  //BuildInfo.Caption := GetBuildInfoString
end;

function TAboutBoids.GetBuildInfoString: string;
var
  v1, v2, v3, v4: Word;
begin
  GetBuildInfo(v1, v2, v3, v4);
  Result := Format('%d.%d  (Build %d.%d)', [v1, v2, v3, v4]);
end;

procedure TAboutBoids.GetBuildInfo(var v1, v2, v3, v4: Word);
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

procedure TAboutBoids.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AboutFormX := AboutBoids.left;
  AboutFormY := AboutBoids.top;
  DoSaver;
  ModalResult := mrOK;
end;

procedure TAboutBoids.oglImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.opengl.org/', '', '', SW_SHOW);
end;

procedure TAboutBoids.glsImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.glscene.org/', '', '', SW_SHOW);
end;

procedure TAboutBoids.DelphiButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.borland.com/delphi/', '', '', SW_SHOW);
end;

procedure TAboutBoids.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(222);
end;



procedure TAboutBoids.CommentsClick(Sender: TObject);
begin
//
end;

procedure TAboutBoids.SystemInformationBtnClick(Sender: TObject);
begin
  ExecuteFile('MSINFO32.EXE', '', '', SW_SHOW);
end;

procedure TAboutBoids.ProgramInformationBtnClick(Sender: TObject);
begin
  SystemInfoForm.Show;
end;

end.
