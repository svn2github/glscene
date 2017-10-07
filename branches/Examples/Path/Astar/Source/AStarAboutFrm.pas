unit AStarAboutFrm;
(***************************************************************
 *                                                             *
 *        Image Locatable Holographics      Version 3.0.0.0    *
 *                                                             *
 *        Copyright (C) 1991,2006 by   Tachyon Software, Inc.  *
prehistoric use:
switch directive
A compiler directive that turns compiler features
on or off depending on the state (+ or -) of the switch.
For example, {.  F+} turns the Force Far calls directive on;
{. F-} turns it off.

FOAWURIQVBXTPHJDLYC
FOXY NERDS BAWL VG U IQ TPHJC

O Optimizations
A Aligned record fields
W Stack frames
U Pentium-safe FDIV
R Range checking
I I/O checking
Q Overflow checking
V Strict var-strings
B Complete boolean eval
X Extended syntax
T Typed @ operator
P Open parameters
H Huge strings
J Assignable typed constants
D Debug information
L Local symbols
Y .. YD Reference info/Definitions only
C Assertions

{. $IFDEF DO87}
{. $F+,O+,X+,I-}
{. $N+,E-,R-,D-,S-}
{. $B-,A+,W+,L-,V-,G+}
{. $ELSE}
{. $F+,O+,X+,I-}
{. $N-,E-,R-,D-,S-}
{. $B-,A+,W+,L-,V-,G+}
{. $ENDIF}

 ***************************************************************)



interface

uses Windows, SysUtils, Classes,
  Registry, {User name}
  Graphics, Forms, Controls, StdCtrls,
  ShellAPI, //links..browser  Consts
  Dialogs, Buttons, ExtCtrls;

type
  TAStarAboutForm = class(TForm)
    Panel1: TPanel;
    Version: TLabel;
    Copyright: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    BuildInfo: TLabel;
    Label12: TLabel;
    PhysMem: TLabel;
    Label13: TLabel;
    FreeRes: TLabel;
    Label10: TLabel;
    Label16: TLabel;
    WindowsVersion: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    SystemInformationBtn: TSpeedButton;
    Bevel1: TBevel;
    DelphiButton: TSpeedButton;
    Label14: TLabel;
    Licensee: TLabel;
    Label15: TLabel;
    RNEdit: TEdit;
    HelpBtn: TSpeedButton;
    AboutBtn: TSpeedButton;
    LicenseBtn: TSpeedButton;
    RegisterBtn: TSpeedButton;
    Panel2: TPanel;
    Image3: TImage;
    Label8: TLabel;
    ProgramInformationBtn: TSpeedButton;
    OrganizationLabel: TLabel;
    oglImage: TImage;
    glsImage: TImage;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label17: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure ProgramIconClick(Sender: TObject);
    procedure oglImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadEdits;
    procedure GetBuildInfo(var v1, v2, v3, v4: Word);
    function GetBuildInfoString: string;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label2Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure SystemInformationBtnClick(Sender: TObject);
    procedure DelphiButtonClick(Sender: TObject);
    procedure RegisterBtnClick(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure ProgramInformationBtnClick(Sender: TObject);
    procedure glsImageClick(Sender: TObject);
    procedure LicenseBtnClick(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
  private
    { Private declarations }
     PrivateParts:String;
  public
    { Public declarations }
  end;

var
  AStarAboutForm: TAStarAboutForm;

implementation
uses AStarGlobals;
//TongGlobals, TongsFrm, graSysInfo;
{$R *.DFM}

procedure TAStarAboutForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin {}        
  AStarAboutFormX := AStarAboutForm.left;
  AStarAboutFormY := AStarAboutForm.top;
  //StartedNameNumber := RNEdit.Text;
  ModalResult := mrOK;
end;

procedure TAStarAboutForm.FormCreate(Sender: TObject);
var
  MS: TMemoryStatus;
begin
  left := AStarAboutFormX;
  top := AStarAboutFormY;
  GlobalMemoryStatus(MS);
  PhysMem.Caption := FormatFloat('#,###" KB"', MS.dwTotalPhys /
    1024);
  FreeRes.Caption := Format('%d %%', [MS.dwMemoryLoad]);
  PrivateParts:= 'Not Registered';
  //RNEdit.Text := StartedNameNumber;
  LoadEdits;
end;


procedure TAStarAboutForm.HelpBtnClick(Sender: TObject);
var PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0));
  if FileExists(PathS + 'astar1.html') then
    ExecuteFile('astar1.html', '', PathS, SW_SHOW)
   //HelpBtn    Application.HelpContext(222);
end;
                    {Readme}

procedure TAStarAboutForm.ProgramIconClick(Sender: TObject);
var PathS: string;
begin
  PathS := ExtractFilePath(ParamStr(0));
  if FileExists(PathS + 'astarabout.html') then
    ExecuteFile('astarabout.html', '', PathS, SW_SHOW)
//    else DoMessages(20003);{20003, "Readme.txt Files not found"}
end;

                    {License}
procedure TAStarAboutForm.LicenseBtnClick(Sender: TObject);
//var PathS: string;
begin
  {PathS := ExtractFilePath(ParamStr(0));
  if FileExists(PathS + 'license.doc') then
    ExecuteFile('license.doc', '', PathS, SW_SHOW) else
    DoMessages(20002);}
end;


procedure TAStarAboutForm.RegisterBtnClick(Sender: TObject);
//var PathS: string;
begin
  {PathS := ExtractFilePath(ParamStr(0));
  if FileExists(PathS + 'order.txt') then
    ExecuteFile('order.txt', '', PathS, SW_SHOW) else
    DoMessages(20001); }
end;

          {Order form}

procedure TAStarAboutForm.LoadEdits;
const
  Win95RegInfo = 'SOFTWARE\Microsoft\Windows\CurrentVersion\';
var
  OsVersionInfo: TOsVersionInfo;
  Reg: TRegistry;
{dc : HDC;}
begin
{ Fetch registered user name from Win95 Registry }
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists(Win95RegInfo) then
    begin
      OpenKey(Win95RegInfo, False);
      Licensee.Caption := ReadString('RegisteredOwner');
      If Licensee.Caption='' then
      Licensee.Caption:='Licensee';
      OrganizationLabel.Caption :=
        ReadString('RegisteredOrganization');
      If OrganizationLabel.Caption='' then
      OrganizationLabel.Caption:='Organization';
    end ;//    else DoMessages(29976);
    Free;
  end; { with }
  OsVersionInfo.dwOSVersionInfoSize := sizeof(OsVersionInfo);
  GetVersionEx(OsVersionInfo);
  with OsVersionInfo do
  begin
    WindowsVersion.Caption := (IntToStr(dwMajorVersion) + '.'
      + IntToStr(dwMinorVersion));
{WORD HIWORD(DWORD dwValue ); // value from which high-order word is retrieved
LOWORD...}
  end;
  BuildInfo.Caption := GetBuildInfoString
end; {Of Getting ... Load}




procedure TAStarAboutForm.GetBuildInfo(var v1, v2, v3, v4: Word);
var
  VerInfoSize: DWord;
  VerInfo: Pointer;
  VerValueSize: DWord;
  VerValue: PVSFixedFileInfo;
  Dummy: DWord;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),
    dummy);
  GetMem(VerInfo, VerInfoSize);
  GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize,
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

function TAStarAboutForm.GetBuildInfoString: string;
var
  v1, v2, v3, v4: Word;
begin
  GetBuildInfo(v1, v2, v3, v4);
  Result := Format('%d.%d.%d  (Build %d)', [v1, v2, v3, v4]);
end;

{Check Registration... kinda currently ignored}
procedure TAStarAboutForm.Image3Click(Sender: TObject);
{var Dodad:Integer;   }
begin
 // StartedNameNumber := RNEdit.Text;
 (*If RNEdit.Text='Not Registered' then begin end else
 begin
  Dodad:= DoColorEdit(strtoint(StartedNameNumber));
  Case Dodad of
  {-1 is  Not Registered}
  0:Begin
      {Wrong Length <> 20  DoMessages(17)}
      DoMessages(17)
    End;
  1:{Wrong Number}  DoMessages(16);
  2:{ok.. Correct};
  End;
  end;*)
  Close;
end;


procedure TAStarAboutForm.Label2Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'mailto:ilh2o@ezl.com', '', '',  SW_SHOW);
end;

procedure TAStarAboutForm.Label5Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.ezl.com/~ilh2o/', '', '', SW_SHOW);
end;

procedure TAStarAboutForm.SystemInformationBtnClick(Sender: TObject);
begin
  ExecuteFile('MSINFO32.EXE', '', '', SW_SHOW);
end;

procedure TAStarAboutForm.DelphiButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.borland.com/delphi/', '', '', SW_SHOW);
end;



procedure TAStarAboutForm.ProgramInformationBtnClick(Sender: TObject);
begin  { SystemInfoForm.Show;}
   { SystemInfoForm := TSystemInfoForm.Create(Application);
    SystemInfoForm.ShowModal;
    SystemInfoForm.Free; }
end;

procedure TAStarAboutForm.oglImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.opengl.org/', '', '', SW_SHOW);
end;

procedure TAStarAboutForm.glsImageClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.glscene.org/', '', '', SW_SHOW);
end;



procedure TAStarAboutForm.Label7Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'mailto:pwlester@policyalmanac.org', '', '',  SW_SHOW);
end;

procedure TAStarAboutForm.Label9Click(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'http://www.policyalmanac.org/games/aStarTutorial.htm', '', '', SW_SHOW);
end;

procedure TAStarAboutForm.Label11Click(Sender: TObject);
begin  //
  ShellExecute(0, 'open',
    'http://www-cs-students.stanford.edu/~amitp/gameprog.html', '', '', SW_SHOW);

end;

end.
