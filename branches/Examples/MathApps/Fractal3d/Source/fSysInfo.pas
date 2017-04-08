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
unit fSysInfo;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TSystemInfoForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Label16: TLabel;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Edit19: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    SIHelpBtn: TBitBtn;
    SICloseBtn: TBitBtn;
    SIReLoadBtn: TBitBtn;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Edit23: TEdit;
    Label31: TLabel;
    Edit24: TEdit;
    Edit25: TEdit;
    Edit26: TEdit;
    Label32: TLabel;
    SIDDCBox: TComboBox;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Edit27: TEdit;
    Edit28: TEdit;
    SIDDReloadBtn: TBitBtn;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Edit29: TEdit;
    Edit30: TEdit;
    Edit31: TEdit;
    Label40: TLabel;
    Edit32: TEdit;
    procedure FormShow(Sender: TObject);
    procedure SIReLoadBtnClick(Sender: TObject);
    function HasCoProcesser: bool;
    procedure LoadEdits;
    function ShowDriveType(PRoot: Pchar): string;
{    procedure GetDiskSizeAvail(TheDrive : PChar;
                           var TotalBytes : double;
                           var TotalFree : double); }
    procedure SIHelpBtnClick(Sender: TObject);
    procedure SICloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SystemInfoForm: TSystemInfoForm;

implementation

uses fUGlobal,Registry, aclass5, fMain;
{$R *.DFM}

procedure TSystemInfoForm.FormCreate(Sender: TObject);
begin
  left := SystemInfoFormX;
  top := SystemInfoFormY;
end;

procedure TSystemInfoForm.SIHelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(13000);
end;

procedure TSystemInfoForm.SICloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TSystemInfoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SystemInfoFormX := SystemInfoForm.left;
  SystemInfoFormY := SystemInfoForm.top;
  DoSaver;
end;


var
  UI: TUserInfo;
  DrivePChar: array[0..3] of Char;
{WhataName:array[0..99]of Char;}
{function GetDiskFreeSpaceEx(lpDirectoryName: PAnsiChar;
  var lpFreeBytesAvailableToCaller : Integer;
  var lpTotalNumberOfBytes: Integer;
  var lpTotalNumberOfFreeBytes: Integer) : bool;
  stdcall;
  external kernel32
  name 'GetDiskFreeSpaceExA';
}

procedure TSystemInfoForm.SIReLoadBtnClick(Sender: TObject);
begin
  LoadEdits;
end;

procedure TSystemInfoForm.FormShow(Sender: TObject);
begin
  LoadEdits;
end;


function TSystemInfoForm.HasCoProcesser: bool;
{$IFDEF WIN32}
var
  TheKey: hKey;
{$ENDIF}
begin
  Result := true;
{$IFNDEF WIN32}
  if GetWinFlags and Wf_80x87 = 0 then
    Result := false;
{$ELSE}
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\FloatingPointProcessor',
    0,
    KEY_EXECUTE,
    TheKey) <> ERROR_SUCCESS then result := false;
  RegCloseKey(TheKey);
{$ENDIF}
end;



procedure TSystemInfoForm.LoadEdits;
var
{UserPChar:array[0..99]of Char;}
  MyStats: THeapStatus;
  DriveString, sString: string;
{TotalBytes, TotalFree : double;}
  DriveChar: Char;
{iInput:Integer;}
{nSize:DWord;pnSize,pBuf:pointer;Buf:PChar;}
{MyMem:MEMORYSTATUS;pMyMem:Pointer;
Buff: Array[0..144] OF Char;PCstr:PChar;}
  MemoryStatus: TMemoryStatus;
  SystemInfo: TSystemInfo;
  OsVersionInfo: TOsVersionInfo;
  dc: HDC;
begin
  try
    try
      UI := TUserInfo.create;
      Edit1.Text := UI.UserName;
      Edit2.Text := UI.CompanyName;
    except
      on ERegInfoNotFound do DoMessages(39976);
    end;
  finally
    UI.Free;
  end;

  OsVersionInfo.dwOSVersionInfoSize := sizeof(OsVersionInfo);
  GetVersionEx(OsVersionInfo);
  with OsVersionInfo do
  begin
    Edit3.Text := (IntToStr(dwMajorVersion) + '.' +
      IntToStr(dwMinorVersion));
{WORD HIWORD(DWORD dwValue ); // value from which high-order word is retrieved
LOWORD...}
  end;

  GetSystemInfo(SystemInfo);
  with SystemInfo do begin
    Edit4.Text := (IntToStr(dwProcessorType));
    Edit5.Text := (IntToStr(dwNumberOfProcessors));
  end;
  if HasCoProcesser then
    Edit26.Text := ('Has CoProcessor') else
    Edit26.Text := ('None:Emulation Mode');


  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  with MemoryStatus do
  begin
// Size of MemoryStatus record
    Edit6.Text := (IntToStr(dwLength));
// Per-Cent of Memory in use by your system
    Edit7.Text := (IntToStr(dwMemoryLoad));
// The amount of Total Physical memory allocated to your system.
    Edit8.Text := (IntToStr(dwTotalPhys));
// The amount available of physical memory in your system.
{    Edit9.Text := (IntToStr(dwAvailPhys));}
MemoryAvailable:=
Round(dwTotalPhys -
((dwMemoryLoad / 100)*dwTotalPhys));
{dwAvailPhys;}
    Edit9.Text := (IntToStr(MemoryAvailable));
// The amount of Total Bytes allocated to your page file.
    Edit10.Text := (IntToStr(dwTotalPageFile));
// The amount of available bytes in your page file.
    Edit11.Text := (IntToStr(dwAvailPageFile));
// The amount of Total bytes allocated to this program
// (generally 2 gigabytes of virtual space).
    Edit12.Text := (IntToStr(dwTotalVirtual));
// The amount of avalable bytes that is left to your program to use.
    Edit23.Text := (IntToStr(dwAvailVirtual));

  end; // with
(*Sample Output of what is contained in Memo1.Lines:
32: Size of 'MemoryStatus' record in bytes
76%: memory in use
33054720: Total Physical Memory in bytes
499712: Available Physical Memory in bytes
53608448: Total Bytes of Paging File
36372480: Available bytes in paging file
2143289344: User Bytes of Address space
2135556096: Available User bytes of address space *)


  DriveString := SIDDCBox.Text;
  DriveChar := DriveString[1];
  StrPCopy(DrivePChar, DriveString);
  Edit31.Text := ShowDriveType(DrivePChar);
{GetDiskSizeAvail(DrivePChar,TotalBytes,TotalFree);}
  Edit27.Text := IntToStr(DiskSize(ord(DriveChar) - 64));
{IntToStr(DiskSize(0) div 1024) + ' Kbytes capacity.';}
{(FloatToStr(TotalBytes));}
  Edit28.Text := IntToStr(DiskFree(ord(DriveChar) - 64));
 {(FloatToStr(TotalFree));}

  MyStats := GetHeapStatus;
  Str(MyStats.TotalAddrSpace, sString);
  Edit13.Text := sString;
  Str(MyStats.TotalUncommitted, sString);
  Edit14.Text := sString;
  Str(MyStats.TotalCommitted, sString);
  Edit15.Text := sString;
  Str(MyStats.TotalAllocated, sString);
  Edit16.Text := sString;
  Str(MyStats.TotalFree, sString);
  Edit17.Text := sString;
  Str(MyStats.FreeSmall, sString);
  Edit18.Text := sString;
  Str(MyStats.FreeBig, sString);
  Edit19.Text := sString;
  Str(MyStats.Unused, sString);
  Edit20.Text := sString;
  Str(MyStats.Overhead, sString);
  Edit21.Text := sString;
  Str(MyStats.HeapErrorCode, sString);
  Edit22.Text := sString;

  Edit24.Text := (IntToStr(GetSystemMetrics(SM_CXSCREEN)));
  Edit32.Text := (IntToStr(GetSystemMetrics(SM_CYSCREEN)));
  dc := GetDc(0);
  Edit25.Text := (IntToStr(GetDeviceCaps(dc, BITSPIXEL)));
 {  dc := } ReleaseDc(0, dc); {Give back the screen dc}
  if (GetSystemMetrics(SM_MOUSEPRESENT) > 0) then
    Edit29.Text := 'Moused'
  else Edit29.Text := 'Cat got the Mouse';
  if (GetSystemMetrics(SM_SLOWMACHINE) > 0) then
    Edit30.Text := 'Slow Machine'
  else Edit30.Text := 'OK';

{Printer 29 30}

end; {Of Getting ... Load}

function TSystemInfoForm.ShowDriveType(PRoot: Pchar): string;
var
  i: word;
begin {Make it lower case.}
{  if DriveLetter in ['A'..'Z'] then
    DriveLetter := chr(ord(DriveLetter) + $20);
   i := GetDriveType(chr(ord(DriveLetter) - ord('a')));}
  i := GetDriveType(PRoot);
  case i of
    0: result := 'Undetermined';
    1: result := 'Root does not exist';
    DRIVE_REMOVABLE: result := 'Floppy';
    DRIVE_FIXED: result := 'Hard disk';
    DRIVE_REMOTE: result := 'Network drive';
    DRIVE_CDROM: result := 'CD-ROM';
    DRIVE_RAMDISK: result := 'RAM disk';
  else result := 'Does not exist';
  end;
end;
(*
function GetDiskFreeSpaceEx(lpDirectoryName: PAnsiChar;
  var lpFreeBytesAvailableToCaller : Integer;
  var lpTotalNumberOfBytes: Integer;
  var lpTotalNumberOfFreeBytes: Integer) : bool;
  stdcall;
  external kernel32
  name 'GetDiskFreeSpaceExA';
*)
(*
procedure TSystemInfoForm.GetDiskSizeAvail(TheDrive : PAnsiChar;
                           var TotalBytes : double;
                           var TotalFree : double);
var
  AvailToCall : integer;
  TheSize : integer;
  FreeAvail : integer;
begin
{  GetDiskFreeSpaceEx(TheDrive,
                     AvailToCall,
                     TheSize,
                     FreeAvail); }

{$IFOPT Q+}
 {$DEFINE TURNOVERFLOWON}
 {$Q-}
{$ENDIF}
  if TheSize >= 0 then
    TotalBytes := TheSize else
  if TheSize = -1 then begin
    TotalBytes := $7FFFFFFF;
    TotalBytes := TotalBytes * 2;
    TotalBytes := TotalBytes + 1;
  end else
  begin
    TotalBytes := $7FFFFFFF;
    TotalBytes := TotalBytes + abs($7FFFFFFF - TheSize);
  end;

  if AvailToCall >= 0 then
    TotalFree := AvailToCall else
  if AvailToCall = -1 then begin
    TotalFree := $7FFFFFFF;
    TotalFree := TotalFree * 2;
    TotalFree := TotalFree + 1;
  end else
  begin
    TotalFree := $7FFFFFFF;
    TotalFree := TotalFree + abs($7FFFFFFF - AvailToCall);
  end;
end;
*)





end.
