{-----------------------------------------------------------------------------
 Unit Name: frmAbout
 Author:    HochwimmerA
 Purpose:   About Form.
 $Id: frmAbout.pas,v 1.7 2003/08/12 00:01:01 hochwimmera Exp $
-----------------------------------------------------------------------------}
unit frmAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,ExtCtrls, ComCtrls;

type
  TInfoItem = class(TObject)
  private
    fDisplayName: string;
    fInternalName: string;
    fValue: string;
  public
    property DisplayName: string read fDisplayName write fDisplayName;
    property InternalName: string read fInternalName write fInternalName;
    property Value: string read fValue write fValue;
    function DisplayValue: string;
    function DisplayValueForList: string;
  end;

  TAboutInfo = class(TObject)
  private
    fInfoItems: TList;
    fMajor:   Word;
    fMinor:   Word;
    fRelease: Word;
    fBuild:   Word;
    fBuildDateAsStr: string;
    procedure LoadInfoItems;
    procedure GetFileDetails(const sFile: string);
    function ReadVersionInfo(sProgram: string; Major, Minor,
                             Release, Build : pWord) :Boolean;
  public
    constructor Create(const aFile: string);
    destructor Destroy; override;
    property FileInfoItems: TList read fInfoItems;
    property FileBuildDate: string read fBuildDateAsStr;
    function FileVersion: string;
  end;

  TformAbout = class(TForm)
    pnlAboutTop: TPanel;
    lblUrl: TLabel;
    lblCopyright: TLabel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblBuildDate: TLabel;
    PageControl1: TPageControl;
    tsCredits: TTabSheet;
    tsChanges: TTabSheet;
    bOk: TBitBtn;
    memChanges: TMemo;
    tsSecretPage: TTabSheet;
    memQuality: TMemo;
    tsLicence: TTabSheet;
    memLicence: TMemo;
    jvspCredits: TMemo;
    procedure FormShow(Sender: TObject);
    procedure lblUrlClick(Sender: TObject);
    procedure memQualityClick(Sender: TObject);
    procedure lblTitleClick(Sender: TObject);
    procedure jvspCreditsDblClick(Sender: TObject);
    procedure memQualityDblClick(Sender: TObject);
  private
     
  public
     
  end;

var
  formAbout: TformAbout;

implementation

uses
  shellapi,WinProcs,WinTypes;

{$R *.dfm}
// ----- TInfoItem.DisplayValue ------------------------------------------------
function TInfoItem.DisplayValue: string;

begin
  result := fDisplayName + ' = ' + fValue;
end;
// ----- TInfoItem.DisplayValueForList -----------------------------------------
function TInfoItem.DisplayValueForList: string;

begin
  result := Copy(fDisplayName + '                    ', 1, 19) + '= ' + fValue;
end;
// ----- TAboutInfo.Create -----------------------------------------------------
Constructor TAboutInfo.Create(const aFile: string);
begin
  inherited Create;
  fInfoItems := TList.Create;
  LoadInfoItems;
  GetFileDetails(aFile);
end;
// ----- TAboutInfo.Destroy ----------------------------------------------------
Destructor TAboutInfo.Destroy;
begin
  fInfoItems.Clear;
  fInfoItems.Free;
  inherited Destroy;
end;
// ----- TAboutInfo.LoadInfoItems ----------------------------------------------
procedure TAboutInfo.LoadInfoItems;
const
  InfoNum = 11;
  InfoStr : array [1..InfoNum] of String =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'LegalTradeMarks',
     'OriginalFilename',
     'ProductName',
     'ProductVersion',
     'Comments',
     'Author');
  LabelStr : array [1..InfoNum] of String =
    ('Company Name',
     'Description',
     'File Version',
     'Internal Name',
     'Copyright',
     'TradeMarks',
     'Original File Name',
     'Product Name',
     'Product Version',
     'Comments',
     'Author');
var
  i: integer;
  myInfoItem :TInfoItem;
begin
  for i:= 1 to InfoNum do
  begin
    myInfoItem := TInfoItem.Create;
    myInfoItem.InternalName := InfoStr[i];
    myInfoItem.DisplayName := LabelStr[i];
    myInfoItem.Value := '';
    fInfoItems.Add(myInfoItem);
  end;
end;
// ----- TAboutInfo.GetFileDetails ---------------------------------------------
procedure TAboutInfo.GetFileDetails(const sFile: string);
var
  DosDate: integer;
  Major, Minor, Release, Build : Word;
begin
  DosDate := FileAge(sFile);
  if DosDate > 0 then
    fBuildDateAsStr := FormatDateTime('dd-mmmm-yyyy hh:mm',
      FileDateToDateTime(DosDate))
  else
    fBuildDateAsStr := '';

  if ReadVersionInfo(sFile, @Major, @Minor, @Release, @Build) then
  begin
    fMajor   := Major;
    fMinor   := Minor;
    fRelease := Release;
    fBuild   := Build;
  end
  else
  begin
    fMajor   := 0;
    fMinor   := 0;
    fRelease := 0;
    fBuild   := 0;
  end;
end;
// ----- TAboutInfo.FileVersion ------------------------------------------------
function TAboutInfo.FileVersion: string;
begin
  result := IntToStr(fMajor) + '.' + IntToStr(fMinor) + '.' +
                IntToStr(fRelease) + '.' + IntToStr(fBuild);
end;
// ----- TAboutInfo.ReadVersionInfo --------------------------------------------
function TAboutInfo.ReadVersionInfo(sProgram: string; Major, Minor, Release,
  Build : pWord) :Boolean;

var
  i: integer;
  Info : PVSFixedFileInfo;
{$ifdef VER120}
  InfoSize : Cardinal;
{$else}
  InfoSize : UINT;
{$endif}
  nHwnd : DWORD;
  BufferSize : DWORD;
  Buffer : Pointer;
  Value: PChar;
begin
  BufferSize := GetFileVersionInfoSize(pchar(sProgram),nHWnd); {Get buffer size}
  Result := True;
  if BufferSize <> 0 then begin {if zero, there is no version info}
    GetMem( Buffer, BufferSize); {allocate buffer memory}
    try
      if GetFileVersionInfo(PChar(sProgram),nHWnd,BufferSize,Buffer) then begin
        {got version info}
        for i:= 0 to fInfoItems.Count - 1 do
        begin
          if VerQueryValue(Buffer,PChar('StringFileInfo\140904E4\'+
              TInfoItem(fInfoItems[i]).InternalName),Pointer(Value),InfoSize) then
            if Length(value) > 0 then
              TInfoItem(fInfoItems[i]).Value := value;
        end;

        if VerQueryValue(Buffer, '\', Pointer(Info), InfoSize) then begin
          {got root block version information}
          if Assigned(Major) then begin
            Major^ := HiWord(Info^.dwFileVersionMS); {extract major version}
          end;
          if Assigned(Minor) then begin
            Minor^ := LoWord(Info^.dwFileVersionMS); {extract minor version}
          end;
          if Assigned(Release) then begin
            Release^ := HiWord(Info^.dwFileVersionLS); {extract release version}
          end;
          if Assigned(Build) then begin
            Build^ := LoWord(Info^.dwFileVersionLS); {extract build version}
          end;
        end else begin
          Result := False; {no root block version info}
        end;
      end else begin
        Result := False; {couldn't get version info}
      end;
    finally
      FreeMem(Buffer, BufferSize); {release buffer memory}
    end;
  end else begin
    Result := False; {no version info at all}
  end;
end;
// ----- TformAbout.FormShow ---------------------------------------------------
procedure TformAbout.FormShow(Sender: TObject);

var
  ai : TAboutInfo;
  sFileName :string;

begin
  sFileName := ExtractFilePath(ParamStr(0))+'ChangeLog.txt';
  if FileExists(sFileName) then
    memChanges.Lines.LoadFromFile(sFileName)
  else
    tsChanges.TabVisible := false;

  sFileName := ExtractFilePath(ParamStr(0))+'MPL-1_1.txt';
  if FileExists(sFileName) then
    memLicence.Lines.LoadFromFile(sFileName)
  else
    tsLicence.TabVisible := false;

  ai := TAboutInfo.Create(ParamStr(0));
  lblVersion.Caption := 'Pre-Release v' + ai.FileVersion;
  lblBuildDate.Caption := 'Built: ' + ai.FileBuildDate;
  ai.Free;
end;
// ----- TformAbout.lblUrlClick ------------------------------------------------
procedure TformAbout.lblUrlClick(Sender: TObject);
begin
  ShellExecute(0,'open','http://gldata.sourceforge.net','','',SW_SHOW);
end;
// ----- TformAbout.memQualityClick --------------------------------------------
procedure TformAbout.memQualityClick(Sender: TObject);
// just like the matrix reloaded - an easter egg inside an easter egg
begin
  memQuality.Color := RGB(Random(255),Random(255),Random(255));
end;
// ----- TformAbout.lblTitleClick ----------------------------------------------
procedure TformAbout.lblTitleClick(Sender: TObject);

begin
  lbltitle.Font.color := RGB(Random(255),Random(255),Random(255));
end;
// ----- TformAbout.jvspCreditsDblClick ----------------------------------------
procedure TformAbout.jvspCreditsDblClick(Sender: TObject);
begin
  tsSecretPage.TabVisible := not tsSecretPage.TabVisible;
  if tsSecretPage.TabVisible then
    jvspCredits.color := RGB(Random(255),Random(255),Random(255))
  else
    jvspCredits.Color := clwhite;
end;
// ----- TformAbout.memQualityDblClick -----------------------------------------
procedure TformAbout.memQualityDblClick(Sender: TObject);
begin
  memQuality.Color := RGB(Random(255),Random(255),Random(255));
end;
// =============================================================================
end.
