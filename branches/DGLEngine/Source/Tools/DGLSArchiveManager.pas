//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLSArchiveManager
  @HTML (
  <p>
  Archive Files Manager, support ZLib compression
  </p>

  <p>
  <b>History: </b><font size=-1><ul>
  <li>01/01/16 - JD - Imported from GLScene
  </ul></font></p>
  )
}

unit DGLSArchiveManager;

{$I DGLEngine.inc}

interface

uses
  Classes, SysUtils,
  // GLS
  DGLPersistentClasses, DGLApplicationFileIO;

Type

  TCompressionLevel = (clNone, clFastest, clDefault, clMax, clLevel1, clLevel2, clLevel3, clLevel4, clLevel5, clLevel6, clLevel7, clLevel8, clLevel9);

  // ****************************************************************************************
  // TDGLBaseArchive
  //
  TDGLBaseArchive = class(TDGLDataFile)
  protected
    FFileName:         string;
    FContentList:      TStrings;
    FCompressionLevel: TCompressionLevel;
    Procedure SetCompressionLevel(aValue: TCompressionLevel); Virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property ContentList: TStrings read FContentList;

    property CompressionLevel: TCompressionLevel read FCompressionLevel write SetCompressionLevel default clNone;

    procedure Clear; virtual; abstract;

    function ContentExists(ContentName: string): boolean; virtual; abstract;

    function GetContent(Stream: TStream; index: integer): TStream; overload; virtual; abstract;
    function GetContent(ContentName: string): TStream; overload; virtual; abstract;
    function GetContent(index: integer): TStream; overload; virtual; abstract;

    function GetContentSize(index: integer): integer; overload; virtual; abstract;
    function GetContentSize(ContentName: string): integer; overload; virtual; abstract;

    procedure AddFromStream(ContentName, Path: string; FS: TStream); virtual; abstract;
    procedure AddFromFile(FileName, Path: string); virtual; abstract;

    procedure RemoveContent(index: integer); overload; virtual; abstract;
    procedure RemoveContent(ContentName: string); overload; virtual; abstract;

    procedure Extract(index: integer; NewName: string); overload; virtual; abstract;
    procedure Extract(ContentName, NewName: string); overload; virtual; abstract;
  end;

  TDGLBaseArchiveClass = class of TDGLBaseArchive;

  // ****************************************************************************************
  // Классы регистрации архивов, для того что бы по расшырениям архива можно было
  // использовать соответсвующий архиватор. Например: GLFilePak,GLFileZLib

  // TArchiveFileFormat
  //
  TArchiveFileFormat = class
  public
    BaseArchiveClass: TDGLBaseArchiveClass;
    Extension:        string;
    Description:      string;
    DescResID:        integer;
  end;

  // ****************************************************************************************
  // TArchiveFileFormatsList
  //
  TArchiveFileFormatsList = class(TDGLPersistentObjectList)
  public
    { Public Declarations }
    destructor Destroy; override;

    procedure Add(const Ext, Desc: string; DescID: integer; AClass: TDGLBaseArchiveClass);
    function FindExt(Ext: string): TDGLBaseArchiveClass;
    function FindFromFileName(const FileName: string): TDGLBaseArchiveClass;
    procedure Remove(AClass: TDGLBaseArchiveClass);
  end;

  // ****************************************************************************************
  // TDGLLibArchive
  //
  TDGLLibArchive = class(TCollectionItem)
  private
    { Private Declarations }
    vArchive:  TDGLBaseArchive;
    ArcClass:  TDGLBaseArchiveClass;
    FFileName: string;
    FName:     string;
    procedure SetCompressionLevel(aValue: TCompressionLevel);
    function GetCompressionLevel: TCompressionLevel;
    function GetContentList: TStrings;
    procedure SetName(const val: string);
  protected
    { Protected Declarations }
    function GetDisplayName: string; override;
  public
    { Public Declarations }
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property CompressionLevel: TCompressionLevel read GetCompressionLevel write SetCompressionLevel default clDefault;

    procedure CreateArchive(FileName: string; OverwriteExistingFile: boolean = False);

    property ContentList: TStrings read GetContentList;

    procedure LoadFromFile(aFileName: string); overload;
    procedure LoadFromFile(aFileName, aAchiverType: string); overload;

    procedure Clear;

    function ContentExists(aContentName: string): boolean;
    property FileName: string read FFileName;

    function GetContent(aindex: integer): TStream; overload;
    function GetContent(aContentName: string): TStream; overload;

    function GetContentSize(aindex: integer): integer; overload;
    function GetContentSize(aContentName: string): integer; overload;

    procedure AddFromStream(aContentName, aPath: string; aF: TStream); overload;
    procedure AddFromStream(aContentName: string; aF: TStream); overload;

    procedure AddFromFile(aFileName, aPath: string); overload;
    procedure AddFromFile(aFileName: string); overload;

    procedure RemoveContent(aindex: integer); overload;
    procedure RemoveContent(aContentName: string); overload;

    procedure Extract(aindex: integer; aNewName: string); overload;
    procedure Extract(aContentName, aNewName: string); overload;
  published
    property Name: string read FName write SetName;
  end;

  // ****************************************************************************************
  // TDGLLibArchives
  //
  TDGLLibArchives = class(TOwnedCollection)
  protected
    { Protected Declarations }
    procedure SetItems(index: integer; const val: TDGLLibArchive);
    function GetItems(index: integer): TDGLLibArchive;
  public
    { Public Declarations }
    constructor Create(AOwner: TComponent);

    function Owner: TPersistent;

    function IndexOf(const Item: TDGLLibArchive): integer;
    function Add: TDGLLibArchive;
    function FindItemID(ID: integer): TDGLLibArchive;
    property Items[index: integer]: TDGLLibArchive read GetItems write SetItems; default;
    // Ищем архиватор по именыи открытого архива
    function GetArchiveByFileName(const AName: string): TDGLLibArchive;
    function GetFileNameOfArchive(aValue: TDGLLibArchive): string;
    // ищем нужный итем
    function MakeUniqueName(const nameRoot: string): string;
    function GeTDGLLibArchiveByName(const AName: string): TDGLLibArchive;
    function GetNameOfLibArchive(const Archive: TDGLLibArchive): string;
  end;

  // ****************************************************************************************
  // Компонента VCL для работы с архивами.

  // TDGLArchiveManager
  //
  TDGLArchiveManager = class(TComponent)
  Private
    FArchives: TDGLLibArchives;
    Procedure SetArchives(aValue: TDGLLibArchives);
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetArchiveByFileName(const AName: string): TDGLLibArchive;
    function GetFileNameOfArchive(const aArchive: TDGLLibArchive): string;
    function GetContent(aContentName: string): TStream;
    function ContentExists(aContentName: string): boolean;
    function OpenArchive(aFileName: string): TDGLLibArchive; overload;
    function OpenArchive(aFileName, aAchiverType: string): TDGLLibArchive; overload;
    procedure CloseArchive(aArchive: TDGLLibArchive);
  Published
    property Archives: TDGLLibArchives read FArchives write SetArchives;
  end;

// ****************************************************************************************
// Другое

  EInvalidArchiveFile = class(Exception);

  // Получение класса доступных архиваторов
function GetArchiveFileFormats: TArchiveFileFormatsList;

// Регистрация архиватора.
procedure RegisterArchiveFormat(const AExtension, ADescription: string; AClass: TDGLBaseArchiveClass);
procedure UnregisterArchiveFormat(AClass: TDGLBaseArchiveClass);

// Получение активного менеджера архивов
// Внимание!!! Работает только для одного Менеджера Архивов
function GetArchiveManager: TDGLArchiveManager;

// DGLApplicationFileIO
// Эти функции служат для автоматизации загрузки
// Пользователь вводит LoadFromFile а через эти функции получает результат.

function ArcCreateFileStream(const FileName: string; mode: word): TStream;
function ArcFileStreamExists(const FileName: string): boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

Uses
  DGLResStrings;

var
  vArchiveFileFormats: TArchiveFileFormatsList;
  vArchiveManager:     TDGLArchiveManager;

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

function GetArchiveFileFormats: TArchiveFileFormatsList;
begin
  if not Assigned(vArchiveFileFormats) then
    vArchiveFileFormats := TArchiveFileFormatsList.Create;
  Result                := vArchiveFileFormats;
end;

procedure RegisterArchiveFormat(const AExtension, ADescription: string; AClass: TDGLBaseArchiveClass);
begin
  RegisterClass(AClass);
  GetArchiveFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterArchiveFormat(AClass: TDGLBaseArchiveClass);
begin
  if Assigned(vArchiveFileFormats) then
    vArchiveFileFormats.Remove(AClass);
end;

function GetArchiveManager: TDGLArchiveManager;
begin
  Result := vArchiveManager;
end;

function ArcCreateFileStream(const FileName: string; mode: word): TStream;
begin
  If GetArchiveManager <> nil then
    with GetArchiveManager do
      if ContentExists(FileName) then
      begin
        Result := GetContent(FileName);
        Exit;
      end;
  if SysUtils.FileExists(FileName) then
  begin
    Result := TFileStream.Create(FileName, mode);
    Exit;
    // ????
    // Не пойму зачем создавать файловый поток когда файл не найден
    { end
      else begin
      Result := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
      Exit; }
  end;

  Result := nil;
end;

function ArcFileStreamExists(const FileName: string): boolean;
begin
  If GetArchiveManager <> nil then
    with GetArchiveManager do
      if ContentExists(FileName) then
      begin
        Result := True;
        Exit;
      end;
  Result := SysUtils.FileExists(FileName);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibArchive }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibArchive'}{$ENDIF}

constructor TDGLLibArchive.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := TDGLLibArchives(ACollection).MakeUniqueName('LibArchive');
end;

destructor TDGLLibArchive.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDGLLibArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if vArchive = nil then
    Exit;
  vArchive.CompressionLevel := aValue;
end;

function TDGLLibArchive.GetCompressionLevel: TCompressionLevel;
begin
  Result := clDefault;
  if vArchive = nil then
    Exit;
  Result := vArchive.CompressionLevel;
end;

procedure TDGLLibArchive.CreateArchive(FileName: string; OverwriteExistingFile: boolean = False);
var
  fFile: TFileStream;
begin
  if OverwriteExistingFile or not SysUtils.FileExists(FileName) then
  begin
    fFile := TFileStream.Create(FileName, fmCreate);
    fFile.Free;
  end;
end;

procedure TDGLLibArchive.LoadFromFile(aFileName: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(aFileName));
  Delete(Ext, 1, 1);
  LoadFromFile(aFileName, Ext);
end;

procedure TDGLLibArchive.LoadFromFile(aFileName, aAchiverType: string);
begin
  if not SysUtils.FileExists(aFileName) then
    Exit;
  ArcClass := GetArchiveFileFormats.FindExt(aAchiverType);
  If ArcClass = nil then
  begin
    raise Exception.Create(ClassName + ': Unable to find module archiver to expand ' + aAchiverType);
    Exit;
  end;
  vArchive := ArcClass.Create(nil);
  vArchive.LoadFromFile(aFileName);
  FFileName := aFileName;
end;

procedure TDGLLibArchive.Clear;
begin
  if vArchive = nil then
    Exit;
  vArchive.Clear;
  vArchive.Free;
  ArcClass  := nil;
  FFileName := '';
end;

function TDGLLibArchive.ContentExists(aContentName: string): boolean;
begin
  Result := False;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentExists(aContentName)
end;

function TDGLLibArchive.GetContent(aindex: integer): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aindex)
end;

function TDGLLibArchive.GetContent(aContentName: string): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aContentName)
end;

function TDGLLibArchive.GetContentSize(aindex: integer): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aindex)
end;

function TDGLLibArchive.GetContentSize(aContentName: string): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aContentName)
end;

procedure TDGLLibArchive.AddFromStream(aContentName, aPath: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, aPath, aF)
end;

procedure TDGLLibArchive.AddFromStream(aContentName: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, '', aF)
end;

procedure TDGLLibArchive.AddFromFile(aFileName, aPath: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, aPath)
end;

procedure TDGLLibArchive.AddFromFile(aFileName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, '')
end;

procedure TDGLLibArchive.RemoveContent(aindex: integer);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aindex)
end;

procedure TDGLLibArchive.RemoveContent(aContentName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aContentName)
end;

procedure TDGLLibArchive.Extract(aindex: integer; aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aindex, aNewName)
end;

procedure TDGLLibArchive.Extract(aContentName, aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aContentName, aNewName)
end;

function TDGLLibArchive.GetContentList: TStrings;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentList;
end;

procedure TDGLLibArchive.SetName(const val: string);
begin
  if val <> FName then
  begin
    if not(csLoading in TComponent(TDGLLibArchives(Collection).GetOwner).ComponentState) then
    begin
      if TDGLLibArchives(Collection).GeTDGLLibArchiveByName(val) <> Self then
        FName := TDGLLibArchives(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
  end;
end;

function TDGLLibArchive.GetDisplayName: string;
begin
  Result := Name;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLLibArchives }
{$IFDEF GLS_REGION}{$REGION 'TDGLLibArchives'}{$ENDIF}

procedure TDGLLibArchives.SetItems(index: integer; const val: TDGLLibArchive);
begin
  GetItems(Index).Assign(val);
end;

function TDGLLibArchives.GetItems(index: integer): TDGLLibArchive;
begin
  Result := TDGLLibArchive(inherited GetItem(Index));
end;

constructor TDGLLibArchives.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TDGLLibArchive);
end;

function TDGLLibArchives.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TDGLLibArchives.IndexOf(const Item: TDGLLibArchive): integer;
var
  I: integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TDGLLibArchives.Add: TDGLLibArchive;
begin
  Result := (inherited Add) as TDGLLibArchive;
end;

function TDGLLibArchives.FindItemID(ID: integer): TDGLLibArchive;
begin
  Result := (inherited FindItemID(ID)) as TDGLLibArchive;
end;

function TDGLLibArchives.GetArchiveByFileName(const AName: string): TDGLLibArchive;
var
  I:   integer;
  Arc: TDGLLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TDGLLibArchive(inherited Items[I]);
    if Arc.FileName = AName then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TDGLLibArchives.GetFileNameOfArchive(aValue: TDGLLibArchive): string;
var
  ArcIndex: integer;
begin
  ArcIndex := IndexOf(aValue);
  if ArcIndex <> -1 then
    Result := GetItems(ArcIndex).FileName
  else
    Result := '';
end;

function TDGLLibArchives.MakeUniqueName(const nameRoot: string): string;
var
  I: integer;
begin
  Result := nameRoot;
  I      := 1;
  while GeTDGLLibArchiveByName(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(I);
    Inc(I);
  end;
end;

function TDGLLibArchives.GetDGLLibArchiveByName(const AName: string): TDGLLibArchive;
var
  I:   integer;
  Arc: TDGLLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TDGLLibArchive(inherited Items[I]);
    if (Arc.Name = AName) then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TDGLLibArchives.GetNameOfLibArchive(const Archive: TDGLLibArchive): string;
var
  MatIndex: integer;
begin
  MatIndex := IndexOf(Archive);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TArchiveFileFormatsList  }
{$IFDEF GLS_REGION}{$REGION 'TArchiveFileFormatsList '}{$ENDIF}

destructor TArchiveFileFormatsList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TArchiveFileFormatsList.Add(const Ext, Desc: string; DescID: integer; AClass: TDGLBaseArchiveClass);
var
  newRec: TArchiveFileFormat;
begin
  newRec := TArchiveFileFormat.Create;
  with newRec do
  begin
    Extension        := AnsiLowerCase(Ext);
    BaseArchiveClass := AClass;
    Description      := Desc;
    DescResID        := DescID;
  end;
  inherited Add(newRec);
end;

function TArchiveFileFormatsList.FindExt(Ext: string): TDGLBaseArchiveClass;
var
  I: integer;
begin
  Ext   := AnsiLowerCase(Ext);
  for I := Count - 1 downto 0 do
    with TArchiveFileFormat(Items[I]) do
    begin
      if Extension = Ext then
      begin
        Result := BaseArchiveClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TArchiveFileFormatsList.FindFromFileName(const FileName: string): TDGLBaseArchiveClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidArchiveFile.CreateFmt(glsUnknownExtension, [Ext, 'GLFile' + UpperCase(Ext)]);
end;

procedure TArchiveFileFormatsList.Remove(AClass: TDGLBaseArchiveClass);
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if TArchiveFileFormat(Items[I]).BaseArchiveClass.InheritsFrom(AClass) then
      DeleteAndFree(I);
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLBaseArchive }
{$IFDEF GLS_REGION}{$REGION 'TDGLBaseArchive'}{$ENDIF}

procedure TDGLBaseArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if FCompressionLevel <> aValue then
    FCompressionLevel := aValue;
end;

constructor TDGLBaseArchive.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FContentList      := TStringList.Create;
  FCompressionLevel := clDefault;
end;

destructor TDGLBaseArchive.Destroy;
begin
  FContentList.Free;
  inherited Destroy;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLArchiveManager }
{$IFDEF GLS_REGION}{$REGION 'TDGLArchiveManager'}{$ENDIF}

constructor TDGLArchiveManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArchives             := TDGLLibArchives.Create(Self);
  vArchiveManager       := Self;
  vAFIOCreateFileStream := ArcCreateFileStream;
  vAFIOFileStreamExists := ArcFileStreamExists;
end;

destructor TDGLArchiveManager.Destroy;
begin
  vArchiveManager := nil;
  FArchives.Free;
  inherited Destroy;
end;

procedure TDGLArchiveManager.SetArchives(aValue: TDGLLibArchives);
begin
  FArchives.Assign(aValue);
end;

function TDGLArchiveManager.GetArchiveByFileName(const AName: string): TDGLLibArchive;
begin
  Result := FArchives.GetArchiveByFileName(AName);
end;

function TDGLArchiveManager.GetFileNameOfArchive(const aArchive: TDGLLibArchive): string;
begin
  Result := FArchives.GetFileNameOfArchive(aArchive)
end;

function TDGLArchiveManager.GetContent(aContentName: string): TStream;
var
  I: integer;
begin
  Result := nil;
  With FArchives do
    for I := 0 to Count - 1 do
      if Items[I].ContentExists(aContentName) then
      begin
        Result := Items[I].GetContent(aContentName);
        Exit;
      end;
end;

function TDGLArchiveManager.ContentExists(aContentName: string): boolean;
var
  I: integer;
begin
  Result := False;
  With FArchives do
    for I := 0 to Count - 1 do
      if Items[I].ContentExists(aContentName) then
      begin
        Result := Items[I].ContentExists(aContentName);
        Exit;
      end;
end;

function TDGLArchiveManager.OpenArchive(aFileName: string): TDGLLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName);
end;

function TDGLArchiveManager.OpenArchive(aFileName, aAchiverType: string): TDGLLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName, aAchiverType);
end;

procedure TDGLArchiveManager.CloseArchive(aArchive: TDGLLibArchive);
begin
  FArchives.Delete(FArchives.IndexOf(aArchive));
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization

RegisterClasses([TDGLArchiveManager, TDGLLibArchives]);

finalization

FreeAndNil(vArchiveFileFormats);

end.
