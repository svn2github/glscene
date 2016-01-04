//
// This unit is part of the DGLEngine Project, http://glscene.org
//
{ : DGLPersistentClasses

  @HTML (
  <p>Base persistence classes.</p>
  <p>
  These classes are used in GLScene, but are designed for generic purpose.<br>
  They implement a slightly different persistence mechanism than that of the VCL,
  allowing for object-level versioning (100% backward compatibility) and full
  polymorphic persistence.</p>
  <p>
  <b>History : </b><font size=-1><ul>
  <li>22/12/15 - JD - Imported from GLScene
  </ul></font></p> )
}
unit DGLPersistentClasses;

interface

{$I DGLEngine.inc}

uses
  Classes,
  SysUtils,
  DGLCrossPlatform, DGLResStrings;

type

  PObject = ^TObject;

  // TVirtualReader
  //
  { : Virtual layer similar to VCL's TReader (but reusable) }
  TVirtualReader = class
  private
    { Private Declarations }
    FStream: TStream;

  public
    { Public Declarations }
    constructor Create(Stream: TStream); virtual;

    property Stream: TStream read FStream;

    procedure ReadTypeError;

    procedure Read(var Buf; Count: Longint); virtual; abstract;
    function NextValue: TValueType; virtual; abstract;

    function ReadInteger: Integer; virtual; abstract;
    function ReadBoolean: Boolean; virtual; abstract;
    function ReadString: string; virtual; abstract;
    function ReadFloat: Extended; virtual; abstract;

    procedure ReadListBegin; virtual; abstract;
    procedure ReadListEnd; virtual; abstract;
    function EndOfList: Boolean; virtual; abstract;

    procedure ReadTStrings(aStrings: TStrings);
  end;

  // TVirtualWriter
  //
  { : Virtual layer similar to VCL's TWriter (but reusable) }
  TVirtualWriter = class
  private
    { Private Declarations }
    FStream: TStream;

  public
    { Public Declarations }
    constructor Create(Stream: TStream); virtual;

    property Stream: TStream read FStream;

    procedure Write(const Buf; Count: Longint); virtual; abstract;
    procedure WriteInteger(anInteger: Integer); virtual; abstract;
    procedure WriteBoolean(aBoolean: Boolean); virtual; abstract;
    procedure WriteString(const aString: string); virtual; abstract;
    procedure WriteFloat(const aFloat: Extended); virtual; abstract;

    procedure WriteListBegin; virtual; abstract;
    procedure WriteListEnd; virtual; abstract;

    procedure WriteTStrings(const aStrings: TStrings; storeObjects: Boolean = True);
  end;

  TVirtualReaderClass = class of TVirtualReader;
  TVirtualWriterClass = class of TVirtualWriter;

  // IDGLPersistentObject
  //
  { : Interface for persistent objects.<p>
    This interface does not really allow polymorphic persistence,
    but is rather intended as a way to unify persistence calls
    for iterators. }
  IDGLPersistentObject = interface(IInterface)
    ['{37BC80D9-177B-4A56-A5A9-250459ABC891}']
    procedure WriteToFiler(writer: TVirtualWriter);
    procedure ReadFromFiler(reader: TVirtualReader);
  end;

  // TDGLPersistentObject
  //
  { : Base class for persistent objects.<p>
    The base requirement is implementation of ReadFromFiler & WriteToFiler
    in sub-classes, the immediate benefits are support of streaming (to stream,
    file or string), assignment and cloning.<br>
    The other requirement being the use of a virtual constructor, which allows
    polymorphic construction (don't forget to register your subclasses).<p>
    Note that TDGLPersistentObject implements IUnknown, but does *not* implement
    reference counting. }
  TDGLPersistentObject = class(TPersistent, IDGLPersistentObject)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure RaiseFilerException(const archiveVersion: Integer);

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
    { Public Declarations }
    constructor Create; virtual;
    constructor CreateFromFiler(reader: TVirtualReader);
    destructor Destroy; override;

    procedure Assign(source: TPersistent); override;
    function CreateClone: TDGLPersistentObject; dynamic;

    class function FileSignature: string; virtual;
    class function FileVirtualWriter: TVirtualWriterClass; virtual;
    class function FileVirtualReader: TVirtualReaderClass; virtual;

    procedure WriteToFiler(writer: TVirtualWriter); dynamic;
    procedure ReadFromFiler(reader: TVirtualReader); dynamic;

    procedure SaveToStream(Stream: TStream; writerClass: TVirtualWriterClass = nil); dynamic;
    procedure LoadFromStream(Stream: TStream; readerClass: TVirtualReaderClass = nil); dynamic;
    procedure SaveToFile(const fileName: string; writerClass: TVirtualWriterClass = nil); dynamic;
    procedure LoadFromFile(const fileName: string; readerClass: TVirtualReaderClass = nil); dynamic;
    function SaveToString(writerClass: TVirtualWriterClass = nil): string; dynamic;
    procedure LoadFromString(const data: string; readerClass: TVirtualReaderClass = nil); dynamic;
  end;

  TDGLPersistentObjectClass = class of TDGLPersistentObject;

  TPointerObjectList     = array [0 .. MaxInt div (2 * SizeOf(Pointer))] of TObject;
  PPointerObjectList     = ^TPointerObjectList;
  TObjectListSortCompare = function(item1, item2: TObject): Integer;

  // TDGLPersistentObjectList
  //
  { : A persistent Object list.<p>
    Similar to TList but works on TObject items and has facilities for
    persistence of contained data. Unlike the VCL's TObjectList, this one
    does NOT free its objects upon destruction or Clear, use Clean and CleanFree
    for that, and as such can be used for object referral lists too.<br>
    But only TDGLPersistentObject items will be streamed appropriately.<p>
    The list can be used in a stack-like fashion with Push & Pop, and can
    perform basic boolean set operations.<p>
    Note: the IndexOf implementation is up to 3 times faster than that of TList }
  TDGLPersistentObjectList = class(TDGLPersistentObject)
  private
    { Private Declarations }
    FList:        PPointerObjectList;
    FCount:       Integer;
    FCapacity:    Integer;
    FGrowthDelta: Integer;

  protected
    { Protected Declarations }
    procedure Error; virtual;
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    procedure SetCapacity(newCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetFirst: TObject;
    procedure SetFirst(Item: TObject);
    function GetLast: TObject;
    procedure SetLast(Item: TObject);

    // : Default event for ReadFromFiler
    procedure AfterObjectCreatedByReader(Sender: TObject); virtual;
    procedure DoClean;

  public
    { Public Declarations }
    constructor Create; override;
    destructor Destroy; override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;
    procedure ReadFromFilerWithEvent(reader: TVirtualReader; afterSenderObjectCreated: TNotifyEvent);

    function Add(const Item: TObject): Integer;
    procedure AddNils(nbVals: Cardinal);
    procedure Delete(Index: Integer);
    procedure DeleteItems(Index: Integer; nbVals: Cardinal);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; Item: TObject);
    procedure InsertNils(Index: Integer; nbVals: Cardinal);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TObject): Integer;
    procedure DeleteAndFree(Index: Integer);
    procedure DeleteAndFreeItems(Index: Integer; nbVals: Cardinal);
    function RemoveAndFree(Item: TObject): Integer;

    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;
    function Expand: TDGLPersistentObjectList;

    property Items[Index: Integer]: TObject read Get write Put; default;
    property Count: Integer read FCount write SetCount;
    property List: PPointerObjectList read FList;

    property Capacity: Integer read FCapacity write SetCapacity;
    { : Makes sure capacity is at least aCapacity. }
    procedure RequiredCapacity(aCapacity: Integer);

    { : Removes all "nil" from the list.<p>
      Note: Capacity is unchanged, no memory us freed, the list is just
      made shorter. This functions is orders of magnitude faster than
      its TList eponymous. }
    procedure Pack;
    { : Empty the list without freeing the objects. }
    procedure Clear; dynamic;
    { : Empty the list and free the objects. }
    procedure Clean; dynamic;
    { : Empty the list, free the objects and Free self. }
    procedure CleanFree;

    function IndexOf(Item: TObject): Integer;

    property First: TObject read GetFirst write SetFirst;
    property Last: TObject read GetLast write SetLast;
    procedure Push(Item: TObject);
    function Pop: TObject;
    procedure PopAndFree;

    function AddObjects(const objectList: TDGLPersistentObjectList): Integer;
    procedure RemoveObjects(const objectList: TDGLPersistentObjectList);
    procedure Sort(compareFunc: TObjectListSortCompare);
  end;

  // TBinaryReader
  //
  { : Wraps a TReader-compatible reader. }
  TBinaryReader = class(TVirtualReader)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    function ReadValue: TValueType;
    function ReadWideString(vType: TValueType): WideString;

  public
    { Public Declarations }
    procedure Read(var Buf; Count: Longint); override;
    function NextValue: TValueType; override;

    function ReadInteger: Integer; override;
    function ReadBoolean: Boolean; override;
    function ReadString: string; override;
    function ReadFloat: Extended; override;

    procedure ReadListBegin; override;
    procedure ReadListEnd; override;
    function EndOfList: Boolean; override;
  end;

  // TBinaryWriter
  //
  { : Wraps a TWriter-compatible writer. }
  TBinaryWriter = class(TVirtualWriter)
  private
    { Private Declarations }

  protected
    { Protected Declarations }
    procedure WriteAnsiString(const aString: AnsiString); virtual;
    procedure WriteWideString(const aString: WideString); virtual;

  public
    { Public Declarations }
    procedure Write(const Buf; Count: Longint); override;
    procedure WriteInteger(anInteger: Integer); override;
    procedure WriteBoolean(aBoolean: Boolean); override;
    procedure WriteString(const aString: string); override;
    procedure WriteFloat(const aFloat: Extended); override;

    procedure WriteListBegin; override;
    procedure WriteListEnd; override;
  end;

  // TTextReader
  //
  { : Reads object persistence in Text format. }
  TTextReader = class(TVirtualReader)
  private
    { Private Declarations }
    FValueType: string;
    FData:      string;

  protected
    { Protected Declarations }
    procedure ReadLine(const requestedType: string = '');

  public
    { Public Declarations }
    procedure Read(var Buf; Count: Longint); override;
    function NextValue: TValueType; override;

    function ReadInteger: Integer; override;
    function ReadBoolean: Boolean; override;
    function ReadString: string; override;
    function ReadFloat: Extended; override;

    procedure ReadListBegin; override;
    procedure ReadListEnd; override;
    function EndOfList: Boolean; override;
  end;

  // TTextWriter
  //
  { : Writes object persistence in Text format. }
  TTextWriter = class(TVirtualWriter)
  private
    { Private Declarations }
    FIndentLevel: Integer;

  protected
    { Protected Declarations }
    procedure WriteLine(const valueType, data: string);

  public
    { Public Declarations }
    constructor Create(aStream: TStream); override;
    destructor Destroy; override;

    procedure Write(const Buf; Count: Longint); override;
    procedure WriteInteger(anInteger: Integer); override;
    procedure WriteBoolean(aBoolean: Boolean); override;
    procedure WriteString(const aString: string); override;
    procedure WriteFloat(const aFloat: Extended); override;

    procedure WriteListBegin; override;
    procedure WriteListEnd; override;
  end;

  // TGLOwnedPersistent
  //
  { : TPersistent which has knowledge of its owner. }
  TDGLOwnedPersistent = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
  end;

  // TGLInterfacedPersistent
  //
  { : TPersistent thet inplements IInterface. }
  TDGLInterfacedPersistent = class(TPersistent, IInterface)
  protected
    // Implementing IInterface.
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  // TGLInterfacedCollectionItem
  //
  { : TCollectionItem thet inplements IInterface. }
  TDGLInterfacedCollectionItem = class(TCollectionItem, IInterface)
  protected
    // Implementing IInterface.
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  end;

  // EInvalidFileSignature
  //
  { : Triggered when file signature does not match. }
  EInvalidFileSignature = class(Exception)
  end;

  // EFilerException
  //
  { : Usually triggered when a filing error is detected. }
  EFilerException = class(Exception)
  end;

procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
function UTF8ToWideString(const s: AnsiString): WideString;

implementation

uses
  DGLApplicationFileIO;

const
  cDefaultListGrowthDelta = 16;

const
  cVTInteger   = 'Int';
  cVTFloat     = 'Float';
  cVTString    = 'Str';
  cVTBoolean   = 'Bool';
  cVTRaw       = 'Raw';
  cVTListBegin = '{';
  cVTListEnd   = '}';

  cTrue  = 'True';
  cFalse = 'False';

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
begin
  raise EFilerException.Create(aClass.ClassName + cUnknownArchiveVersion + IntToStr(archiveVersion));
end;

// UTF8ToWideString
//

function UTF8ToWideString(const s: AnsiString): WideString;
// Based on Mike Lischke's function (Unicode.pas unit, http://www.delphi-gems.com)
const
  bytesFromUTF8: packed array [0 .. 255] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5);
  offsetsFromUTF8: array [0 .. 5] of Cardinal = ($00000000, $00003080, $000E2080, $03C82080, $FA082080, $82082080);
  MaximumUCS2: Cardinal                       = $0000FFFF;
  MaximumUCS4: Cardinal                       = $7FFFFFFF;
  ReplacementCharacter: Cardinal              = $0000FFFD;
  halfShift: Integer                          = 10;
  halfBase: Cardinal                          = $0010000;
  halfMask: Cardinal                          = $3FF;
  SurrogateHighStart: Cardinal                = $D800;
  SurrogateLowStart: Cardinal                 = $DC00;
var
  sLength, L, J, T:  Cardinal;
  ch:                Cardinal;
  extraBytesToWrite: Word;
begin
  sLength := Length(s);
  if sLength = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, sLength); // create enough room

  L := 1;
  T := 1;
  while L <= Cardinal(sLength) do
  begin
    ch                := 0;
    extraBytesToWrite := bytesFromUTF8[Ord(s[L])];
    for J             := extraBytesToWrite downto 1 do
    begin
      ch := ch + Ord(s[L]);
      Inc(L);
      ch := ch shl 6;
    end;
    ch := ch + Ord(s[L]);
    Inc(L);
    ch := ch - offsetsFromUTF8[extraBytesToWrite];

    if ch <= MaximumUCS2 then
    begin
      Result[T] := WideChar(ch);
      Inc(T);
    end
    else if ch > MaximumUCS4 then
    begin
      Result[T] := WideChar(ReplacementCharacter);
      Inc(T);
    end
    else
    begin
      ch        := ch - halfBase;
      Result[T] := WideChar((ch shr halfShift) + SurrogateHighStart);
      Inc(T);
      Result[T] := WideChar((ch and halfMask) + SurrogateLowStart);
      Inc(T);
    end;
  end;
  SetLength(Result, T - 1); // now fix up length
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TVirtualReader }
{$IFDEF GLS_REGION}{$REGION 'TVirtualReader'}{$ENDIF}

constructor TVirtualReader.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TVirtualReader.ReadTypeError;
begin
  raise EReadError.CreateFmt('%s, read type error', [ClassName]);
end;

procedure TVirtualReader.ReadTStrings(aStrings: TStrings);
var
  i:             Integer;
  objectsStored: Boolean;
begin
  aStrings.BeginUpdate;
  aStrings.Clear;
  objectsStored := ReadBoolean;
  i             := ReadInteger;
  if objectsStored then
    while i > 0 do
    begin
      aStrings.AddObject(ReadString, TObject(PtrUInt(ReadInteger)));
      Dec(i);
    end
  else
    while i > 0 do
    begin
      aStrings.Add(ReadString);
      Dec(i);
    end;
  aStrings.EndUpdate;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TVirtualWriter }
{$IFDEF GLS_REGION}{$REGION 'VirtualWriter'}{$ENDIF}

constructor TVirtualWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TVirtualWriter.WriteTStrings(const aStrings: TStrings; storeObjects: Boolean = True);
var
  i: Integer;
begin
  WriteBoolean(storeObjects);
  if Assigned(aStrings) then
  begin
    WriteInteger(aStrings.Count);
    if storeObjects then
      for i := 0 to aStrings.Count - 1 do
      begin
        WriteString(aStrings[i]);
        WriteInteger(Integer(aStrings.Objects[i]));
      end
    else
      for i := 0 to aStrings.Count - 1 do
        WriteString(aStrings[i]);
  end
  else
    WriteInteger(0);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLPersistentObject }
{$IFDEF GLS_REGION}{$REGION 'TDGLPersistentObject'}{$ENDIF}

constructor TDGLPersistentObject.Create;
begin
  inherited Create;
end;

constructor TDGLPersistentObject.CreateFromFiler(reader: TVirtualReader);
begin
  Create;
  ReadFromFiler(reader);
end;

destructor TDGLPersistentObject.Destroy;
begin
  inherited Destroy;
end;

procedure TDGLPersistentObject.Assign(source: TPersistent);
var
  ms: TStringStream; // faster than a TMemoryStream...
begin
  if source.ClassType = Self.ClassType then
  begin
    ms := TStringStream.Create('');
    try
      TDGLPersistentObject(source).SaveToStream(ms);
      ms.Position := 0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end
  else
    inherited;
end;

function TDGLPersistentObject.CreateClone: TDGLPersistentObject;
begin
  Result := TDGLPersistentObjectClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

class function TDGLPersistentObject.FileSignature: string;
begin
  Result := '';
end;

class function TDGLPersistentObject.FileVirtualWriter: TVirtualWriterClass;
begin
  Result := TBinaryWriter;
end;

class function TDGLPersistentObject.FileVirtualReader: TVirtualReaderClass;
begin
  Result := TBinaryReader;
end;

procedure TDGLPersistentObject.WriteToFiler(writer: TVirtualWriter);
begin
  // nothing
  Assert(Assigned(writer));
end;

procedure TDGLPersistentObject.ReadFromFiler(reader: TVirtualReader);
begin
  // nothing
  Assert(Assigned(reader));
end;

procedure TDGLPersistentObject.RaiseFilerException(const archiveVersion: Integer);
begin
  raise EFilerException.Create(ClassName + cUnknownArchiveVersion + IntToStr(archiveVersion)); // :IGNORE
end;

function TDGLPersistentObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TDGLPersistentObject._AddRef: Integer; stdcall;
begin
  // ignore
  Result := 1;
end;

function TDGLPersistentObject._Release: Integer; stdcall;
begin
  // ignore
  Result := 0;
end;

procedure TDGLPersistentObject.SaveToStream(Stream: TStream; writerClass: TVirtualWriterClass = nil);
var
  wr:      TVirtualWriter;
  fileSig: AnsiString;
begin
  if writerClass = nil then
    writerClass := TBinaryWriter;
  wr            := writerClass.Create(Stream);
  try
    if FileSignature <> '' then
    begin
      fileSig := AnsiString(FileSignature);
      wr.Write(fileSig[1], Length(fileSig));
    end;
    WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TDGLPersistentObject.LoadFromStream(Stream: TStream; readerClass: TVirtualReaderClass = nil);
var
  rd:  TVirtualReader;
  sig: AnsiString;
begin
  if readerClass = nil then
    readerClass := TBinaryReader;
  rd            := readerClass.Create(Stream);
  try
    if FileSignature <> '' then
    begin
      SetLength(sig, Length(FileSignature));
      rd.Read(sig[1], Length(FileSignature));
      if sig <> AnsiString(FileSignature) then
        raise EInvalidFileSignature.Create(cInvalidFileSignature);
    end;
    ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

procedure TDGLPersistentObject.SaveToFile(const fileName: string; writerClass: TVirtualWriterClass = nil);
var
  fs: TStream;
begin
  if writerClass = nil then
    writerClass := FileVirtualWriter;
  fs            := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs, writerClass);
  finally
    fs.Free;
  end;
end;

procedure TDGLPersistentObject.LoadFromFile(const fileName: string; readerClass: TVirtualReaderClass = nil);
var
  fs: TStream;
begin
  if readerClass = nil then
    readerClass := FileVirtualReader;
  fs            := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs, readerClass);
  finally
    fs.Free;
  end;
end;

function TDGLPersistentObject.SaveToString(writerClass: TVirtualWriterClass = nil): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    SaveToStream(ss, writerClass);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TDGLPersistentObject.LoadFromString(const data: string; readerClass: TVirtualReaderClass = nil);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(data);
  try
    LoadFromStream(ss, readerClass);
  finally
    ss.Free;
  end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLPersistentObjectList }
{$IFDEF GLS_REGION}{$REGION 'TDGLPersistentObjectList'}{$ENDIF}

constructor TDGLPersistentObjectList.Create;
begin
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

destructor TDGLPersistentObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDGLPersistentObjectList.Add(const Item: TObject): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TDGLPersistentObjectList.AddNils(nbVals: Cardinal);
begin
  if Integer(nbVals) + Count > Capacity then
    SetCapacity(Integer(nbVals) + Count);
  FillChar(FList[FCount], Integer(nbVals) * SizeOf(TObject), 0);
  FCount := FCount + Integer(nbVals);
end;

function TDGLPersistentObjectList.AddObjects(const objectList: TDGLPersistentObjectList): Integer;
begin
  if Assigned(objectList) then
  begin
    Result := FCount;
    SetCount(Result + objectList.Count);
    System.Move(objectList.FList^[0], FList^[Result], objectList.FCount * SizeOf(TObject));
  end
  else
    Result := 0;
end;

procedure TDGLPersistentObjectList.RemoveObjects(const objectList: TDGLPersistentObjectList);
var
  i: Integer;
begin
  for i := 0 to objectList.Count - 1 do
    Remove(objectList[i]);
end;

procedure TDGLPersistentObjectList.Clear;
begin
  if Assigned(Self) and Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TDGLPersistentObjectList.Delete(Index: Integer);
begin
  {$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
  {$ENDIF}
  Dec(FCount);
  if index < FCount then
    System.Move(FList[index + 1], FList[index], (FCount - index) * SizeOf(TObject));
end;

procedure TDGLPersistentObjectList.DeleteItems(Index: Integer; nbVals: Cardinal);
begin
  {$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
  {$ENDIF}
  if nbVals > 0 then
  begin
    if index + Integer(nbVals) < FCount then
    begin
      System.Move(FList[index + Integer(nbVals)], FList[index], (FCount - index - Integer(nbVals)) * SizeOf(TObject));
    end;
    Dec(FCount, nbVals);
  end;
end;

procedure TDGLPersistentObjectList.Exchange(Index1, Index2: Integer);
var
  Item:    TObject;
  locList: PPointerObjectList;
begin
  {$IFOPT R+}
  if (Cardinal(Index1) >= Cardinal(FCount)) or (Cardinal(Index2) >= Cardinal(FCount)) then
    Error;
  {$ENDIF}
  locList          := FList;
  Item             := locList^[Index1];
  locList^[Index1] := locList^[Index2];
  locList^[Index2] := Item;
end;

function TDGLPersistentObjectList.Expand: TDGLPersistentObjectList;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  Result := Self;
end;

function TDGLPersistentObjectList.GetFirst: TObject;
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
  {$ENDIF}
  Result := FList^[0];
end;

procedure TDGLPersistentObjectList.SetFirst(Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
  {$ENDIF}
  FList^[0] := Item;
end;

procedure TDGLPersistentObjectList.Error;
begin
  raise EListError.Create(cListIndexError);
end;

function TDGLPersistentObjectList.Get(Index: Integer): TObject;
begin
  {$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
  {$ENDIF}
  Result := FList^[Index];
end;

function TDGLPersistentObjectList.IndexOf(Item: TObject): Integer;
{$IFNDEF GLS_NO_ASM}
var
  c: Integer;
  p: ^TObject;
begin
  if FCount <= 0 then
    Result := -1
  else
  begin
    c := FCount;
    p := @FList^[0];
    asm
      mov eax, Item;
      mov ecx, c;
      mov edx, ecx;
      push edi;
      mov edi, p;
      repne scasd;
      je @@FoundIt
      mov edx, -1;
      jmp @@SetResult;
    @@FoundIt:
      sub edx, ecx;
      dec edx;
    @@SetResult:
      mov Result, edx;
      pop edi;
    end;
  end;
{$ELSE}

var
  i: Integer;
begin
  if FCount <= 0 then
    Result := -1
  else
  begin
    Result := -1;
    for i  := 0 to FCount - 1 do
      if FList^[i] = Item then
      begin
        Result := i;
        Exit;
      end;
  end;
  {$ENDIF}
end;

procedure TDGLPersistentObjectList.Insert(Index: Integer; Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(index) > Cardinal(FCount) then
    Error;
  {$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[index], FList[index + 1], (FCount - index) * SizeOf(TObject));
  FList^[index] := Item;
  Inc(FCount);
end;

procedure TDGLPersistentObjectList.InsertNils(Index: Integer; nbVals: Cardinal);
var
  nc: Integer;
begin
  {$IFOPT R+}
  Assert(Cardinal(Index) <= Cardinal(FCount));
  {$ENDIF}
  if nbVals > 0 then
  begin
    nc := FCount + Integer(nbVals);
    if nc > FCapacity then
      SetCapacity(nc);
    if Index < FCount then
      System.Move(FList[Index], FList[Index + Integer(nbVals)], (FCount - Index) * SizeOf(TObject));
    FillChar(FList[Index], Integer(nbVals) * SizeOf(TObject), 0);
    FCount := nc;
  end;
end;

function TDGLPersistentObjectList.GetLast: TObject;
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
  {$ENDIF}
  Result := FList^[FCount - 1];
end;

procedure TDGLPersistentObjectList.SetLast(Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
  {$ENDIF}
  FList^[FCount - 1] := Item;
end;

procedure TDGLPersistentObjectList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    {$IFOPT R+}
    if Cardinal(NewIndex) >= Cardinal(Count) then
      Error;
    if Cardinal(CurIndex) >= Cardinal(Count) then
      Error;
    {$ENDIF}
    Item := FList^[CurIndex];
    if CurIndex < NewIndex then
    begin
      // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
      System.Move(List[CurIndex + 1], List[CurIndex], (NewIndex - CurIndex) * SizeOf(TObject));
    end
    else
    begin
      // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
      System.Move(List[NewIndex], List[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TObject));
    end;
    FList^[NewIndex] := TObject(Item);
  end;
end;

procedure TDGLPersistentObjectList.Put(Index: Integer; Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
  {$ENDIF}
  FList^[Index] := Item;
end;

function TDGLPersistentObjectList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TDGLPersistentObjectList.Pack;
var
  i, J, n: Integer;
  p:       PPointerObjectList;
  pk:      PObject;
begin
  p := List;
  n := Count - 1;
  while (n >= 0) and (p^[n] = nil) do
    Dec(n);
  for i := 0 to n do
  begin
    if p^[i] = nil then
    begin
      pk    := @(p^[i]);
      for J := i + 1 to n do
      begin
        if p^[J] <> nil then
        begin
          pk^ := p^[J];
          Inc(pk);
        end;
      end;
      SetCount((PtrUInt(pk) - PtrUInt(p)) div SizeOf(TObject));
      Exit;
    end;
  end;
  SetCount(n + 1);
end;

procedure TDGLPersistentObjectList.SetCapacity(newCapacity: Integer);
begin
  if newCapacity <> FCapacity then
  begin
    if newCapacity < FCount then
      FCount := newCapacity;
    ReallocMem(FList, newCapacity * SizeOf(TObject));
    FCapacity := newCapacity;
  end;
end;

procedure TDGLPersistentObjectList.RequiredCapacity(aCapacity: Integer);
begin
  if FCapacity < aCapacity then
    SetCapacity(aCapacity);
end;

procedure TDGLPersistentObjectList.SetCount(NewCount: Integer);
begin
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(TObject), 0);
  FCount := NewCount;
end;

procedure TDGLPersistentObjectList.DeleteAndFree(Index: Integer);
var
  Obj: TObject;
begin
  Obj := Get(index);
  Delete(index);
  Obj.Free;
end;

procedure TDGLPersistentObjectList.DeleteAndFreeItems(Index: Integer; nbVals: Cardinal);
var
  i, n: Integer;
begin
  {$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
  {$ENDIF}
  n := index + Integer(nbVals);
  if n >= FCount then
    n   := FCount - 1;
  for i := index to n do
    FList^[i].Free;
  DeleteItems(index, nbVals);
end;

function TDGLPersistentObjectList.RemoveAndFree(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
  begin
    Delete(Result);
    Item.Free;
  end;
end;

procedure TDGLPersistentObjectList.DoClean;
var
  i: Integer;
begin
  // a 'for' loop could crash if freeing an item removes other items form the list
  i := FCount - 1;
  while i >= 0 do
  begin
    if i < FCount then
      FList^[i].Free;
    Dec(i);
  end;
end;

procedure TDGLPersistentObjectList.Clean;
begin
  DoClean;
  Clear;
end;

procedure TDGLPersistentObjectList.CleanFree;
begin
  if Self <> nil then
  begin
    Clean;
    Destroy;
  end;
end;

procedure TDGLPersistentObjectList.WriteToFiler(writer: TVirtualWriter);
(*
  Object List Filer Format :

  Integer (Version)
  ListBegin
  ...[Object]...[Object]...
  ListEnd

  with [Object] being either (read vertically)

  Boolean (unused)        String (ClassName)        Integer (reference)
  Integer                 Object Data               Object Data
*)
var
  i, objId: Integer;
  objTypes: TList;
  aType:    TClass;
begin
  objTypes := TList.Create;
  try
    with writer do
    begin
      WriteInteger(0); // Archive Version 0 (uh... not exactly... but...)
      WriteListBegin;
      for i := 0 to FCount - 1 do
      begin
        if FList^[i] = nil then
        begin
          // store nil as... nil
          WriteBoolean(False);
          WriteInteger(0);
        end
        else if (FList^[i] is TDGLPersistentObject) then
        begin
          // yeah, a TDGLPersistentObject
          aType := FList^[i].ClassType;
          objId := objTypes.IndexOf(aType);
          if objId < 0 then
          begin
            // class is unknown
            objTypes.Add(aType);
            WriteString(aType.ClassName);
          end
          else
          begin
            // class already registered
            WriteInteger(objId);
          end;
          TDGLPersistentObject(FList^[i]).WriteToFiler(writer);
        end
        else
        begin
          // Dunno that stuff here, store as is
          WriteBoolean(False);
          WriteInteger(Integer(FList^[i]));
        end;
      end;
      WriteListEnd;
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TDGLPersistentObjectList.ReadFromFilerWithEvent(reader: TVirtualReader; afterSenderObjectCreated: TNotifyEvent);
var
  Obj:      TDGLPersistentObject;
  m:        TDGLPersistentObjectClass;
  version:  Integer;
  objTypes: TList;
begin
  objTypes := TList.Create;
  try
    Clean;
    with reader do
    begin
      version := ReadInteger;
      if version = 0 then
      begin
        ReadListBegin;
        while not EndOfList do
          case Cardinal(NextValue) of
            Cardinal(vaFalse), Cardinal(vaTrue):
              begin
                // stored 'as was' value
                ReadBoolean; // ignored
                Add(TObject(PtrUInt(ReadInteger)));
              end;
            Cardinal(vaString), Cardinal(vaLString), Cardinal(vaWString), Cardinal(vaInt64) + 1 { vaUTF8String } :
              begin
                // Unknown class, to be registered
                m := TDGLPersistentObjectClass(FindClass(ReadString));
                objTypes.Add(m);
                Obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(Obj);
                Obj.ReadFromFiler(reader);
                Add(Obj);
              end;
            Cardinal(vaInt8), Cardinal(vaInt16), Cardinal(vaInt32):
              begin
                // known class, direct retrieve
                m   := TDGLPersistentObjectClass(objTypes[ReadInteger]);
                Obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(Obj);
                Obj.ReadFromFiler(reader);
                Add(Obj);
              end;
          else
            raise Exception.Create(cBrokenObjectListArchive);
          end;
        ReadListEnd;
      end
      else
        RaiseFilerException(version);
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TDGLPersistentObjectList.ReadFromFiler(reader: TVirtualReader);
begin
  ReadFromFilerWithEvent(reader, AfterObjectCreatedByReader);
end;

procedure TDGLPersistentObjectList.AfterObjectCreatedByReader(Sender: TObject);
begin
  // nothing
end;

procedure TDGLPersistentObjectList.Push(Item: TObject);
begin
  Add(Item);
end;

function TDGLPersistentObjectList.Pop: TObject;
begin
  if FCount > 0 then
  begin
    Result := FList^[FCount - 1];
    Dec(FCount);
  end
  else
    Result := nil;
end;

procedure TDGLPersistentObjectList.PopAndFree;
begin
  Pop.Free;
end;

procedure POListQuickSort(SortList: PPointerObjectList; L, R: Integer; compareFunc: TObjectListSortCompare);
var
  i, J: Integer;
  p, T: TObject;
begin
  repeat
    i := L;
    J := R;
    p := SortList^[(L + R) shr 1];
    repeat
      while compareFunc(SortList^[i], p) < 0 do
        Inc(i);
      while compareFunc(SortList^[J], p) > 0 do
        Dec(J);
      if i <= J then
      begin
        T            := SortList^[i];
        SortList^[i] := SortList^[J];
        SortList^[J] := T;
        Inc(i);
        Dec(J);
      end;
    until i > J;
    if L < J then
      POListQuickSort(SortList, L, J, compareFunc);
    L := i;
  until i >= R;
end;

procedure TDGLPersistentObjectList.Sort(compareFunc: TObjectListSortCompare);
begin
  if Count > 1 then
    POListQuickSort(FList, 0, Count - 1, compareFunc);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TBinaryReader }
{$IFDEF GLS_REGION}{$REGION 'TBinaryReader'}{$ENDIF}

procedure TBinaryReader.Read(var Buf; Count: Longint);
begin
  FStream.Read(Buf, Count);
end;

function TBinaryReader.ReadValue: TValueType;
var
  b: Byte;
begin
  Read(b, 1);
  Result := TValueType(b);
end;

function TBinaryReader.NextValue: TValueType;
var
  pos: Int64;
begin
  pos              := FStream.Position;
  Result           := ReadValue;
  FStream.Position := pos;
end;

function TBinaryReader.ReadInteger: Integer;
var
  tempShort:    ShortInt;
  tempSmallInt: SmallInt;
begin
  case ReadValue of
    vaInt8:
      begin
        Read(tempShort, 1);
        Result := tempShort;
      end;
    vaInt16:
      begin
        Read(tempSmallInt, 2);
        Result := tempSmallInt;
      end;
    vaInt32:
      Read(Result, 4);
  else
    Result := 0;
    ReadTypeError;
  end;
end;

function TBinaryReader.ReadBoolean: Boolean;
begin
  case ReadValue of
    vaTrue:
      Result := True;
    vaFalse:
      Result := False;
  else
    ReadTypeError;
    Result := False;
  end;
end;

function TBinaryReader.ReadString: string;
var
  n:          Cardinal;
  vType:      TValueType;
  tempString: AnsiString;
begin
  n     := 0;
  vType := ReadValue;
  case Cardinal(vType) of
    Cardinal(vaWString), Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        Result := ReadWideString(vType);
        Exit;
      end;
    Cardinal(vaString):
      Read(n, 1);
    Cardinal(vaLString):
      Read(n, 4);
  else
    ReadTypeError;
  end;
  SetLength(tempString, n);
  if n > 0 then
    Read(tempString[1], n);
  Result := string(tempString);
end;

function TBinaryReader.ReadWideString(vType: TValueType): WideString;
var
  n:       Cardinal;
  utf8buf: AnsiString;
begin
  Read(n, 4);
  case Cardinal(vType) of
    Cardinal(vaWString):
      begin
        SetLength(Result, n);
        if n > 0 then
          Read(Result[1], n * 2);
      end;
    Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        SetLength(utf8buf, n);
        if n > 0 then
        begin
          Read(utf8buf[1], n);
          Result := UTF8ToWideString(utf8buf);
        end;
      end;
  else
    ReadTypeError;
  end;
end;

function TBinaryReader.ReadFloat: Extended;
{$IFDEF WIN64}
var
  c: TExtended80Rec; // Temporary variable to store 10 bytes floating point number in a Win64 application
  {$ENDIF}
begin
  {$IFDEF WIN64}
  if ReadValue = vaExtended then
  begin
    Read(c, SizeOf(c)); // Load value into the temp variable
    Result := Extended(c); // Typecast into an Extended: in a win64 application is a Double
  end
  else
    ReadTypeError;
  {$ELSE}
  if ReadValue = vaExtended then
    Read(Result, SizeOf(Result))
  else
    ReadTypeError;
  {$ENDIF}
end;

procedure TBinaryReader.ReadListBegin;
begin
  if ReadValue <> vaList then
    ReadTypeError;
end;

procedure TBinaryReader.ReadListEnd;
begin
  if ReadValue <> vaNull then
    ReadTypeError;
end;

function TBinaryReader.EndOfList: Boolean;
begin
  Result := (NextValue = vaNull);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TBinaryWriter }
{$IFDEF GLS_REGION}{$REGION 'TBinaryWriter'}{$ENDIF}

procedure TBinaryWriter.Write(const Buf; Count: Longint);
begin
  FStream.Write(Buf, Count);
end;

procedure TBinaryWriter.WriteInteger(anInteger: Integer);
type
  TIntStruct = packed record
    typ: Byte;
    val: Integer;
  end;
var
  ins: TIntStruct;
begin
  ins.val := anInteger;
  if (anInteger >= Low(ShortInt)) and (anInteger <= High(ShortInt)) then
  begin
    ins.typ := Byte(vaInt8);
    Write(ins, 2);
  end
  else if (anInteger >= Low(SmallInt)) and (anInteger <= High(SmallInt)) then
  begin
    ins.typ := Byte(vaInt16);
    Write(ins, 3);
  end
  else
  begin
    ins.typ := Byte(vaInt32);
    Write(ins, 5);
  end;
end;

procedure TBinaryWriter.WriteBoolean(aBoolean: Boolean);
const
  cBoolToType: array [False .. True] of Byte = (Byte(vaFalse), Byte(vaTrue));
begin
  Write(cBoolToType[aBoolean], 1);
end;

procedure TBinaryWriter.WriteAnsiString(const aString: AnsiString);
type
  TStringHeader = packed record
    typ: Byte;
    Length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  if sh.Length <= 255 then
  begin
    sh.typ := Byte(vaString);
    Write(sh, 2);
    if sh.Length > 0 then
      Write(aString[1], sh.Length);
  end
  else
  begin
    sh.typ := Byte(vaLString);
    Write(sh, 5);
    Write(aString[1], sh.Length);
  end;
end;

procedure TBinaryWriter.WriteWideString(const aString: WideString);
type
  TStringHeader = packed record
    typ: Byte;
    Length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  sh.typ    := Byte(vaWString);
  Write(sh, 5);
  if sh.Length > 0 then
    Write(aString[1], sh.Length * SizeOf(WideChar));
end;

procedure TBinaryWriter.WriteString(const aString: string);
begin
  {$IFDEF UNICODE}
  // TODO: should really check if the string can be simplified to: vaString / vaLString / vaUTF8String
  WriteWideString(aString);
  {$ELSE}
  WriteAnsiString(aString);
  {$ENDIF}
end;

procedure TBinaryWriter.WriteFloat(const aFloat: Extended);
type
  TExtendedStruct = packed record
    typ: Byte;
    {$IFDEF WIN64}
    val: TExtended80Rec; // Structure to handle a 10 bytes floating point value
    {$ELSE}
    val: Extended;
    {$ENDIF}
  end;
var
  str: TExtendedStruct;
begin
  {$IFDEF WIN64}
  str.typ := Byte(vaExtended);
  str.val := TExtended80Rec(aFloat); // Typecast the float value (in a Win64 app the type is a Double) into the 10 bytes struct
  Write(str, SizeOf(str));
  {$ELSE}
  str.typ := Byte(vaExtended);
  str.val := aFloat;
  Write(str, SizeOf(str));
  {$ENDIF}
end;

procedure TBinaryWriter.WriteListBegin;
const
  Buf: Byte = Byte(vaList);
begin
  Write(Buf, 1);
end;

procedure TBinaryWriter.WriteListEnd;
const
  Buf: Byte = Byte(vaNull);
begin
  Write(Buf, 1);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TTextReader }
{$IFDEF GLS_REGION}{$REGION 'TTextReader'}{$ENDIF}

procedure TTextReader.ReadLine(const requestedType: string = '');
var
  line: string;
  c:    Byte;
  p:    Integer;
begin
  // will need speed upgrade, someday...
  line := '';
  repeat
    Stream.Read(c, 1);
    if c >= 32 then
      line := line + chr(c);
  until c = 10;
  line := Trim(line);
  p    := pos(' ', line);
  if p > 0 then
  begin
    FValueType := Copy(line, 1, p - 1);
    FData      := Trim(Copy(line, p + 1, MaxInt));
  end
  else
  begin
    FValueType := line;
    FData      := '';
  end;
  if requestedType <> '' then
    if requestedType <> FValueType then
      raise EFilerException.Create('Invalid type, expected "' + requestedType + '", found "FValueType".');
end;

procedure TTextReader.Read(var Buf; Count: Longint);

  function HexCharToInt(const c: Char): Integer;
  begin
    if c <= '9' then
      Result := Integer(c) - Integer('0')
    else if c < 'a' then
      Result := Integer(c) - Integer('A') + 10
    else
      Result := Integer(c) - Integer('a') + 10;
  end;

var
  i, J: Integer;
begin
  ReadLine(cVTRaw);
  J     := 1;
  for i := 0 to Count - 1 do
  begin
    PAnsiChar(@Buf)[i] := AnsiChar((HexCharToInt(FData[J]) shl 4) + HexCharToInt(FData[J + 1]));
    Inc(J, 2);
  end;
end;

function TTextReader.NextValue: TValueType;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  if FValueType = cVTInteger then
    Result := vaInt32
  else if FValueType = cVTFloat then
    Result := vaExtended
  else if FValueType = cVTString then
    Result := vaString
  else if FValueType = cVTBoolean then
    if FData = cTrue then
      Result := vaTrue
    else
      Result := vaFalse
  else if FValueType = cVTRaw then
    Result := vaBinary
  else if FValueType = cVTListBegin then
    Result := vaList
  else
    Result        := vaNull;
  Stream.Position := p;
end;

function TTextReader.ReadInteger: Integer;
begin
  ReadLine(cVTInteger);
  Result := StrToInt(FData);
end;

function TTextReader.ReadBoolean: Boolean;
begin
  ReadLine(cVTBoolean);
  Result := (FData = cTrue);
end;

function TTextReader.ReadString: string;
var
  i: Integer;
begin
  ReadLine(cVTString);
  Result := '';
  i      := 1;
  while i < Length(FData) do
  begin
    if FData[i] = '#' then
    begin
      Result := Result + Char(StrToInt(Copy(FData, i + 1, 3)));
      Inc(i, 3);
    end
    else
      Result := Result + FData[i];
    Inc(i);
  end;
  Assert(FData[i] = '.', 'Invalid stored string.');
end;

function TTextReader.ReadFloat: Extended;
var
  oldDc: Char;
begin
  ReadLine(cVTInteger);
  oldDc := GetDecimalSeparator;
  SetDecimalSeparator('.');
  Result := StrToFloat(FData);
  SetDecimalSeparator(oldDc);
end;

procedure TTextReader.ReadListBegin;
begin
  ReadLine(cVTListBegin);
end;

procedure TTextReader.ReadListEnd;
begin
  ReadLine(cVTListEnd);
end;

function TTextReader.EndOfList: Boolean;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  Result          := (FValueType = cVTListEnd);
  Stream.Position := p;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TTextWriter }
{$IFDEF GLS_REGION}{$REGION 'TTextWriter'}{$ENDIF}

constructor TTextWriter.Create(aStream: TStream);
begin
  inherited;
end;

destructor TTextWriter.Destroy;
begin
  inherited;
end;

procedure TTextWriter.WriteLine(const valueType, data: string);
var
  Buf: AnsiString;
begin
  Buf := StringOfChar(AnsiChar(#32), FIndentLevel);
  Buf := Buf + AnsiString(valueType + ' ' + data) + #13#10;
  Stream.Write(Buf[1], Length(Buf));
end;

procedure TTextWriter.Write(const Buf; Count: Longint);
const
  cNibbleToHex: PChar = '0123456789ABCDEF';
var
  i, J, b: Integer;
  data:    string;
begin
  SetLength(data, Count * 2);
  J     := 1;
  for i := 0 to Count - 1 do
  begin
    b           := Integer(PAnsiChar(@Buf)[i]);
    data[J]     := cNibbleToHex[b shr 4];
    data[J + 1] := cNibbleToHex[b and 15];
    Inc(J, 2);
  end;
  WriteLine(cVTRaw, data);
end;

procedure TTextWriter.WriteInteger(anInteger: Integer);
begin
  WriteLine(cVTInteger, IntToStr(anInteger));
end;

procedure TTextWriter.WriteBoolean(aBoolean: Boolean);
begin
  if aBoolean then
    WriteLine(cVTBoolean, cTrue)
  else
    WriteLine(cVTBoolean, cFalse);
end;

procedure TTextWriter.WriteString(const aString: string);
var
  i: Integer;
  s: string;
begin
  s     := '';
  for i := 1 to Length(aString) do
    if aString[i] >= #32 then
      s := s + aString[i]
    else
      s := s + Format('#%.3d', [Integer(aString[i])]);
  WriteLine(cVTString, s + '.');
end;

procedure TTextWriter.WriteFloat(const aFloat: Extended);
begin
  WriteLine(cVTInteger, FloatToStr(aFloat));
end;

procedure TTextWriter.WriteListBegin;
begin
  WriteLine(cVTListBegin, '');
  Inc(FIndentLevel, 3);
end;

procedure TTextWriter.WriteListEnd;
begin
  Dec(FIndentLevel, 3);
  WriteLine(cVTListEnd, '');
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLOwnedPersistent }
{$IFDEF GLS_REGION}{$REGION 'TDGLOwnedPersistent'}{$ENDIF}

constructor TDGLOwnedPersistent.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

function TDGLOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLInterfacedPersistent }
{$IFDEF GLS_REGION}{$REGION 'TDGLInterfacedPersistent'}{$ENDIF}

function TDGLInterfacedPersistent._AddRef: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TDGLInterfacedPersistent._Release: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TDGLInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLInterfacedCollectionItem }
{$IFDEF GLS_REGION}{$REGION 'TDGLInterfacedCollectionItem'}{$ENDIF}

function TDGLInterfacedCollectionItem._AddRef: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TDGLInterfacedCollectionItem._Release: Integer; stdcall;
begin
  Result := -1; // ignore
end;

function TDGLInterfacedCollectionItem.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

initialization

// class registrations
RegisterClass(TDGLPersistentObjectList);

end.
