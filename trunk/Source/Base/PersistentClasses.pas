//
// This unit is part of the GLScene Project, http://glscene.org
//
{: PersistentClasses<p>

   Base persistence classes.<p>

   These classes are used in GLScene, but are designed for generic purpose.<br>
   They implement a slightly different persistence mechanism than that of the VCL,
   allowing for object-level versioning (100% backward compatibility) and full
   polymorphic persistence.<p>

   Internal Note: stripped down versions of XClasses & XLists.<p>

	<b>History : </b><font size=-1><ul>
      <li>12/02/03 - EG - Added IPersistentObject
      <li>09/09/01 - EG - Optimized Pack (x2.5)
      <li>14/08/01 - EG - Added AfterObjectCreatedByReader
      <li>03/08/01 - EG - Big update with addition of Virtual filers
      <li>24/07/01 - EG - D6-related changes
	   <li>15/03/01 - EG - Creation
	</ul></font><p>
}
unit PersistentClasses;

interface

uses Classes, SysUtils;

type

   // TVirtualReader
   //
   {: Virtual layer similar to VCL's TReader (but reusable) }
   TVirtualReader = class
	   private
	      { Private Declarations }
         FStream : TStream;

      public
	      { Public Declarations }
         constructor Create(Stream: TStream); virtual;

         property Stream : TStream read FStream;

         procedure Read(var Buf; Count: Longint); virtual; abstract;
         function NextValue : TValueType; virtual; abstract;

         function ReadInteger : Integer; virtual; abstract;
         function ReadBoolean : Boolean; virtual; abstract;
         function ReadString : String; virtual; abstract;
         function ReadFloat : Extended; virtual; abstract;

         procedure ReadListBegin; virtual; abstract;
         procedure ReadListEnd; virtual; abstract;
         function EndOfList : Boolean; virtual; abstract;

         procedure ReadTStrings(aStrings : TStrings);
   end;

   // TVirtualWriter
   //
   {: Virtual layer similar to VCL's TWriter (but reusable) }
   TVirtualWriter = class
	   private
	      { Private Declarations }
         FStream : TStream;

      public
	      { Public Declarations }
         constructor Create(Stream: TStream); virtual;

         property Stream : TStream read FStream;

         procedure Write(const Buf; Count: Longint); virtual; abstract;
         procedure WriteInteger(anInteger : Integer); virtual; abstract;
         procedure WriteBoolean(aBoolean : Boolean); virtual; abstract;
         procedure WriteString(const aString : String); virtual; abstract;
         procedure WriteFloat(const aFloat : Extended); virtual; abstract;

         procedure WriteListBegin; virtual; abstract;
         procedure WriteListEnd; virtual; abstract;

         procedure WriteTStrings(const aStrings : TStrings; storeObjects : Boolean = True);
   end;

   TVirtualReaderClass = class of TVirtualReader;
   TVirtualWriterClass = class of TVirtualWriter;

   // IPersistentObject
   //
   {: Interface for persistent objects.<p>
      This interface does not really allow polymorphic persistence,
      but is rather intended as a way to unify persistence calls
      for iterators. }
   IPersistentObject = interface (IUnknown)
      procedure WriteToFiler(writer : TVirtualWriter);
	   procedure ReadFromFiler(reader : TVirtualReader);
   end;

	// TPersistentObject
	//
   {: Base class for persistent objects.<p>
      The base requirement is implementation of ReadFromFiler & WriteToFiler
      in sub-classes, the immediate benefits are support of streaming (to stream,
      file or string), assignment and cloning.<br>
      The other requirement being the use of a virtual constructor, which allows
      polymorphic construction (don't forget to register your subclasses).<p>
      Note that TPersistentObject implements IUnknown, but does *not* implement
      reference counting. }
	TPersistentObject = class (TPersistent, IPersistentObject)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
   	   procedure RaiseFilerException(const archiveVersion : Integer);

         function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;
         
	   public
	      { Public Declarations }
	      constructor Create; virtual;
         constructor CreateFromFiler(reader : TVirtualReader);
         destructor Destroy; override;

         procedure Assign(source : TPersistent); override;
         function CreateClone : TPersistentObject; dynamic;

         class function FileSignature : String; virtual;
         class function FileVirtualWriter : TVirtualWriterClass; virtual;
         class function FileVirtualReader : TVirtualReaderClass; virtual;

	      procedure WriteToFiler(writer : TVirtualWriter); dynamic;
	      procedure ReadFromFiler(reader : TVirtualReader); dynamic;

         procedure SaveToStream(stream : TStream; writerClass : TVirtualWriterClass = nil); dynamic;
         procedure LoadFromStream(stream : TStream; readerClass : TVirtualReaderClass = nil); dynamic;
         procedure SaveToFile(const fileName : String; writerClass : TVirtualWriterClass = nil); dynamic;
         procedure LoadFromFile(const fileName : String; readerClass : TVirtualReaderClass = nil); dynamic;
         function SaveToString(writerClass : TVirtualWriterClass = nil) : String; dynamic;
         procedure LoadFromString(const data : String; readerClass : TVirtualReaderClass = nil); dynamic;
	end;

   TPersistentObjectClass = class of TPersistentObject;

	PPointerObjectList = ^TPointerObjectList;
	TPointerObjectList = array [0..MaxInt shr 3] of TObject;

   // TPersistentObjectList
   //
   {: A persistent Object list.<p>
      Similar to TList but works on TObject items and has facilities for
      persistence of contained data. Unlike the VCL's TObjectList, this one
      does NOT free its objects upon destruction or Clear, use Clean and CleanFree
      for that, and as such can be used for object referral lists too.<br>
      But only TPersistentObject items will be streamed appropriately.<p>
      The list can be used in a stack-like fashion with Push & Pop, and can
      perform basic boolean set operations.<p>
      Note: the IndexOf implementation is up to 3 times faster than that of TList }
	TPersistentObjectList = class (TPersistentObject)
		private
	      { Private Declarations }
			FList: PPointerObjectList;
			FCount: Integer;
			FCapacity: Integer;
			FGrowthDelta : integer;

		protected
	      { Protected Declarations }
			procedure Error; virtual;
			function  Get(Index: Integer): TObject;
			procedure Put(Index: Integer; Item: TObject);
			procedure SetCapacity(NewCapacity: Integer);
			procedure SetCount(NewCount: Integer);
			function GetFirst : TObject;
			procedure SetFirst(item : TObject);
			function GetLast : TObject;
			procedure SetLast(item : TObject);

         //: Default event for ReadFromFiler
         procedure AfterObjectCreatedByReader(Sender : TObject); virtual;

		public
	      { Public Declarations }
			constructor Create; override;
			destructor Destroy; override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;
			procedure ReadFromFilerWithEvent(reader : TVirtualReader;
													   afterSenderObjectCreated : TNotifyEvent);

			function Add(const Item: TObject) : Integer;
			procedure Delete(Index: Integer);
			procedure Exchange(Index1, Index2: Integer);
			procedure Insert(Index: Integer; Item: TObject);
			procedure Move(CurIndex, NewIndex: Integer);
			function Remove(Item: TObject): Integer;
			procedure DeleteAndFree(index : Integer);
			function RemoveAndFree(item : TObject) : Integer;

			property GrowthDelta : integer read FGrowthDelta write FGrowthDelta;
			function Expand: TPersistentObjectList;

			property Items[Index: Integer]: TObject read Get write Put; default;
			property Count: Integer read FCount write SetCount;
			property List: PPointerObjectList read FList;

			property Capacity: Integer read FCapacity write SetCapacity;
         {: Makes sure capacity is at least aCapacity. }
         procedure RequiredCapacity(aCapacity : Integer);

         {: Removes all "nil" from the list.<p>
            Note: Capacity is unchanged, no memory us freed, the list is just
            made shorter. This functions is orders of magnitude faster than
            its TList eponymous. }
         procedure Pack;
			{: Empty the list without freeing the objects. }
			procedure Clear; dynamic;
         {: Empty the list and free the objects. }
			procedure Clean; dynamic;
         {: Empty the list, free the objects and Free self. }
			procedure CleanFree;

			function IndexOf(Item: TObject): Integer;

			property First : TObject read GetFirst write SetFirst;
			property Last : TObject read GetLast write SetLast;
			procedure Push(item : TObject);
			function Pop : TObject;
			function AddObjects(const objectList : TPersistentObjectList) : Integer;
			procedure RemoveObjects(const objectList : TPersistentObjectList);
	end;

   // TBinaryReader
   //
   {: Wraps a standard TReader. }
   TBinaryReader = class (TVirtualReader)
		private
	      { Private Declarations }
         FReader : TReader;

		protected
	      { Protected Declarations }

		public
	      { Public Declarations }
         constructor Create(Stream: TStream); override;
         destructor Destroy; override;

         procedure Read(var Buf; Count: Longint); override;
         function NextValue : TValueType; override;

         function ReadInteger : Integer; override;
         function ReadBoolean : Boolean; override;
         function ReadString : String; override;
         function ReadFloat : Extended; override;

         procedure ReadListBegin; override;
         procedure ReadListEnd; override;
         function EndOfList : Boolean; override;
   end;

   // TBinaryWriter
   //
   {: Wraps a standard TWriter. }
   TBinaryWriter = class (TVirtualWriter)
		private
	      { Private Declarations }
         FWriter : TWriter;

		protected
	      { Protected Declarations }

		public
	      { Public Declarations }
         constructor Create(Stream: TStream); override;
         destructor Destroy; override;

         procedure Write(const Buf; Count: Longint); override;
         procedure WriteInteger(anInteger : Integer); override;
         procedure WriteBoolean(aBoolean : Boolean); override;
         procedure WriteString(const aString : String); override;
         procedure WriteFloat(const aFloat : Extended); override;

         procedure WriteListBegin; override;
         procedure WriteListEnd; override;
   end;

   // TTextReader
   //
   {: Reads object persistence in Text format. }
   TTextReader = class (TVirtualReader)
		private
	      { Private Declarations }
         FValueType : String;
         FData : String;

		protected
	      { Protected Declarations }
         procedure ReadLine(const requestedType : String = '');

		public
	      { Public Declarations }
         constructor Create(Stream: TStream); override;
         destructor Destroy; override;

         procedure Read(var Buf; Count: Longint); override;
         function NextValue : TValueType; override;

         function ReadInteger : Integer; override;
         function ReadBoolean : Boolean; override;
         function ReadString : String; override;
         function ReadFloat : Extended; override;

         procedure ReadListBegin; override;
         procedure ReadListEnd; override;
         function EndOfList : Boolean; override;
   end;

   // TTextWriter
   //
   {: Writes object persistence in Text format. }
   TTextWriter = class (TVirtualWriter)
		private
	      { Private Declarations }
         FIndentLevel : Integer;

		protected
	      { Protected Declarations }
         procedure WriteLine(const valueType, data : String);

		public
	      { Public Declarations }
         constructor Create(Stream: TStream); override;
         destructor Destroy; override;

         procedure Write(const Buf; Count: Longint); override;
         procedure WriteInteger(anInteger : Integer); override;
         procedure WriteBoolean(aBoolean : Boolean); override;
         procedure WriteString(const aString : String); override;
         procedure WriteFloat(const aFloat : Extended); override;

         procedure WriteListBegin; override;
         procedure WriteListEnd; override;
   end;

   // EInvalidFileSignature
   //
   {: Triggered when file signature does not match. }
   EInvalidFileSignature = class (Exception)
   end;

   // EFilerException
   //
   {: Usually triggered when a filing error is detected. }
	EFilerException = Class(Exception)
	end;

procedure RaiseFilerException(aClass : TClass; archiveVersion : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses ApplicationFileIO;

resourcestring
   cInvalidFileSignature = 'Invalid file signature';
   cUnknownArchiveVersion = ' : unknown archive version ';
   cBrokenObjectListArchive = 'Broken ObjectList archive';
   cListIndexError = 'Invalid list index';

const
   cDefaultListGrowthDelta = 16;

const
   cVTInteger     = 'Int';
   cVTFloat       = 'Float';
   cVTString      = 'Str';
   cVTBoolean     = 'Bool';
   cVTRaw         = 'Raw';
   cVTListBegin   = '{';
   cVTListEnd     = '}';

   cTrue    = 'True';
   cFalse   = 'False';

// RaiseFilerException
//
procedure RaiseFilerException(aClass : TClass; archiveVersion : Integer);
begin
   raise EFilerException.Create(aClass.ClassName+cUnknownArchiveVersion+IntToStr(archiveVersion));
end;

// ------------------
// ------------------ TVirtualReader ------------------
// ------------------

// Create
//
constructor TVirtualReader.Create(Stream: TStream);
begin
   FStream:=Stream;
end;

// ReadTStrings
//
procedure TVirtualReader.ReadTStrings(aStrings : TStrings);
var
	i : Integer;
	objectsStored : Boolean;
begin
   aStrings.BeginUpdate;
   aStrings.Clear;
   objectsStored:=ReadBoolean;
   i:=ReadInteger;
   if objectsStored then while i>0 do begin
      aStrings.AddObject(ReadString, Pointer(ReadInteger));
      Dec(i);
   end else while i>0 do begin
      aStrings.Add(ReadString);
      Dec(i);
   end;
   aStrings.EndUpdate;
end;

// ------------------
// ------------------ TVirtualWriter ------------------
// ------------------

// Create
//
constructor TVirtualWriter.Create(Stream: TStream);
begin
   FStream:=Stream;
end;

// WriteTStrings
//
procedure TVirtualWriter.WriteTStrings(const aStrings : TStrings;
               								storeObjects : Boolean = True);
var
	i : Integer;
begin
   writeBoolean(storeObjects);
   if Assigned(aStrings) then begin
      WriteInteger(aStrings.Count);
      if storeObjects then for i:=0 to aStrings.Count-1 do begin
         WriteString(aStrings[i]);
         WriteInteger(Integer(aStrings.Objects[i]));
      end else for i:=0 to aStrings.Count-1 do WriteString(aStrings[i]);
   end else WriteInteger(0);
end;

// ------------------
// ------------------ TPersistentObject ------------------
// ------------------

// Create
//
constructor TPersistentObject.Create;
begin
	inherited Create;
end;

// CreateFromFiler
//
constructor TPersistentObject.CreateFromFiler(reader : TVirtualReader);
begin
   Create;
   ReadFromFiler(reader);
end;

// Destroy
//
destructor TPersistentObject.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TPersistentObject.Assign(source : TPersistent);
var
   ms : TStringStream;  // faster than a TMemoryStream...
begin
   if source.ClassType=Self.ClassType then begin
      ms:=TStringStream.Create('');
      try
         TPersistentObject(source).SaveToStream(ms);
         ms.Position:=0;
         LoadFromStream(ms);
      finally
         ms.Free;
      end;
   end else inherited;
end;

// CreateClone
//
function TPersistentObject.CreateClone : TPersistentObject;
begin
   Result:=TPersistentObjectClass(Self.ClassType).Create;
   Result.Assign(Self);
end;

// FileSignature
//
class function TPersistentObject.FileSignature : String;
begin
   Result:='';
end;

// FileVirtualWriter
//
class function TPersistentObject.FileVirtualWriter : TVirtualWriterClass;
begin
   Result:=TBinaryWriter;
end;

// FileVirtualReader
//
class function TPersistentObject.FileVirtualReader : TVirtualReaderClass;
begin
   Result:=TBinaryReader;
end;

// WriteToFiler
//
procedure TPersistentObject.WriteToFiler(writer : TVirtualWriter);
begin
   // nothing
end;

// ReadFromFiler
//
procedure TPersistentObject.ReadFromFiler(reader : TVirtualReader);
begin
   // nothing
end;

// RaiseFilerException
//
procedure TPersistentObject.RaiseFilerException(const archiveVersion : Integer);
begin
	raise EFilerException.Create(ClassName+cUnknownArchiveVersion+IntToStr(archiveVersion)); //:IGNORE
end;

// QueryInterface
//
function TPersistentObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const
   E_NOINTERFACE = HResult($80004002);
begin
   if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

// _AddRef
//
function TPersistentObject._AddRef: Integer;
begin
   // ignore
   Result:=1;
end;

// _Release
//
function TPersistentObject._Release: Integer;
begin
   // ignore
   Result:=0;
end;

// SaveToStream
//
procedure TPersistentObject.SaveToStream(stream : TStream; writerClass : TVirtualWriterClass = nil);
var
   wr : TVirtualWriter;
begin
   if writerClass=nil then
      writerClass:=TBinaryWriter;
   wr:=writerClass.Create(stream);
   try
      if FileSignature<>'' then
         wr.Write(FileSignature[1], Length(FileSignature));
      WriteToFiler(wr);
   finally
      wr.Free;
   end;
end;

// LoadFromStream
//
procedure TPersistentObject.LoadFromStream(stream : TStream; readerClass : TVirtualReaderClass = nil);
var
   rd : TVirtualReader;
   sig : String;
begin
   if readerClass=nil then
      readerClass:=TBinaryReader;
   rd:=readerClass.Create(stream);
   try
      if FileSignature<>'' then begin
         SetLength(sig, Length(FileSignature));
         rd.Read(sig[1], Length(FileSignature));
         if sig<>FileSignature then
            raise EInvalidFileSignature.Create(cInvalidFileSignature);
      end;
      ReadFromFiler(rd);
   finally
      rd.Free;
   end;
end;

// SaveToFile
//
procedure TPersistentObject.SaveToFile(const fileName : String; writerClass : TVirtualWriterClass = nil);
var
   fs : TStream;
begin
   if writerClass=nil then
      writerClass:=FileVirtualWriter;
   fs:=CreateFileStream(fileName, fmCreate);
   try
      SaveToStream(fs, writerClass);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TPersistentObject.LoadFromFile(const fileName : String; readerClass : TVirtualReaderClass = nil);
var
   fs : TStream;
begin
   if readerClass=nil then
      readerClass:=FileVirtualReader;
   fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadFromStream(fs, readerClass);
   finally
      fs.Free;
   end;
end;

// SaveToString
//
function TPersistentObject.SaveToString(writerClass : TVirtualWriterClass = nil) : String;
var
   ss : TStringStream;
begin
   ss:=TStringStream.Create('');
   try
      SaveToStream(ss, writerClass);
      Result:=ss.DataString;
   finally
      ss.Free;
   end;
end;

// LoadFromString
//
procedure TPersistentObject.LoadFromString(const data : String; readerClass : TVirtualReaderClass = nil);
var
   ss : TStringStream;
begin
   ss:=TStringStream.Create(data);
   try
      LoadFromStream(ss, readerClass);
   finally
      ss.Free;
   end;
end;

// ------------------
// ------------------ TPersistentObjectList ------------------
// ------------------

// Create
//
constructor TPersistentObjectList.Create;
begin
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Destroy
//
destructor TPersistentObjectList.Destroy;
begin
	Clear;
	inherited Destroy;
end;

// Add
//
function TPersistentObjectList.Add(const Item: TObject): Integer;
begin
	Result := FCount;
	if Result = FCapacity then
		SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
	Inc(FCount);
end;

// AddObjects
//
function TPersistentObjectList.AddObjects(const objectList : TPersistentObjectList) : Integer;
begin
	if Assigned(objectList) then begin
   	Result:=FCount;
		SetCount(Result+objectList.Count);
		System.Move(objectList.FList^[0], FList^[Result],
						objectList.FCount*SizeOf(TObject));
	end else Result:=0;
end;

// RemoveObjects
//
procedure TPersistentObjectList.RemoveObjects(const objectList : TPersistentObjectList);
var
	i : Integer;
begin
	for i:=0 to objectList.Count-1 do
		Remove(objectList[i]);
end;

// Clear
//
procedure TPersistentObjectList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;

// Delete
//
procedure TPersistentObjectList.Delete(Index: Integer);
begin
{$IFOPT R+}
	if (Cardinal(Index)>=Cardinal(FCount)) then Error;
{$ENDIF}
	Dec(FCount);
	if Index < FCount then
		System.Move(FList^[Index+1], FList^[Index], (FCount-Index)*SizeOf(TObject));
end;

// Exchange
//
procedure TPersistentObjectList.Exchange(Index1, Index2: Integer);
var
	Item: TObject;
begin
{$IFOPT R+}
	if (Cardinal(Index1)>=Cardinal(FCount))
		or	(Cardinal(Index2)>=Cardinal(FCount)) then Error;
{$ENDIF}
	Item:=FList^[Index1];
	FList^[Index1]:=FList^[Index2];
	FList^[Index2]:=Item;
end;

// Expand
//
function TPersistentObjectList.Expand: TPersistentObjectList;
begin
	if FCount=FCapacity then
		SetCapacity(FCapacity + FGrowthDelta);
	Result:=Self;
end;

// GetFirst
//
function TPersistentObjectList.GetFirst: TObject;
begin
	Result:=Get(0);
end;

// SetFirst
//
procedure TPersistentObjectList.SetFirst(item : TObject);
begin
	Put(0, item);
end;

// Error
//
procedure TPersistentObjectList.Error;
begin
   raise EListError.Create(cListIndexError);
end;

// Get
//
function TPersistentObjectList.Get(Index: Integer): TObject;
begin
{$IFOPT R+}
	if Cardinal(Index)>=Cardinal(FCount) then Error;
{$ENDIF}
	Result := FList^[Index];
end;

// IndexOf
//
function TPersistentObjectList.IndexOf(Item: TObject): Integer;
var
	c : Integer;
	p : ^TObject;
begin
	if FCount<=0 then
		Result:=-1
	else begin
		c:=FCount;
		p:=@FList^[0];
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
end;

// Insert
//
procedure TPersistentObjectList.Insert(Index: Integer; Item: TObject);
begin
{$IFOPT R+}
	if Cardinal(Index)>Cardinal(FCount) then Error;
{$ENDIF}
	if FCount = FCapacity then
		SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount - Index) * SizeOf(TObject));
	FList^[Index] := Item;
	Inc(FCount);
end;

// GetLast
//
function TPersistentObjectList.GetLast: TObject;
begin
	Result:=Get(FCount-1)
end;

// SetLast
//
procedure TPersistentObjectList.SetLast(item : TObject);
begin
	Put(FCount-1, item);
end;

// Move
//
procedure TPersistentObjectList.Move(CurIndex, NewIndex: Integer);
var
	Item: TObject;
begin
	if CurIndex <> NewIndex then begin
{$IFOPT R+}
		if (NewIndex < 0) or (NewIndex >= FCount) then Error;
{$ENDIF}
		Item := Get(CurIndex);
		Delete(CurIndex);
		Insert(NewIndex, Item);
	end;
end;

// Put
//
procedure TPersistentObjectList.Put(Index: Integer; Item: TObject);
begin
{$IFOPT R+}
	if Cardinal(Index)>=Cardinal(FCount) then Error;
{$ENDIF}
//   if not Shared then FList^[Index].Free;
	FList^[Index] := Item;
end;

// Remove
//
function TPersistentObjectList.Remove(Item: TObject): Integer;
begin
	Result:=IndexOf(Item);
	if Result>=0 then Delete(Result);
end;

// Pack
//
procedure TPersistentObjectList.Pack;
var
	i : Integer;
   srcList, destList : PPointerObjectList;
   obj : TObject;
begin
   srcList:=FList;
   destList:=FList;
   for i:=0 to FCount-1 do begin
      obj:=srcList[0];
      if obj<>nil then begin
         destList[0]:=obj;
         destList:=Pointer(Integer(destList)+SizeOf(TObject));
      end;
      srcList:=Pointer(Integer(srcList)+SizeOf(TObject));
   end;
	SetCount((Integer(destList)-Integer(FList)) div 4);
end;

// SetCapacity
//
procedure TPersistentObjectList.SetCapacity(NewCapacity: Integer);
begin
	if NewCapacity <> FCapacity then	begin
		ReallocMem(FList, NewCapacity * SizeOf(TObject));
		FCapacity := NewCapacity;
	end;
end;

// RequiredCapacity
//
procedure TPersistentObjectList.RequiredCapacity(aCapacity : Integer);
begin
   if FCapacity<aCapacity then SetCapacity(aCapacity);
end;

// SetCount
//
procedure TPersistentObjectList.SetCount(NewCount : Integer);
begin
	if NewCount>FCapacity then
		SetCapacity(NewCount);
	if NewCount>FCount then
		FillChar(FList^[FCount], (NewCount-FCount)*SizeOf(TObject), 0);
	FCount:=NewCount;
end;

// DeleteAndFree
//
procedure TPersistentObjectList.DeleteAndFree(index : Integer);
var
	obj : TObject;
begin
	obj:=Get(index);
	Delete(index);
	obj.Free;
end;

// RemoveAndFree
//
function TPersistentObjectList.RemoveAndFree(item : TObject) : Integer;
begin
   Result:=IndexOf(item);
	if Result<>-1 then begin
      Delete(Result);
      Item.Free;
   end;
end;

// Clean
//
procedure TPersistentObjectList.Clean;
var
	i : Integer;
begin
	// a 'for' loop could crash if freeing an item removes other items form the list
	i:=FCount-1; while i>=0 do begin
		if i<FCount then FList^[i].Free;
		Dec(i);
	end;
	Clear;
end;

// CleanFree
//
procedure TPersistentObjectList.CleanFree;
begin
   if Self<>nil then begin
      Clean;
      Destroy;
	end;
end;

// WriteToFiler
//
procedure TPersistentObjectList.WriteToFiler(writer : TVirtualWriter);
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
	i, objId : integer;
	objTypes : TList;
	aType : TClass;
begin
   objTypes:=TList.Create;
   try
      with writer do begin
         WriteInteger(0); // Archive Version 0 (uh... not exactly... but...)
         WriteListBegin;
         for i:=0 to FCount-1 do begin
            if FList^[i]=nil then begin
               // store nil as... nil
               WriteBoolean(False);
               WriteInteger(0);
            end else if (FList^[i] is TPersistentObject) then begin
               // yeah, a TPersistentObject
               aType:=FList^[i].ClassType;
               objId:=objTypes.IndexOf(aType);
               if objId<0 then begin
                  // class is unknown
                  objTypes.Add(aType);
                  WriteString(aType.ClassName);
               end else begin
                  // class already registered
                  WriteInteger(objId);
               end;
               TPersistentObject(FList^[i]).WriteToFiler(writer);
            end else begin
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

// ReadFromFilerWithEvent
//
procedure TPersistentObjectList.ReadFromFilerWithEvent(reader : TVirtualReader; afterSenderObjectCreated : TNotifyEvent);
var
	obj : TPersistentObject;
	m : TPersistentObjectClass;
	version : integer;
   objTypes : TList;
begin
	objTypes:=TList.Create;
   try
      Clean;
      with reader do begin
         version:=ReadInteger;
         if version=0 then begin
            ReadListBegin;
            while not EndOfList do case NextValue of
               vaFalse, vaTrue : begin
                  // stored 'as was' value
                  ReadBoolean; // ignored
                  Add(Pointer(ReadInteger));
               end;
               vaString, vaLString : begin
                  // Unknown class, to be registered
                  m:=TPersistentObjectClass(FindClass(ReadString));
                  objTypes.Add(m);
                  obj:=m.Create;
                  if Assigned(afterSenderObjectCreated) then
                     afterSenderObjectCreated(obj);
                  obj.ReadFromFiler(reader);
                  Add(obj);
               end;
               vaInt8, vaInt16, vaInt32 : begin
                  // known class, direct retrieve
                  m:=TPersistentObjectClass(objTypes[ReadInteger]);
                  obj:=m.Create;
                  if Assigned(afterSenderObjectCreated) then
                     afterSenderObjectCreated(obj);
                  obj.ReadFromFiler(reader);
                  Add(obj);
               end;
            else
               raise Exception.Create(cBrokenObjectListArchive);
            end;
            ReadListEnd;
         end else RaiseFilerException(version);
      end;
   finally
      objTypes.Free;
   end;
end;

// ReadFromFiler
//
procedure TPersistentObjectList.ReadFromFiler(reader : TVirtualReader);
begin
	ReadFromFilerWithEvent(reader, AfterObjectCreatedByReader);
end;

// AfterObjectCreatedByReader
//
procedure TPersistentObjectList.AfterObjectCreatedByReader(Sender : TObject);
begin
   // nothing
end;

// Push
//
procedure TPersistentObjectList.Push(item : TObject);
begin
	Add(item);
end;

// Pop
//
function TPersistentObjectList.Pop : TObject;
begin
	if FCount>0 then begin
		Result:=GetLast;
		Delete(FCount-1);
	end else Result:=nil;
end;

// ------------------
// ------------------ TBinaryReader ------------------
// ------------------

// Create
//
constructor TBinaryReader.Create(Stream: TStream);
begin
   inherited;
   FReader:=TReader.Create(Stream, 16384);
end;

// Destroy
//
destructor TBinaryReader.Destroy;
begin
   FReader.Free;
end;

// Read
//
procedure TBinaryReader.Read(var Buf; Count: Longint);
begin
   FReader.Read(Buf, Count);
end;

// NextValue
//
function TBinaryReader.NextValue : TValueType;
begin
   Result:=FReader.NextValue;
end;

// ReadInteger
//
function TBinaryReader.ReadInteger : Integer;
begin
   Result:=FReader.ReadInteger;
end;

// ReadBoolean
//
function TBinaryReader.ReadBoolean : Boolean;
begin
   Result:=FReader.ReadBoolean;
end;

// ReadString
//
function TBinaryReader.ReadString : String;
begin
   Result:=FReader.ReadString;
end;

// ReadFloat
//
function TBinaryReader.ReadFloat : Extended;
begin
   Result:=FReader.ReadFloat;
end;

// ReadListBegin
//
procedure TBinaryReader.ReadListBegin;
begin
   FReader.ReadListBegin;
end;

// ReadListEnd
//
procedure TBinaryReader.ReadListEnd;
begin
   FReader.ReadListEnd;
end;

// EndOfList
//
function TBinaryReader.EndOfList : Boolean;
begin
   Result:=FReader.EndOfList;
end;

// ------------------
// ------------------ TBinaryWriter ------------------
// ------------------

// Create
//
constructor TBinaryWriter.Create(Stream: TStream);
begin
   inherited;
   FWriter:=TWriter.Create(Stream, 16384);
end;

// Destroy
//
destructor TBinaryWriter.Destroy;
begin
   FWriter.Free;
end;

// Write
//
procedure TBinaryWriter.Write(const Buf; Count: Longint);
begin
   FWriter.Write(Buf, Count);
end;

// WriteInteger
//
procedure TBinaryWriter.WriteInteger(anInteger : Integer);
begin
   FWriter.WriteInteger(anInteger);
end;

// WriteBoolean
//
procedure TBinaryWriter.WriteBoolean(aBoolean : Boolean);
begin
   FWriter.WriteBoolean(aBoolean);
end;

// WriteString
//
procedure TBinaryWriter.WriteString(const aString : String);
begin
   FWriter.WriteString(aString);
end;

// WriteFloat
//
procedure TBinaryWriter.WriteFloat(const aFloat : Extended);
begin
   FWriter.WriteFloat(aFloat);
end;

// WriteListBegin
//
procedure TBinaryWriter.WriteListBegin;
begin
   FWriter.WriteListBegin;
end;

// WriteListEnd
//
procedure TBinaryWriter.WriteListEnd;
begin
   FWriter.WriteListEnd;
end;

// ------------------
// ------------------ TTextReader ------------------
// ------------------

// Create
//
constructor TTextReader.Create(Stream: TStream);
begin
   inherited;
end;

// Destroy
//
destructor TTextReader.Destroy;
begin
   inherited;
end;

// ReadLine
//
procedure TTextReader.ReadLine(const requestedType : String = '');
var
   line : String;
   c : Char;
   p : Integer;
begin
   // will need speed upgrade, someday...
   line:='';
   repeat
      Stream.Read(c, 1);
      if c>=#32 then
         line:=line+c;
   until c=#10;
   line:=Trim(line);
   p:=Pos(' ', line);
   if p>0 then begin
      FValueType:=Copy(line, 1, p-1);
      FData:=Trim(Copy(line, p+1, MaxInt));
   end else begin
      FValueType:=line;
      FData:='';
   end;
   if requestedType<>'' then
      if requestedType<>FValueType then
         raise EFilerException.Create('Invalid type, expected "'
                                      +requestedType+'", found "FValueType".');
end;

// Read
//
procedure TTextReader.Read(var Buf; Count: Longint);

   function HexCharToInt(const c : Char) : Integer;
   begin
      if c<='9' then
         Result:=Integer(c)-Integer('0')
      else if c<'a' then
         Result:=Integer(c)-Integer('A')+10
      else Result:=Integer(c)-Integer('a')+10;
   end;

var
   i, j : Integer;
begin
   ReadLine(cVTRaw);
   j:=1;
   for i:=0 to Count-1 do begin
      PChar(@Buf)[i]:=Char((HexCharToInt(FData[j]) shl 4)
                           +HexCharToInt(FData[j+1]));
      Inc(j, 2);
   end;
end;

// NextValue
//
function TTextReader.NextValue : TValueType;
var
   p : Integer;
begin
   p:=Stream.Position;
   ReadLine;
   if FValueType=cVTInteger then
      Result:=vaInt32
   else if FValueType=cVTFloat then
      Result:=vaExtended
   else if FValueType=cVTString then
      Result:=vaString
   else if FValueType=cVTBoolean then
      if FData=cTrue then
         Result:=vaTrue
      else Result:=vaFalse
   else if FValueType=cVTRaw then
      Result:=vaBinary
   else if FValueType=cVTListBegin then
      Result:=vaList
   else Result:=vaNULL;
   Stream.Position:=p;
end;

// ReadInteger
//
function TTextReader.ReadInteger : Integer;
begin
   ReadLine(cVTInteger);
   Result:=StrToInt(FData);
end;

// ReadBoolean
//
function TTextReader.ReadBoolean : Boolean;
begin
   ReadLine(cVTBoolean);
   Result:=(FData=cTrue);
end;

// ReadString
//
function TTextReader.ReadString : String;
var
   i : Integer;
begin
   ReadLine(cVTString);
   Result:='';
   i:=1;
   while i<Length(FData) do begin
      if FData[i]='#' then begin
         Result:=Result+Char(StrToInt(Copy(FData, i+1, 3)));
         Inc(i, 3);
      end else Result:=Result+FData[i];
      Inc(i);
   end;
   Assert(FData[i]='.', 'Invalid stored string.');
end;

// ReadFloat
//
function TTextReader.ReadFloat : Extended;
var
   oldDc : Char;
begin
   ReadLine(cVTInteger);
   oldDc:=DecimalSeparator;
   DecimalSeparator:='.';
   Result:=StrToFloat(FData);
   DecimalSeparator:=oldDc;
end;

// ReadListBegin
//
procedure TTextReader.ReadListBegin;
begin
   ReadLine(cVTListBegin);
end;

// ReadListEnd
//
procedure TTextReader.ReadListEnd;
begin
   ReadLine(cVTListEnd);
end;

// EndOfList
//
function TTextReader.EndOfList : Boolean;
var
   p : Integer;
begin
   p:=Stream.Position;
   ReadLine;
   Result:=(FValueType=cVTListEnd);
   Stream.Position:=p;
end;

// ------------------
// ------------------ TTextWriter ------------------
// ------------------

// Create
//
constructor TTextWriter.Create(Stream: TStream);
begin
   inherited;
end;

// Destroy
//
destructor TTextWriter.Destroy;
begin
   inherited;
end;

// WriteLine
//
procedure TTextWriter.WriteLine(const valueType, data : String);
var
   buf : String;
begin
   buf:=StringOfChar(' ', FIndentLevel)+valueType+' '+data+#13#10;
   Stream.Write(buf[1], Length(buf));
end;

// Write
//
procedure TTextWriter.Write(const Buf; Count: Longint);
const
   cNibbleToHex : PChar = '0123456789ABCDEF';
var
   i, j, b : Integer;
   data : String;
begin
   SetLength(data, Count*2);
   j:=1;
   for i:=0 to Count-1 do begin
      b:=Integer(PChar(@buf)[i]);
      data[j]:=cNibbleToHex[b shr 4];
      data[j+1]:=cNibbleToHex[b and 15];
      Inc(j, 2);
   end;
   WriteLine(cVTRaw, data);
end;

// WriteInteger
//
procedure TTextWriter.WriteInteger(anInteger : Integer);
begin
   WriteLine(cVTInteger, IntToStr(anInteger));
end;

// WriteBoolean
//
procedure TTextWriter.WriteBoolean(aBoolean : Boolean);
begin
   if aBoolean then
      WriteLine(cVTBoolean, cTrue)
   else WriteLine(cVTBoolean, cFalse);
end;

// WriteString
//
procedure TTextWriter.WriteString(const aString : String);
var
   i : Integer;
   s : String;
begin
   s:='';
   for i:=1 to Length(aString) do
      if aString[i]>=#32 then
         s:=s+aString[i]
      else s:=s+Format('#%.3d', [Integer(aString[i])]);
   WriteLine(cVTString, s+'.');
end;

// WriteFloat
//
procedure TTextWriter.WriteFloat(const aFloat : Extended);
begin
   WriteLine(cVTInteger, FloatToStr(aFloat));
end;

// WriteListBegin
//
procedure TTextWriter.WriteListBegin;
begin
   WriteLine(cVTListBegin, '');
   Inc(FIndentLevel, 3);
end;

// WriteListEnd
//
procedure TTextWriter.WriteListEnd;
begin
   Dec(FIndentLevel, 3);
   WriteLine(cVTListEnd, '');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
	RegisterClass(TPersistentObjectList);

end.

