// PersistentClasses
{: Base persistence classes.<p>

   These classes are used in GLScene, but are designed for generic purpose.<br>
   They implement a slightly different persistence mechanism than that of the VCL,
   allowing for object-level versioning (100% backward compatibility) and full
   polymorphic persistence.<p>

   Internal Note: stripped down versions of XClasses & XLists.<p>

	<b>History : </b><font size=-1><ul>
      <li>24/07/01 - EG - D6-related changes
	   <li>15/03/01 - EG - Creation
	</ul></font><p>
}
unit PersistentClasses;

interface

uses Classes, SysUtils;

type

	// TPersistentObject
	//
   {: Base class for persistent objects.<p>
      The base requirement is implementation of ReadFromFiler & WriteToFiler
      in sub-classes, the immediate benefits are support of streaming (to stream,
      file or string), assignment and cloning.<br>
      The other requirement being the use of a virtual constructor, which allows
      polymorphic construction (don't forget to register your subclasses). }
	TPersistentObject = class (TPersistent)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
   	   procedure RaiseFilerException(const archiveVersion : Integer);

	   public
	      { Public Declarations }
	      constructor Create; virtual;
         destructor Destroy; override;

         procedure Assign(source : TPersistent); override;
         function CreateClone : TPersistentObject; dynamic;

         class function FileSignature : String; virtual;
	      procedure WriteToFiler(writer : TWriter); dynamic;
	      procedure ReadFromFiler(reader : TReader); dynamic;

         procedure SaveToStream(stream : TStream); dynamic;
         procedure LoadFromStream(stream : TStream); dynamic;
         procedure SaveToFile(const fileName : String); dynamic;
         procedure LoadFromFile(const fileName : String); dynamic;
         function SaveToString : String; dynamic;
         procedure LoadFromString(const data : String); dynamic;
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

		public
	      { Public Declarations }
			constructor Create; override;
			destructor Destroy; override;

			procedure WriteToFiler(writer : TWriter); override;
			procedure ReadFromFiler(reader : TReader); override;
			procedure ReadFromFilerWithEvent(reader : TReader;
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
         procedure RequiredCapacity(aCapacity : Integer);

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
			procedure Pack;
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Consts;

resourcestring
   cInvalidFileSignature = 'Invalid file signature';
   cUnknownArchiveVersion = ' : unknown archive version ';
   cBrokenObjectListArchive = 'Broken ObjectList archive';
   cListIndexError = 'Invalid list index';

const
   cDefaultListGrowthDelta = 16;

// ------------------
// ------------------ TPersistentObject ------------------
// ------------------

// Create
//
constructor TPersistentObject.Create;
begin
	inherited Create;
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
   ms : TMemoryStream;
begin
   if source.ClassType=Self.ClassType then begin
      ms:=TMemoryStream.Create;
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

// WriteToFiler
//
procedure TPersistentObject.WriteToFiler(writer : TWriter);
begin
   // nothing
end;

// ReadFromFiler
//
procedure TPersistentObject.ReadFromFiler(reader : TReader);
begin
   // nothing
end;

// RaiseFilerException
//
procedure TPersistentObject.RaiseFilerException(const archiveVersion : Integer);
begin
	raise EFilerException.Create(ClassName+cUnknownArchiveVersion+IntToStr(archiveVersion)); //:IGNORE
end;

// SaveToStream
//
procedure TPersistentObject.SaveToStream(stream : TStream);
var
   wr : TWriter;
begin
   wr:=TWriter.Create(stream, 16384);
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
procedure TPersistentObject.LoadFromStream(stream : TStream);
var
   rd : TReader;
   sig : String;
begin
   rd:=TReader.Create(stream, 16384);
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
procedure TPersistentObject.SaveToFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TPersistentObject.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveToString
//
function TPersistentObject.SaveToString : String;
var
   ss : TStringStream;
begin
   ss:=TStringStream.Create('');
   try
      SaveToStream(ss);
   finally
      ss.Free;
   end;
end;

// LoadFromString
//
procedure TPersistentObject.LoadFromString(const data : String);
var
   ss : TStringStream;
begin
   ss:=TStringStream.Create(data);
   try
      LoadFromStream(ss);
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
	i, k : Integer;
begin
	k:=0;
	for i:=0 to FCount-1 do
		if FList^[i]<>nil then begin
			FList^[k]:=FList^[i];
			Inc(k);
		end;
	SetCount(k);
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
procedure TPersistentObjectList.WriteToFiler(writer : TWriter);
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
procedure TPersistentObjectList.ReadFromFilerWithEvent(reader : TReader; afterSenderObjectCreated : TNotifyEvent);
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
procedure TPersistentObjectList.ReadFromFiler(reader : TReader);
begin
	ReadFromFilerWithEvent(reader, nil);
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

