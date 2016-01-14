//
// VKScene project based on GLScene library, http://glscene.sourceforge.net
//
{
  A polymorphism-enabled TCollection-like set of classes
 
     
}
unit GLS.XCollection;

{.$DEFINE DEBUG_XCOLLECTION }  // on define the most apps will not work

interface

{$I GLScene.inc}

uses
  System.Classes, System.SysUtils, System.Types,

  GLS.CrossPlatform, GLS.PersistentClasses
  {$IFDEF DEBUG_XCOLLECTION}, TypInfo {$ENDIF};


type
  TVKXCollection = class;

  EFilerException = class(Exception)
  end;

  // TVKXCollectionItem

  { Base class for implementing a XCollection item. 
    NOTES : 
     Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
     Subclasses must be registered using the RegisterXCollectionItemClass
    function for proper operation
      }

  TVKXCollectionItem = class(TGLInterfacedPersistent)
  private
    { Private Declarations }
    FOwner: TVKXCollection;
    FName: string;

  protected
    { Protected Declarations }
    function GetName: string; virtual;
    procedure SetName(const val: string); virtual;
    function GetOwner: TPersistent; override;

    { Override this function to write subclass data. }
    procedure WriteToFiler(writer: TWriter); virtual;
    { Override this function to read subclass data. }
    procedure ReadFromFiler(reader: TReader); virtual;
    { Override to perform things when owner form has been loaded. }
    procedure Loaded; dynamic;

    { Triggers an EFilerException with appropriate version message. }
    procedure RaiseFilerException(const archiveVersion: integer);

  public
    { Public Declarations }
    constructor Create(aOwner: TVKXCollection); virtual;
    destructor Destroy; override;

    function GetNamePath: string; override;
    property Owner: TVKXCollection read FOwner;

    { Default implementation uses WriteToFiler/ReadFromFiler.  }
    procedure Assign(Source: TPersistent); override;

    procedure MoveUp;
    procedure MoveDown;
    function Index: integer;

    { Returns a user-friendly denomination for the class. 
      This denomination is used for picking a texture image class
      in the IDE expert. }
    class function FriendlyName: String; virtual; {$IFNDEF VKS_CPPB}abstract;
    {$ENDIF}
    { Returns a user-friendly description for the class. 
      This denomination is used for helping the user when picking a
      texture image class in the IDE expert. If it's not overriden,
      takes its value from FriendlyName. }
    class function FriendlyDescription: String; virtual;
    { Category of the item class. 
      This is a free string, it will used by the XCollectionEditor to
      regroup collectionitems and menu items }
    class function ItemCategory: string; virtual;
    { If true only one such XCollectionItem is allowed per BaseSceneObject. 
      Inheritance is accounted for UniqueXCollectionItem resolution, ie.
      if TClassA is unique, and TClassB is a subclass of TClassA,
      whatever the unicity of TClassB, TClassA and TClassB won't be allowed
      to mix (since TClassB is a TClassA, and TClassA is unique). 
      Attempting to break the unicity rules will not be possible at
      design-time (in Delphi IDE) and will trigger an exception at run-time. }
    class function UniqueItem: Boolean; virtual;
    { Allows the XCollectionItem class to determine if it should be allowed
      to be added to the given collection. }
    class function CanAddTo(collection: TVKXCollection): Boolean; virtual;

  published
    { Published Declarations }
    property Name: string read FName write SetName;
  end;

  TVKXCollectionItemClass = class of TVKXCollectionItem;

  // TVKXCollection

  { Holds a list of TVKXCollectionItem objects. 
    This class looks a lot like a polymorphic-enabled TCollection, it is
    a much stripped down version of a proprietary TObjectList and persistence
    classes (XClasses & XLists), if the copyrights are ever partially lifted
    on the originals, I'll base this code on them since they are way faster
    than Borland's lists and persistence mechanisms (and unlike Borland's,
    with polymorphism-support and full backward compatibility). }
  TVKXCollection = class(TPersistent)
  private
    { Private Declarations }
    FOwner: TPersistent;
    FList: TList;
    FCount: integer;

    { Archive Version is used to update the way data items is loaded. }
    FArchiveVersion: integer;
  protected
    { Protected Declarations }
    function GetItems(Index: integer): TVKXCollectionItem;
    function GetOwner: TPersistent; override;

    procedure ReadFromFiler(reader: TReader);
    procedure WriteToFiler(writer: TWriter);

  public
    { Public Declarations }
    constructor Create(aOwner: TPersistent); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Loaded;

    property Owner: TPersistent read FOwner write FOwner;
    function GetNamePath: string; override;

    { Class of the items.
      Unlike TCollection, items can be of ItemsClass OR ANY of its
      subclasses, ie. this function is used only for asserting your adding
      objects of the right class, and not for persistence. }
    class function ItemsClass: TVKXCollectionItemClass; virtual;

    property Items[index: integer]: TVKXCollectionItem read GetItems; default;
    property Count: integer read FCount;
    function Add(anItem: TVKXCollectionItem): integer;
    function GetOrCreate(anItem: TVKXCollectionItemClass): TVKXCollectionItem;
    procedure Delete(Index: integer);
    procedure Remove(anItem: TVKXCollectionItem);
    procedure Clear;
    function IndexOf(anItem: TVKXCollectionItem): integer;
    // : Returns the index of the first XCollectionItem of the given class (or -1)
    function IndexOfClass(aClass: TVKXCollectionItemClass): integer;
    // : Returns the first XCollection of the given class (or nil)
    function GetByClass(aClass: TVKXCollectionItemClass): TVKXCollectionItem;
    // : Returns the index of the first XCollectionItem of the given name (or -1)
    function IndexOfName(const aName: string): integer;
    { Indicates if an object of the given class can be added. 
      This function is used to enforce Unique XCollection. }
    function CanAdd(aClass: TVKXCollectionItemClass): Boolean; virtual;

    property archiveVersion: integer read FArchiveVersion;
  end;

resourcestring
  cUnknownArchiveVersion = 'Unknown archive version : ';

  { Registers an event to be called when an XCollection is destroyed. }
procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
{ DeRegisters event. }
procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);

{ Registers a TVKXCollectionItem subclass for persistence requirements. }
procedure RegisterXCollectionItemClass(aClass: TVKXCollectionItemClass);
{ Removes a TVKXCollectionItem subclass from the list. }
procedure UnregisterXCollectionItemClass(aClass: TVKXCollectionItemClass);
{ Retrieves a registered TVKXCollectionItemClass from its classname. }
function FindXCollectionItemClass(const ClassName: string)
  : TVKXCollectionItemClass;
{ Creates and returns a copy of internal list of TVKXCollectionItem classes. 
  Returned list should be freed by caller, the parameter defines an ancestor
  class filter. If baseClass is left nil, TVKXCollectionItem is used as ancestor. }
function GetXCollectionItemClassesList(baseClass
  : TVKXCollectionItemClass = nil): TList;
procedure GetXCollectionClassesList(var ClassesList: TList;
  baseClass: TVKXCollectionItemClass = nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  { Magic is a workaround that will allow us to know when the archive
    version is 0 (equivalent to : there is no ArchiveVersion stored in
    the DFM file) }
  MAGIC: array [0 .. 3] of AnsiChar = 'XCOL';

var
  vXCollectionItemClasses: TList;
  vXCollectionDestroyEvent: TNotifyEvent;

  // Dummy method for CPP
{$IFDEF VKS_CPPB}

class function TVKXCollectionItem.FriendlyName: String;
begin
  result := '';
end;
{$ENDIF}
// ---------- internal global routines (used by xcollection editor) -------------

// RegisterXCollectionDestroyEvent

procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := notifyEvent;
end;

// DeRegisterXCollectionDestroyEvent

procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := nil;
end;

// ------------------------------------------------------------------------------

// RegisterXCollectionItemClass

procedure RegisterXCollectionItemClass(aClass: TVKXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then
    vXCollectionItemClasses := TList.Create;
  if vXCollectionItemClasses.IndexOf(aClass) < 0 then
    vXCollectionItemClasses.Add(aClass);
end;

// UnregisterXCollectionItemClass

procedure UnregisterXCollectionItemClass(aClass: TVKXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then
    exit;
  if vXCollectionItemClasses.IndexOf(aClass) >= 0 then
    vXCollectionItemClasses.Remove(aClass);
end;

// FindXCollectionItemClass

function FindXCollectionItemClass(const ClassName: string)
  : TVKXCollectionItemClass;
var
  i: integer;
begin
  result := nil;
  if Assigned(vXCollectionItemClasses) then
    for i := 0 to vXCollectionItemClasses.Count - 1 do
      if TVKXCollectionItemClass(vXCollectionItemClasses[i]).ClassName = ClassName
      then
      begin
        result := TVKXCollectionItemClass(vXCollectionItemClasses[i]);
        Break;
      end;
end;

// GetXCollectionItemClassesList

function GetXCollectionItemClassesList(baseClass
  : TVKXCollectionItemClass = nil): TList;
begin
  result := TList.Create;
  GetXCollectionClassesList(result, baseClass);
end;

procedure GetXCollectionClassesList(var ClassesList: TList;
  baseClass: TVKXCollectionItemClass = nil);
var
  i: integer;
begin
  if not Assigned(baseClass) then
    baseClass := TVKXCollectionItem;
  if Assigned(vXCollectionItemClasses) then
    for i := 0 to vXCollectionItemClasses.Count - 1 do
      if TVKXCollectionItemClass(vXCollectionItemClasses[i])
        .InheritsFrom(baseClass) then
        ClassesList.Add(vXCollectionItemClasses[i]);
end;

// ------------------
// ------------------ TVKXCollectionItem ------------------
// ------------------

// Create
//
constructor TVKXCollectionItem.Create(aOwner: TVKXCollection);
begin
  inherited Create;
  FOwner := aOwner;
  if Assigned(aOwner) then
  begin
    Assert(aOwner.CanAdd(TVKXCollectionItemClass(Self.ClassType)),
      'Addition of ' + Self.ClassName + ' to ' + aOwner.ClassName +
      ' rejected.');
    aOwner.FList.Add(Self);
    aOwner.FCount := aOwner.FList.Count;
  end;
end;

// Destroy

destructor TVKXCollectionItem.Destroy;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FList.Remove(Self);
    FOwner.FCount := FOwner.FList.Count;
  end;
  inherited Destroy;
end;

// Assign
//
procedure TVKXCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TVKXCollectionItem then
  begin
    FName := TVKXCollectionItem(Source).Name;
  end
  else
    inherited Assign(Source);
end;

// SetName
//
procedure TVKXCollectionItem.SetName(const val: string);
begin
  FName := val;
end;

// GetOwner

function TVKXCollectionItem.GetOwner: TPersistent;
begin
  result := FOwner;
end;

// WriteToFiler

procedure TVKXCollectionItem.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
  end;
end;

// ReadFromFiler
//
procedure TVKXCollectionItem.ReadFromFiler(reader: TReader);
var
  ver: integer;
begin
  with reader do
  begin
    ver := ReadInteger;
    Assert(ver = 0);
    FName := ReadString;
  end;
end;

// Loaded

procedure TVKXCollectionItem.Loaded;
begin
  // does nothing by default
end;

// GetName

function TVKXCollectionItem.GetName: string;
begin
  result := FName;
end;

// GetNamePath

function TVKXCollectionItem.GetNamePath: string;
begin
  if FOwner <> nil then
    result := Format('%s[%d]', [FOwner.GetNamePath, Index])
  else
    result := inherited GetNamePath;
end;

// MoveUp

procedure TVKXCollectionItem.MoveUp;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if i > 0 then
      Owner.FList.Exchange(i, i - 1);
  end;
end;

// MoveDown

procedure TVKXCollectionItem.MoveDown;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if cardinal(i) < cardinal(Owner.FList.Count - 1) then
      Owner.FList.Exchange(i, i + 1);
  end;
end;

// Index

function TVKXCollectionItem.Index: integer;
begin
  if Assigned(Owner) then
    result := Owner.FList.IndexOf(Self)
  else
    result := -1;
end;

// RaiseFilerException

procedure TVKXCollectionItem.RaiseFilerException(const archiveVersion: integer);
begin
  raise EFilerException.Create(ClassName + cUnknownArchiveVersion +
    IntToStr(archiveVersion));
end;

// FriendlyDescription

class function TVKXCollectionItem.FriendlyDescription: string;
begin
  result := FriendlyName;
end;

// ItemCategory

class function TVKXCollectionItem.ItemCategory: string;
begin
  result := '';
end;

// UniqueXCollectionItem

class function TVKXCollectionItem.UniqueItem: Boolean;
begin
  result := False;
end;

// CanAddTo

class function TVKXCollectionItem.CanAddTo(collection: TVKXCollection): Boolean;
begin
  result := True;
end;


// ------------------
// ------------------ TVKXCollection ------------------
// ------------------

// Create

constructor TVKXCollection.Create(aOwner: TPersistent);
begin
  inherited Create;
  FOwner := aOwner;
  FList := TList.Create;
end;

// Destroy

destructor TVKXCollection.Destroy;
begin
  if Assigned(vXCollectionDestroyEvent) then
    vXCollectionDestroyEvent(Self);
  Clear;
  FList.Free;
  inherited Destroy;
end;

// Assign

procedure TVKXCollection.Assign(Source: TPersistent);
var
  i: integer;
  srcItem, newItem: TVKXCollectionItem;
begin
  if not Assigned(Source) then
  begin
    Clear;
  end
  else if Source.ClassType = Self.ClassType then
  begin
    Clear;
    FList.Capacity := TVKXCollection(Source).FList.Count;
    for i := 0 to TVKXCollection(Source).Count - 1 do
    begin
      srcItem := TVKXCollectionItem(TVKXCollection(Source).FList[i]);
      newItem := TVKXCollectionItemClass(srcItem.ClassType).Create(Self);
      newItem.Assign(srcItem);
    end;
  end
  else
    inherited Assign(Source);
  FCount := FList.Count;
end;

// Loaded

procedure TVKXCollection.Loaded;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TVKXCollectionItem(FList[i]).Loaded;
end;

// WriteToFiler

procedure TVKXCollection.WriteToFiler(writer: TWriter);
var
  i, n: integer;
  classList: TList;
  XCollectionItem: TVKXCollectionItem;
begin
  // Here, we write all listed XCollection through their WriteToFiler methods,
  // but to be able to restore them, we also write their classname, and to
  // avoid wasting space if the same class appears multiple times we build up
  // a lookup table while writing them, if the class is anew, the name is
  // written, otherwise, only the index in the table is written.
  // Using a global lookup table (instead of a "per-WriteData" one) could save
  // more space, but would also increase dependencies, and this I don't want 8)
  FArchiveVersion := 1;
  classList := TList.Create;
  try
    with writer do
    begin
      // Magic header and archive version are always written now
      WriteInteger(PInteger(@MAGIC[0])^);
      WriteInteger(FArchiveVersion);

      WriteInteger(FList.Count);
      for i := 0 to FList.Count - 1 do
      begin
        XCollectionItem := TVKXCollectionItem(FList[i]);
        n := classList.IndexOf(XCollectionItem.ClassType);
        if n < 0 then
        begin
          WriteString(XCollectionItem.ClassName);
          classList.Add(XCollectionItem.ClassType);
        end
        else
          WriteInteger(n);
        XCollectionItem.WriteToFiler(writer);
      end;
    end;
  finally
    classList.Free;
  end;
end;

// ReadFromFiler
//
procedure TVKXCollection.ReadFromFiler(reader: TReader);
var
  vt: TValueType;
  Header: array [0 .. 3] of AnsiChar;
  n, lc, lcnum: integer;
  classList: TList;
  cName: string;
  XCollectionItemClass: TVKXCollectionItemClass;
  XCollectionItem: TVKXCollectionItem;
begin
  // see WriteData for a description of what is going on here
  Clear;
  classList := TList.Create;
  try
    with reader do
    begin
      // save current reader position, it will be used to rewind the reader if the DFM is too old
      try
        vt := NextValue;
        if vt in [vaInt32, vaInt16, vaInt8] then
          PInteger(@Header[0])^ := ReadInteger
        else
        begin
          Read(Header[0], Length(Header));
        end;
      except
        Header[0] := #0;
        Header[1] := #0;
        Header[2] := #0;
        Header[3] := #0;
      end;

      // after reading the header, we need to compare it with the MAGIC reference
      if (Header[0] = MAGIC[0]) and (Header[1] = MAGIC[1]) and
        (Header[2] = MAGIC[2]) and (Header[3] = MAGIC[3]) then
      begin
        // if its ok we can just read the archive version
        FArchiveVersion := ReadInteger;
        lc := ReadInteger;
      end
      else
      begin
        // if the header is invalid (old DFM) just assume archive version is 0 and rewind reader
        FArchiveVersion := 0;
        lc := PInteger(@Header[0])^;
      end;

      for n := 1 to lc do
      begin
        if NextValue in [vaString, vaLString] then
        begin
          cName := ReadString;
{$IFDEF DEBUG_XCOLLECTION}
          writeln('TVKXCollection.ReadFromFiler create class entry: ', cName);
{$ENDIF}
          XCollectionItemClass := FindXCollectionItemClass(cName);
          Assert(Assigned(XCollectionItemClass),
            'Class ' + cName +
            ' unknown. Add the relevant unit to your "uses".');
          classList.Add(XCollectionItemClass);
        end
        else
        begin
{$IFDEF DEBUG_XCOLLECTION}
          Assert(NextValue in [vaInt8, vaInt16, vaInt32],
            'Non-Integer ValueType: ' + GetEnumName(TypeInfo(TValueType),
            Ord(NextValue)));
{$ENDIF}
          lcnum := ReadInteger;
          Assert((lcnum >= 0) and (lcnum < classList.Count),
            'Invalid classlistIndex: ' + IntToStr(lcnum));
          XCollectionItemClass := TVKXCollectionItemClass(classList[lcnum]);
{$IFDEF DEBUG_XCOLLECTION}
          writeln('TVKXCollection.ReadFromFiler create by number: ', lcnum,
            ' -> ', XCollectionItemClass.ClassName);
{$ENDIF}
        end;

        if Assigned(XCollectionItemClass) then
        begin
          XCollectionItem := XCollectionItemClass.Create(Self);
          XCollectionItem.ReadFromFiler(reader);
        end;
      end;
    end;
  finally
    classList.Free;
  end;
  FCount := FList.Count;
end;

// ItemsClass
//
class function TVKXCollection.ItemsClass: TVKXCollectionItemClass;
begin
  result := TVKXCollectionItem;
end;

// GetItems
//
function TVKXCollection.GetItems(Index: integer): TVKXCollectionItem;
begin
  result := TVKXCollectionItem(FList[index]);
end;

// GetOwner
//
function TVKXCollection.GetOwner: TPersistent;
begin
  result := FOwner;
end;

// GetNamePath
//
function TVKXCollection.GetNamePath: string;
var
  s: string;
begin
  result := ClassName;
  if GetOwner = nil then
    exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    exit;
  result := s + '.XCollection';
end;

// Add
//
function TVKXCollection.Add(anItem: TVKXCollectionItem): integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  Assert(CanAdd(TVKXCollectionItemClass(anItem.ClassType)));
  if Assigned(anItem.FOwner) then
  begin
    anItem.FOwner.FList.Remove(anItem);
    anItem.FOwner.FCount := anItem.FOwner.FList.Count;
  end;
  anItem.FOwner := Self;
  result := FList.Add(anItem);
  FCount := FList.Count;
end;

// GetOrCreate
//
function TVKXCollection.GetOrCreate(anItem: TVKXCollectionItemClass)
  : TVKXCollectionItem;
var
  i: integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  i := Self.IndexOfClass(anItem);
  if i >= 0 then
    result := TVKXCollectionItem(Self[i])
  else
    result := anItem.Create(Self);
end;

// Delete
//
procedure TVKXCollection.Delete(Index: integer);
begin
  Assert(cardinal(index) < cardinal(FList.Count));
  // doin' it the fast way
  with TVKXCollectionItem(FList[index]) do
  begin
    FOwner := nil;
    Free;
  end;
  FList.Delete(index);
  FCount := FList.Count;
end;

// Remove
//
procedure TVKXCollection.Remove(anItem: TVKXCollectionItem);
var
  i: integer;
begin
  i := IndexOf(anItem);
  if i >= 0 then
    Delete(i);
end;

// Clear
//
procedure TVKXCollection.Clear;
var
  i: integer;
begin
  // Fast kill of owned XCollection
  for i := 0 to FList.Count - 1 do
    with TVKXCollectionItem(FList[i]) do
    begin
      FOwner := nil;
      Free;
    end;
  FList.Clear;
  FCount := 0;
end;

// IndexOf
//
function TVKXCollection.IndexOf(anItem: TVKXCollectionItem): integer;
begin
  result := FList.IndexOf(anItem);
end;

// IndexOfClass
//
function TVKXCollection.IndexOfClass(aClass: TVKXCollectionItemClass): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TVKXCollectionItem(FList[i]) is aClass then
    begin
      result := i;
      Break;
    end;
end;

// GetByClass
//
function TVKXCollection.GetByClass(aClass: TVKXCollectionItemClass)
  : TVKXCollectionItem;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if TVKXCollectionItem(FList[i]) is aClass then
    begin
      result := TVKXCollectionItem(FList[i]);
      Break;
    end;
end;

// IndexOfName
//
function TVKXCollection.IndexOfName(const aName: string): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TVKXCollectionItem(FList[i]).Name = aName then
    begin
      result := i;
      Break;
    end;
end;

// CanAdd
//
function TVKXCollection.CanAdd(aClass: TVKXCollectionItemClass): Boolean;
var
  i: integer;
  XCollectionItemClass: TVKXCollectionItemClass;
begin
  result := True;

  // Test if the class allows itself to be added to this collection
  if not aClass.CanAddTo(Self) then
  begin
    result := False;
    exit;
  end;

  // is the given class compatible with owned ones ?
  if aClass.UniqueItem then
    for i := 0 to Count - 1 do
    begin
      if Items[i] is aClass then
      begin
        result := False;
        Break;
      end;
    end;
  // are the owned classes compatible with the given one ?
  if result then
    for i := 0 to Count - 1 do
    begin
      XCollectionItemClass := TVKXCollectionItemClass(Items[i].ClassType);
      if (XCollectionItemClass.UniqueItem) and
        aClass.InheritsFrom(XCollectionItemClass) then
      begin
        result := False;
        Break;
      end;
    end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

vXCollectionItemClasses.Free;

end.
