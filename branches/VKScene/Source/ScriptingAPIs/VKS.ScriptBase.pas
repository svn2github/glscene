//
// VKScene project based on GLScene library, http://glscene.sourceforge.net 
//
{
   An abstract scripting interface for GLScene
   This unit provides the base methods for compiling and executing scripts as
   well as calling scripted functions. No scripting APIs are implemented here,
   only abstracted functions. 
     
}
unit VKS.ScriptBase;

interface

uses
  System.Classes, VKS.XCollection;

type
  TVKScriptState = ( ssUncompiled,    // The script has yet to be compiled.
                     ssCompileErrors, // Errors occurred while compiling.
                     ssCompiled,      // The script has been compiled and is
                                      // ready to be executed/started.
                     ssRunningErrors, // Errors occured while the script was
                                      // running.
                     ssRunning );     // The script is currently active and
                                      // is running without error.

  // TVKScriptBase
  //
  { The base script class that defines the abstract functions and properties. 
     Don't use this class directly, use the script classes descended from this 
     base class.  }
  TVKScriptBase = class(TXCollectionItem)
		private
      { Private Declarations }
      FText : TStringList;
      FDescription : String;
      FErrors : TStringList; // not persistent

		protected
			{ Protected Declarations }
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      function GetState : TVKScriptState; virtual; abstract;
      procedure SetText(const Value : TStringList);
      procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;

		public
      { Public Declarations }
      constructor Create(aOwner : TXCollection); override;
      destructor Destroy; override;

      procedure Assign(Source: TPersistent); override;

      procedure Compile; virtual; abstract;
      procedure Start; virtual; abstract;
      procedure Stop; virtual; abstract;
      procedure Execute; virtual; abstract;
      procedure Invalidate; virtual; abstract;
      function Call(aName : String;
        aParams : array of Variant) : Variant; virtual; abstract;
      

      property Errors : TStringList read FErrors;
      property State : TVKScriptState read GetState;

		published
      { Published Declarations }
      property Text : TStringList read FText write SetText;
      property Description : String read FDescription write FDescription;

  end;

  // TVKScripts
  //
  { XCollection descendant for storing and handling scripts. }
  TVKScripts = class(TXCollection)
		private
			{ Private Declarations }

		protected
			{ Protected Declarations }
      function GetItems(index : Integer) : TVKScriptBase;

		public
			{ Public Declarations }
			procedure Assign(Source: TPersistent); override;

      class function ItemsClass : TXCollectionItemClass; override;

      function CanAdd(aClass : TXCollectionItemClass) : Boolean; override;
      property Items[index : Integer] : TVKScriptBase read GetItems; default;

  end;

  // TVKScriptLibrary
  //
  { Encapsulation of the scripts XCollection to help with script handling at
     design-time. Links the scripts to Delphi's persistence model. }
  TVKScriptLibrary = class (TComponent)
    private
      { Private Declarations }
      FScripts : TVKScripts;

    protected
      { Protected Declarations }
      procedure DefineProperties(Filer : TFiler); override;
      procedure WriteScriptsData(Stream : TStream);
      procedure ReadScriptsData(Stream : TStream);
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      { Public Declarations }
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      { Published Declarations }
      property Scripts : TVKScripts read FScripts;

  end;

implementation

// ---------------
// --------------- TVKScriptBase ---------------
// ---------------

// Create
//
constructor TVKScriptBase.Create(aOwner: TXCollection);
begin
  inherited;
  FText:=TStringList.Create;
  FErrors:=TStringList.Create;
end;

// Destroy
//
destructor TVKScriptBase.Destroy;
begin
  FText.Free;
  FErrors.Free;
  inherited;
end;

// Assign
//
procedure TVKScriptBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVKScriptBase then begin
    Text.Assign(TVKScriptBase(Source).Text);
    Description:=TVKScriptBase(Source).Description;
  end;
end;

// ReadFromFiler
//
procedure TVKScriptBase.ReadFromFiler(reader: TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FText.Text:=ReadString;
    FDescription:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TVKScriptBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0);

  with writer do begin
    WriteString(FText.Text);
    WriteString(FDescription);
  end;
end;

// SetText
//
procedure TVKScriptBase.SetText(const Value : TStringList);
begin
  Text.Assign(Value);
end;

// Notification
//
procedure TVKScriptBase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Virtual
end;

// ---------------
// --------------- TVKScripts ---------------
// ---------------

// Assign
//
procedure TVKScripts.Assign(Source: TPersistent);
begin
  inherited;
  // Nothing yet
end;

// GetItems
//
function TVKScripts.GetItems(index: Integer): TVKScriptBase;
begin
  Result:=TVKScriptBase(inherited GetItems(index));
end;

// ItemsClass
//
class function TVKScripts.ItemsClass: TXCollectionItemClass;
begin
  Result:=TVKScriptBase;
end;

// CanAdd
//
function TVKScripts.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result:=aClass.InheritsFrom(TVKScriptBase);
end;


// ---------------
// --------------- TVKScriptLibrary ---------------
// ---------------

// Create
//
constructor TVKScriptLibrary.Create(AOwner : TComponent);
begin
  inherited;
  FScripts:=TVKScripts.Create(Self);
end;

// Destroy
//
destructor TVKScriptLibrary.Destroy;
begin
  FScripts.Free;
  inherited;
end;

// DefineProperties
//
procedure TVKScriptLibrary.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ScriptsData',
    ReadScriptsData, WriteScriptsData, (Scripts.Count>0));
end;

// WriteScriptsData
//
procedure TVKScriptLibrary.WriteScriptsData(Stream : TStream);
var
  writer : TWriter;
begin
  writer:=TWriter.Create(stream, 16384);
  try
    Scripts.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadScriptsData
//
procedure TVKScriptLibrary.ReadScriptsData(Stream : TStream);
var
  reader : TReader;
begin
  reader:=TReader.Create(stream, 16384);
  try
    Scripts.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

// Loaded
//
procedure TVKScriptLibrary.Loaded;
begin
  inherited;
  Scripts.Loaded;
end;

// Notification
//
procedure TVKScriptLibrary.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : Integer;
begin
  if Assigned(Scripts) then
    for i:=0 to Scripts.Count-1 do
      Scripts[i].Notification(AComponent, Operation);
  inherited;
end;

initialization

  RegisterClasses([TVKScriptLibrary, TVKScripts, TVKScriptBase]);

end.
