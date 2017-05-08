//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   An abstract scripting interface for GLScene
   This unit provides the base methods for compiling and executing scripts as
   well as calling scripted functions. No scripting APIs are implemented here,
   only abstracted functions. 
     
}
unit VXS.ScriptBase;

interface

uses
  System.Classes, VXS.XCollection;

type
  TVXScriptState = ( ssUncompiled,    // The script has yet to be compiled.
                     ssCompileErrors, // Errors occurred while compiling.
                     ssCompiled,      // The script has been compiled and is
                                      // ready to be executed/started.
                     ssRunningErrors, // Errors occured while the script was
                                      // running.
                     ssRunning );     // The script is currently active and
                                      // is running without error.

  // TVXScriptBase
  //
  { The base script class that defines the abstract functions and properties. 
     Don't use this class directly, use the script classes descended from this 
     base class.  }
  TVXScriptBase = class(TVXXCollectionItem)
		private
      
      FText : TStringList;
      FDescription : String;
      FErrors : TStringList; // not persistent

		protected
			
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      function GetState : TVXScriptState; virtual; abstract;
      procedure SetText(const Value : TStringList);
      procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;

		public
      
      constructor Create(aOwner : TVXXCollection); override;
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
      property State : TVXScriptState read GetState;

		published
      
      property Text : TStringList read FText write SetText;
      property Description : String read FDescription write FDescription;

  end;

  // TVXScripts
  //
  { XCollection descendant for storing and handling scripts. }
  TVXScripts = class(TVXXCollection)
		private
			

		protected
			
      function GetItems(index : Integer) : TVXScriptBase;

		public
			
			procedure Assign(Source: TPersistent); override;

      class function ItemsClass : TVXXCollectionItemClass; override;

      function CanAdd(aClass : TVXXCollectionItemClass) : Boolean; override;
      property Items[index : Integer] : TVXScriptBase read GetItems; default;

  end;

  // TVXScriptLibrary
  //
  { Encapsulation of the scripts XCollection to help with script handling at
     design-time. Links the scripts to Delphi's persistence model. }
  TVXScriptLibrary = class (TComponent)
    private
      
      FScripts : TVXScripts;

    protected
      
      procedure DefineProperties(Filer : TFiler); override;
      procedure WriteScriptsData(Stream : TStream);
      procedure ReadScriptsData(Stream : TStream);
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      
      property Scripts : TVXScripts read FScripts;

  end;

implementation

// ---------------
// --------------- TVXScriptBase ---------------
// ---------------

// Create
//
constructor TVXScriptBase.Create(aOwner: TVXXCollection);
begin
  inherited;
  FText:=TStringList.Create;
  FErrors:=TStringList.Create;
end;

// Destroy
//
destructor TVXScriptBase.Destroy;
begin
  FText.Free;
  FErrors.Free;
  inherited;
end;

// Assign
//
procedure TVXScriptBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TVXScriptBase then begin
    Text.Assign(TVXScriptBase(Source).Text);
    Description:=TVXScriptBase(Source).Description;
  end;
end;

// ReadFromFiler
//
procedure TVXScriptBase.ReadFromFiler(reader: TReader);
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
procedure TVXScriptBase.WriteToFiler(writer: TWriter);
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
procedure TVXScriptBase.SetText(const Value : TStringList);
begin
  Text.Assign(Value);
end;

// Notification
//
procedure TVXScriptBase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Virtual
end;

// ---------------
// --------------- TVXScripts ---------------
// ---------------

// Assign
//
procedure TVXScripts.Assign(Source: TPersistent);
begin
  inherited;
  // Nothing yet
end;

// GetItems
//
function TVXScripts.GetItems(index: Integer): TVXScriptBase;
begin
  Result:=TVXScriptBase(inherited GetItems(index));
end;

// ItemsClass
//
class function TVXScripts.ItemsClass: TVXXCollectionItemClass;
begin
  Result:=TVXScriptBase;
end;

// CanAdd
//
function TVXScripts.CanAdd(aClass: TVXXCollectionItemClass): Boolean;
begin
  Result:=aClass.InheritsFrom(TVXScriptBase);
end;


// ---------------
// --------------- TVXScriptLibrary ---------------
// ---------------

// Create
//
constructor TVXScriptLibrary.Create(AOwner : TComponent);
begin
  inherited;
  FScripts:=TVXScripts.Create(Self);
end;

// Destroy
//
destructor TVXScriptLibrary.Destroy;
begin
  FScripts.Free;
  inherited;
end;

// DefineProperties
//
procedure TVXScriptLibrary.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ScriptsData',
    ReadScriptsData, WriteScriptsData, (Scripts.Count>0));
end;

// WriteScriptsData
//
procedure TVXScriptLibrary.WriteScriptsData(Stream : TStream);
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
procedure TVXScriptLibrary.ReadScriptsData(Stream : TStream);
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
procedure TVXScriptLibrary.Loaded;
begin
  inherited;
  Scripts.Loaded;
end;

// Notification
//
procedure TVXScriptLibrary.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : Integer;
begin
  if Assigned(Scripts) then
    for i:=0 to Scripts.Count-1 do
      Scripts[i].Notification(AComponent, Operation);
  inherited;
end;

initialization

  RegisterClasses([TVXScriptLibrary, TVXScripts, TVXScriptBase]);

end.
