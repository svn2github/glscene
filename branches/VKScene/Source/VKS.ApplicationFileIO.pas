//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
   Components and functions that abstract file I/O access for an application.
   Allows re-routing file reads to reads from a single archive file f.i.
   The history is logged in a former GLS version of the unit.
}
unit VKS.ApplicationFileIO;

interface

{$I VKScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  FMX.Dialogs,
  VKS.BaseClasses;


const
  VKS_RC_DDS_Type = RT_RCDATA;
  VKS_RC_JPG_Type = RT_RCDATA;
  VKS_RC_XML_Type = RT_RCDATA;
  VKS_RC_String_Type = RT_RCDATA;

type

  TVKSApplicationResource = (
    aresNone,
    aresSplash,
    aresTexture,
    aresMaterial,
    aresSampler,
    aresFont,
    aresMesh);

  // TAFIOCreateFileStream
  //
  TAFIOCreateFileStream = function(const fileName: string; mode: Word): TStream;

  // TAFIOFileStreamExists
  //
  TAFIOFileStreamExists = function(const fileName: string): Boolean;

  // TAFIOFileStreamEvent
  //
   TAFIOFileStreamEvent = procedure (const fileName : String; mode : Word;var stream : TStream) of object;

  // TAFIOFileStreamExistsEvent
  //
  TAFIOFileStreamExistsEvent = function(const fileName: string): Boolean of object;

  // TVKApplicationFileIO
  //
    { Allows specifying a custom behaviour for VKS.ApplicationFileIO's CreateFileStream. 
       The component should be considered a helper only, you can directly specify
       a function via the vAFIOCreateFileStream variable. 
       If multiple TVKApplicationFileIO components exist in the application,
       the last one created will be the active one. }
  TVKApplicationFileIO = class(TComponent)
  private
    
    FOnFileStream: TAFIOFileStreamEvent;
    FOnFileStreamExists: TAFIOFileStreamExistsEvent;

  protected
    

  public
    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    
      { Event that allows you to specify a stream for the file. 
         Destruction of the stream is at the discretion of the code that
         invoked CreateFileStream. Return nil to let the default mechanism
         take place (ie. attempt a regular file system access). }
    property OnFileStream: TAFIOFileStreamEvent read FOnFileStream write FOnFileStream;
    { Event that allows you to specify if a stream for the file exists.  }
    property OnFileStreamExists: TAFIOFileStreamExistsEvent read FOnFileStreamExists write FOnFileStreamExists;
  end;

  // TVKDataFileCapabilities
  //
  TVKDataFileCapability = (dfcRead, dfcWrite);
  TVKDataFileCapabilities = set of TVKDataFileCapability;

  // TVKDataFile
  //
  { Abstract base class for data file formats interfaces. 
     This class declares base file-related behaviours, ie. ability to load/save
     from a file or a stream. 
     It is highly recommended to overload ONLY the stream based methods, as the
     file-based one just call these, and stream-based behaviours allow for more
     enhancement (such as other I/O abilities, compression, cacheing, etc.)
     to this class, without the need to rewrite subclasses. }
  TVKDataFile = class(TVKUpdateAbleObject)
  private
    
    FResourceName: string;
    procedure SetResourceName(const AName: string);
  public
    

    { Describes what the TVKDataFile is capable of. 
       Default value is [dfcRead]. }
    class function Capabilities: TVKDataFileCapabilities; virtual;

    { Duplicates Self and returns a copy. 
       Subclasses should override this method to duplicate their data. }
    function CreateCopy(AOwner: TPersistent): TVKDataFile; dynamic;

    procedure LoadFromFile(const fileName: string); dynamic;
    procedure SaveToFile(const fileName: string); dynamic;
    procedure LoadFromStream(stream: TStream); dynamic;
    procedure SaveToStream(stream: TStream); dynamic;
    procedure Initialize; dynamic;
    { Optionnal resource name. 
       When using LoadFromFile/SaveToFile, the filename is placed in it,
       when using the Stream variants, the caller may place the resource
       name in it for parser use. }
    property ResourceName: string read FResourceName write SetResourceName;
  end;

  TVKDataFileClass = class of TVKDataFile;
  TVKSResourceStream = TResourceStream;

  // Returns true if an VKS.ApplicationFileIO has been defined
function ApplicationFileIODefined: Boolean;

{ Creates a file stream corresponding to the fileName. 
   If the file does not exists, an exception will be triggered. 
   Default mechanism creates a regular TFileStream, the 'mode' parameter
   is similar to the one for TFileStream. }
function CreateFileStream(const fileName: string;
  mode: Word = fmOpenRead + fmShareDenyNone): TStream;
{ Queries is a file stream corresponding to the fileName exists.  }
function FileStreamExists(const fileName: string): Boolean;

{ Create a resource stream. }
function CreateResourceStream(const ResName: string; ResType: PChar): TVKSResourceStream;

function StrToGLSResType(const AStrRes: string): TVKSApplicationResource;

var
  vAFIOCreateFileStream: TAFIOCreateFileStream = nil;
  vAFIOFileStreamExists: TAFIOFileStreamExists = nil;

  // ---------------------------------------------------------------------
  // ---------------------------------------------------------------------
  // ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

var
  vAFIO: TVKApplicationFileIO = nil;

// ApplicationFileIODefined
//
function ApplicationFileIODefined: Boolean;
begin
  Result := (Assigned(vAFIOCreateFileStream) and Assigned(vAFIOFileStreamExists))
    or Assigned(vAFIO);
end;

// CreateFileStream
//

function CreateFileStream(const fileName: string;
  mode: Word = fmOpenRead + fmShareDenyNone): TStream;
begin
  if Assigned(vAFIOCreateFileStream) then
    Result := vAFIOCreateFileStream(fileName, mode)
  else
  begin
      Result:=nil;
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStream) then
         vAFIO.FOnFileStream(fileName, mode, Result);
      if not Assigned(Result) then begin
         if ((mode and fmCreate)=fmCreate) or FileExists(fileName) then
            Result:=TFileStream.Create(fileName, mode)
         else raise Exception.Create('File not found: "'+fileName+'"');
      end;
   end;
end;

// FileStreamExists
//
function FileStreamExists(const fileName: string): Boolean;
begin
  if Assigned(vAFIOFileStreamExists) then
    Result := vAFIOFileStreamExists(fileName)
  else
  begin
    if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStreamExists) then
      Result := vAFIO.FOnFileStreamExists(fileName)
    else
      Result := FileExists(fileName);
  end;
end;

// FileStreamExists
//
function CreateResourceStream(const ResName: string; ResType: PChar): TVKSResourceStream;
var
  InfoBlock: HRSRC;
begin
  Result := nil;
  InfoBlock := FindResource(HInstance, PChar(ResName), ResType);
  if InfoBlock <> 0 then
    Result := TResourceStream.Create(HInstance, ResName, ResType)
  else
    ShowMessage(Format('Can''t create stream of application resource "%s"', [ResName]));
end;

// ------------------
// ------------------ TVKApplicationFileIO ------------------
// ------------------

// Create
//
constructor TVKApplicationFileIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  vAFIO := Self;
end;

// Destroy
//

destructor TVKApplicationFileIO.Destroy;
begin
  vAFIO := nil;
  inherited Destroy;
end;


// ------------------
// ------------------ TVKDataFile ------------------
// ------------------

// Capabilities
//
class function TVKDataFile.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead];
end;


// CreateCopy
//
function TVKDataFile.CreateCopy(AOwner: TPersistent): TVKDataFile;
begin
  if Self <> nil then
    Result := TVKDataFileClass(Self.ClassType).Create(AOwner)
  else
    Result := nil;
end;

// LoadFromFile
//

procedure TVKDataFile.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

// SaveToFile
//

procedure TVKDataFile.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

// LoadFromStream
//

procedure TVKDataFile.LoadFromStream(stream: TStream);
begin
  Assert(False, 'Imaport for ' + ClassName + ' to ' + stream.ClassName + ' not available.');
end;

// SaveToStream
//

procedure TVKDataFile.SaveToStream(stream: TStream);
begin
  Assert(False, 'Export for ' + ClassName + ' to ' + stream.ClassName + ' not available.');
end;

procedure TVKDataFile.Initialize;
begin
end;

procedure TVKDataFile.SetResourceName(const AName: string);
begin
  FResourceName := AName;
end;

function StrToGLSResType(const AStrRes: string): TVKSApplicationResource;
begin
  if AStrRes = '[SAMPLERS]' then
  begin
    Result := aresSampler;
  end
  else if AStrRes = '[TEXTURES]' then
  begin
    Result := aresTexture;
  end
  else if AStrRes = '[MATERIALS]' then
  begin
    Result := aresMaterial;
  end
  else if AStrRes = '[STATIC MESHES]' then
  begin
    Result := aresMesh;
  end
  else if AStrRes = '[SPLASH]' then
  begin
    Result := aresSplash;
  end
  else if AStrRes = '[FONTS]' then
  begin
    Result := aresFont;
  end
  else
    Result := aresNone;
end;

end.

