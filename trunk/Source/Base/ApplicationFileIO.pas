// ApplicationFileIO
{: Components and fonction that abstract file I/O access for an application.<br>
   Allows re-routing file reads to reads from a single archive file f.i.<p>

	<b>History : </b><font size=-1><ul>
      <li>31/01/03 - Egg - Added FileExists mechanism
	   <li>21/11/02 - Egg - Creation
	</ul></font>
}
unit ApplicationFileIO;

interface

uses Classes, SysUtils;

type

   // TAFIOCreateFileStream
   //
   TAFIOCreateFileStream = function (const fileName : String; mode : Word) : TStream;

   // TAFIOFileStreamExists
   //
   TAFIOFileStreamExists = function (const fileName : String) : Boolean;

   // TAFIOFileStreamEvent
   //
   TAFIOFileStreamEvent = function (const fileName : String; mode : Word) : TStream of object;

   // TAFIOFileStreamExistsEvent
   //
   TAFIOFileStreamExistsEvent = function (const fileName : String) : Boolean of object;

	// TApplicationFileIO
	//
   {: Allows specifying a custom behaviour for ApplicationFileIO's CreateFileStream.<p>
      The component should be considered a helper only, you can directly specify
      a function via the vAFIOCreateFileStream variable.<br>
      If multiple TApplicationFileIO components exist in the application,
      the last one created will be the active one. }
	TApplicationFileIO = class (TComponent)
	   private
	      { Private Declarations }
         FOnFileStream : TAFIOFileStreamEvent;
         FOnFileStreamExists : TAFIOFileStreamExistsEvent;

	   protected
	      { Protected Declarations }

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

	   published
	      { Published Declarations }
         {: Event that allows you to specify a stream for the file.<p>
            Destruction of the stream is at the discretion of the code that
            invoked CreateFileStream. Return nil to let the default mechanism
            take place (ie. attempt a regular file system access). }
         property OnFileStream : TAFIOFileStreamEvent read FOnFileStream write FOnFileStream;
         {: Event that allows you to specify if a stream for the file exists.<p> }
         property OnFileStreamExists : TAFIOFileStreamExistsEvent read FOnFileStreamExists write FOnFileStreamExists;
	end;

{: Creates a file stream corresponding to the fileName.<p>
   If the file does not exists, an exception will be triggered.<br>
   Default mechanism creates a regular TFileStream, the 'mode' parameter
   is similar to the one for TFileStream. }
function CreateFileStream(const fileName : String;
                          mode : Word = fmOpenRead+fmShareDenyNone) : TStream;
{: Queries is a file stream corresponding to the fileName exists.<p> }
function FileStreamExists(const fileName : String) : Boolean;

procedure Register;

var
   vAFIOCreateFileStream : TAFIOCreateFileStream = nil;
   vAFIOFileStreamExists : TAFIOFileStreamExists = nil;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

var
   vAFIO : TApplicationFileIO = nil;

procedure Register;
begin
	RegisterComponents('GLScene', [TApplicationFileIO]);
end;

// CreateFileStream
//
function CreateFileStream(const fileName : String;
                          mode : Word = fmOpenRead+fmShareDenyNone) : TStream;
begin
   if Assigned(vAFIOCreateFileStream) then
      Result:=vAFIOCreateFileStream(fileName, mode)
   else begin
      Result:=nil;
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStream) then
         Result:=vAFIO.FOnFileStream(fileName, mode);
      if not Assigned(Result) then
         Result:=TFileStream.Create(fileName, mode);
   end;
end;

// FileStreamExists
//
function FileStreamExists(const fileName : String) : Boolean;
begin
   if Assigned(vAFIOFileStreamExists) then
      Result:=vAFIOFileStreamExists(fileName)
   else begin
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStreamExists) then
         Result:=vAFIO.FOnFileStreamExists(fileName)
      else Result:=FileExists(fileName);
   end;
end;

// ------------------
// ------------------ TApplicationFileIO ------------------
// ------------------

// Create
//
constructor TApplicationFileIO.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   vAFIO:=Self;
end;

// Destroy
//
destructor TApplicationFileIO.Destroy;
begin
   vAFIO:=nil;
	inherited Destroy;
end;

end.
