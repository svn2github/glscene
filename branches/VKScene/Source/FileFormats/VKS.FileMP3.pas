//
// This unit is part of the GLScene Project   
//
{: VKS.FileMP3<p>

	Support for MP3 format.<p>

	<b>History : </b><font size=-1><ul>
      <li>25/07/09 - DaStr - Added $I GLScene.inc  
      <li>06/05/09 - DanB - Creation from split from GLSoundFileObjects.pas
	</ul></font>
}
unit VKS.FileMP3;

interface

{$I VKScene.inc}

uses
  System.Classes, VKS.ApplicationFileIO, VKS.SoundFileObjects;

type

   // TVKMP3File
   //
   {: Support for MP3 format.<p>
      *Partial* support only, access to PCMData is NOT supported. }
   TVKMP3File = class (TVKSoundFile)
      private
         { Public Declarations }
         data : array of Byte; // used to store MP3 bitstream

      protected
         { Protected Declarations }

      public
         { Private Declarations }
         function CreateCopy(AOwner: TPersistent) : TDataFile; override;

         class function Capabilities : TDataFileCapabilities; override;

         procedure LoadFromStream(Stream: TStream); override;
         procedure SaveToStream(Stream: TStream); override;

         procedure PlayOnWaveOut; override;

	      function WAVData : Pointer; override;
         function WAVDataSize : Integer; override;
	      function PCMData : Pointer; override;
	      function LengthInBytes : Integer; override;
   end;

implementation

// ------------------
// ------------------ TVKMP3File ------------------
// ------------------

// CreateCopy
//
function TVKMP3File.CreateCopy(AOwner: TPersistent) : TDataFile;
begin
   Result:=inherited CreateCopy(AOwner);
   if Assigned(Result) then begin
      TVKMP3File(Result).data := Copy(data);
   end;
end;

// Capabilities
//
class function TVKMP3File.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

// LoadFromStream
//
procedure TVKMP3File.LoadFromStream(stream : TStream);
begin
   // MP3 isn't actually, just loaded directly...
   Assert(Assigned(stream));
   SetLength(data, stream.Size);
   if Length(data)>0 then
      stream.Read(data[0], Length(data));
end;

// SaveToStream
//
procedure TVKMP3File.SaveToStream(stream: TStream);
begin
   if Length(data)>0 then
      stream.Write(data[0], Length(data));
end;

// PlayOnWaveOut
//
procedure TVKMP3File.PlayOnWaveOut;
begin
   Assert(False, 'MP3 playback on WaveOut not supported.');
end;

// WAVData
//
function TVKMP3File.WAVData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[0]
   else Result:=nil;
end;

// WAVDataSize
//
function TVKMP3File.WAVDataSize : Integer;
begin
   Result:=Length(data);
end;

// PCMData
//
function TVKMP3File.PCMData : Pointer;
begin
   Result:=nil;
end;

// LengthInBytes
//
function TVKMP3File.LengthInBytes : Integer;
begin
   Result:=0;
end;

initialization

  RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TVKMP3File);

end.
