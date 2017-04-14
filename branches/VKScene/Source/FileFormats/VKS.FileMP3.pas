//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Support for MP3 format.
}
unit VKS.FileMP3;

interface

{$I VKScene.inc}

uses
  System.Classes,
  VKS.ApplicationFileIO,
  VKS.SoundFileObjects;

type

  { Support for MP3 format.
    *Partial* support only, access to PCMData is NOT supported. }
  TVKMP3File = class(TVKSoundFile)
  private
    data: array of Byte; // used to store MP3 bitstream
  protected
  public
    function CreateCopy(AOwner: TPersistent): TVKDataFile; override;
    class function Capabilities: TVKDataFileCapabilities; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure PlayOnWaveOut; override;
    function WAVData: Pointer; override;
    function WAVDataSize: Integer; override;
    function PCMData: Pointer; override;
    function LengthInBytes: Integer; override;
  end;

//=============================================================
implementation
//=============================================================

// ------------------
// ------------------ TVKMP3File ------------------
// ------------------

function TVKMP3File.CreateCopy(AOwner: TPersistent): TVKDataFile;
begin
  Result := inherited CreateCopy(AOwner);
  if Assigned(Result) then
  begin
    TVKMP3File(Result).data := Copy(data);
  end;
end;

class function TVKMP3File.Capabilities: TVKDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TVKMP3File.LoadFromStream(Stream: TStream);
begin
  // MP3 isn't actually, just loaded directly...
  Assert(Assigned(Stream));
  SetLength(data, Stream.Size);
  if Length(data) > 0 then
    Stream.Read(data[0], Length(data));
end;

procedure TVKMP3File.SaveToStream(Stream: TStream);
begin
  if Length(data) > 0 then
    Stream.Write(data[0], Length(data));
end;

procedure TVKMP3File.PlayOnWaveOut;
begin
  Assert(False, 'MP3 playback on WaveOut not supported.');
end;

function TVKMP3File.WAVData: Pointer;
begin
  if Length(data) > 0 then
    Result := @data[0]
  else
    Result := nil;
end;

function TVKMP3File.WAVDataSize: Integer;
begin
  Result := Length(data);
end;

function TVKMP3File.PCMData: Pointer;
begin
  Result := nil;
end;

function TVKMP3File.LengthInBytes: Integer;
begin
  Result := 0;
end;

//------------------------------------------------------------------
initialization
//------------------------------------------------------------------

RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TVKMP3File);

end.
