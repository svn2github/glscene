//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  Support for MP3 format.
}
unit VXS.FileMP3;

interface

{$I VXScene.inc}

uses
  System.Classes,
  VXS.ApplicationFileIO,
  VXS.SoundFileObjects;

type

  { Support for MP3 format.
    *Partial* support only, access to PCMData is NOT supported. }
  TVXMP3File = class(TVXSoundFile)
  private
    data: array of Byte; // used to store MP3 bitstream
  protected
  public
    function CreateCopy(AOwner: TPersistent): TVXDataFile; override;
    class function Capabilities: TVXDataFileCapabilities; override;
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
// ------------------ TVXMP3File ------------------
// ------------------

function TVXMP3File.CreateCopy(AOwner: TPersistent): TVXDataFile;
begin
  Result := inherited CreateCopy(AOwner);
  if Assigned(Result) then
  begin
    TVXMP3File(Result).data := Copy(data);
  end;
end;

class function TVXMP3File.Capabilities: TVXDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TVXMP3File.LoadFromStream(Stream: TStream);
begin
  // MP3 isn't actually, just loaded directly...
  Assert(Assigned(Stream));
  SetLength(data, Stream.Size);
  if Length(data) > 0 then
    Stream.Read(data[0], Length(data));
end;

procedure TVXMP3File.SaveToStream(Stream: TStream);
begin
  if Length(data) > 0 then
    Stream.Write(data[0], Length(data));
end;

procedure TVXMP3File.PlayOnWaveOut;
begin
  Assert(False, 'MP3 playback on WaveOut not supported.');
end;

function TVXMP3File.WAVData: Pointer;
begin
  if Length(data) > 0 then
    Result := @data[0]
  else
    Result := nil;
end;

function TVXMP3File.WAVDataSize: Integer;
begin
  Result := Length(data);
end;

function TVXMP3File.PCMData: Pointer;
begin
  Result := nil;
end;

function TVXMP3File.LengthInBytes: Integer;
begin
  Result := 0;
end;

//------------------------------------------------------------------
initialization
//------------------------------------------------------------------

RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TVXMP3File);

end.
