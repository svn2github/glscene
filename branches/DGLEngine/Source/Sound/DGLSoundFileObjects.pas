//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : DGLSoundFileObjects<p>

  @HTML (
  <p>
  Support classes for loading various Sound fileformats.<br>
  These classes work together like vector file formats or Delphi's TGraphic classes. </p>
  <b>History: </b><font size=-1><ul>
  <li>01/01/16 - JD - Imported from GLScene
  </ul></font></p>
  )
}
unit DGLSoundFileObjects;

interface

{$I DGLEngine.inc}

uses
  System.Classes, {$IFDEF MSWINDOWS}MMSystem, {$ENDIF}
  // GLS
  DGLApplicationFileIO, DGLCrossPlatform;

type
  // ****************************************************************************************
  // TDGLSoundSampling
  //
  { : Defines a sound sampling quality. }
  TDGLSoundSampling = class(TPersistent)
  private
    { Private Declarations }
    FOwner:         TPersistent;
    FFrequency:     Integer;
    FNbChannels:    Integer;
    FBitsPerSample: Integer;

  protected
    { Protected Declarations }
    function GetOwner: TPersistent; override;

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function BytesPerSec: Integer;
    function BytesPerSample: Integer;

    {$IFDEF MSWINDOWS}
    function WaveFormat: TWaveFormatEx;
    {$ENDIF}
  published
    { Published Declarations }
    { : Sampling frequency in Hz (= samples per sec) }
    property Frequency: Integer read FFrequency write FFrequency default 22050;
    { : Nb of sampling channels.<p>
      1 = mono, 2 = stereo, etc. }
    property NbChannels: Integer read FNbChannels write FNbChannels default 1;
    { : Nb of bits per sample.<p>
      Common values are 8 and 16 bits. }
    property BitsPerSample: Integer read FBitsPerSample write FBitsPerSample default 8;
  end;

  // ****************************************************************************************
  // TDGLSoundFile
  //
  { : Abstract base class for different Sound file formats.<p>
    The actual implementation for these files (WAV, RAW...) must be done
    seperately. The concept for TDGLSoundFile is very similar to TGraphic
    (see Delphi Help).<p>
    Default implementation for LoadFromFile/SaveToFile are to directly call the
    relevent stream-based methods, ie. you will just have to override the stream
    methods in most cases. }
  TDGLSoundFile = class(TDGLDataFile)
  private
    { Private Declarations }
    FSampling: TDGLSoundSampling;

  protected
    { Protected Declarations }
    procedure SetSampling(const val: TDGLSoundSampling);

  public
    { Public Declarations }
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure PlayOnWaveOut; dynamic;

    { : Returns a pointer to the sample data viewed as an in-memory WAV File. }
    function WAVData: Pointer; virtual; abstract;
    { : Returns the size (in bytes) of the WAVData. }
    function WAVDataSize: Integer; virtual; abstract;
    { : Returns a pointer to the sample data viewed as an in-memory PCM buffer. }
    function PCMData: Pointer; virtual; abstract;
    { : Length of PCM data, in bytes. }
    function LengthInBytes: Integer; virtual; abstract;
    { : Nb of intensity samples in the sample. }
    function LengthInSamples: Integer;
    { : Length of play of the sample at nominal speed in seconds. }
    function LengthInSec: Single;

    property Sampling: TDGLSoundSampling read FSampling write SetSampling;
  end;

  TDGLSoundFileClass = class of TDGLSoundFile;

  // ****************************************************************************************
  // TDGLSoundFileFormat
  //
  TDGLSoundFileFormat = record
    SoundFileClass: TDGLSoundFileClass;
    Extension: String;
    Description: String;
    DescResID: Integer;
  end;

  PSoundFileFormat = ^TDGLSoundFileFormat;

  // ****************************************************************************************
  // TDGLSoundFileFormatsList
  //
  TDGLSoundFileFormatsList = class(TList)
  public
    { Public Declarations }
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TDGLSoundFileClass);
    function FindExt(Ext: string): TDGLSoundFileClass;
    procedure Remove(AClass: TDGLSoundFileClass);
    procedure BuildFilterStrings(SoundFileClass: TDGLSoundFileClass; out Descriptions, Filters: string);
  end;

// ****************************************************************************************

function GeTDGLSoundFileFormats: TDGLSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TDGLSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TDGLSoundFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

var
  vSoundFileFormats: TDGLSoundFileFormatsList;

// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

function GeTDGLSoundFileFormats: TDGLSoundFileFormatsList;
begin
  if not Assigned(vSoundFileFormats) then
    vSoundFileFormats := TDGLSoundFileFormatsList.Create;
  Result              := vSoundFileFormats;
end;

procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TDGLSoundFileClass);
begin
  RegisterClass(AClass);
  GeTDGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterSoundFileClass(AClass: TDGLSoundFileClass);
begin
  if Assigned(vSoundFileFormats) then
    vSoundFileFormats.Remove(AClass);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundSampling }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundSampling'}{$ENDIF}

constructor TDGLSoundSampling.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner         := AOwner;
  FFrequency     := 22050;
  FNbChannels    := 1;
  FBitsPerSample := 8;
end;

destructor TDGLSoundSampling.Destroy;
begin
  inherited Destroy;
end;

procedure TDGLSoundSampling.Assign(Source: TPersistent);
begin
  if Source is TDGLSoundSampling then
  begin
    FFrequency     := TDGLSoundSampling(Source).Frequency;
    FNbChannels    := TDGLSoundSampling(Source).NbChannels;
    FBitsPerSample := TDGLSoundSampling(Source).BitsPerSample;
  end
  else
    inherited;
end;

function TDGLSoundSampling.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TDGLSoundSampling.BytesPerSec: Integer;
begin
  Result := (FFrequency * FBitsPerSample * FNbChannels) shr 3;
end;

function TDGLSoundSampling.BytesPerSample: Integer;
begin
  Result := FBitsPerSample shr 3;
end;

{$IFDEF MSWINDOWS}

function TDGLSoundSampling.WaveFormat: TWaveFormatEx;
begin
  Result.nSamplesPerSec  := Frequency;
  Result.nChannels       := NbChannels;
  Result.wFormatTag      := Wave_Format_PCM;
  Result.nAvgBytesPerSec := BytesPerSec;
  Result.wBitsPerSample  := BitsPerSample;
  Result.nBlockAlign     := NbChannels * BytesPerSample;
  Result.cbSize          := 0;
end;
{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundFile }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundFile'}{$ENDIF}

constructor TDGLSoundFile.Create(AOwner: TPersistent);
begin
  inherited;
  FSampling := TDGLSoundSampling.Create(Self);
end;

destructor TDGLSoundFile.Destroy;
begin
  FSampling.Free;
  inherited;
end;

procedure TDGLSoundFile.SetSampling(const val: TDGLSoundSampling);
begin
  FSampling.Assign(val);
end;

procedure TDGLSoundFile.PlayOnWaveOut;
begin
  // GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

function TDGLSoundFile.LengthInSamples: Integer;
var
  d: Integer;
begin
  d := Sampling.BytesPerSample * Sampling.NbChannels;
  if d > 0 then
    Result := LengthInBytes div d
  else
    Result := 0;
end;

function TDGLSoundFile.LengthInSec: Single;
begin
  Result := LengthInBytes / Sampling.BytesPerSec;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------
{ TDGLSoundFileFormatList }
{$IFDEF GLS_REGION}{$REGION 'TDGLSoundFileFormatList'}{$ENDIF}

destructor TDGLSoundFileFormatsList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PSoundFileFormat(Items[i]));
  inherited;
end;

procedure TDGLSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer; AClass: TDGLSoundFileClass);
var
  newRec: PSoundFileFormat;
begin
  New(newRec);
  with newRec^ do
  begin
    Extension      := AnsiLowerCase(Ext);
    SoundFileClass := AClass;
    Description    := Desc;
    DescResID      := DescID;
  end;
  inherited Add(newRec);
end;

function TDGLSoundFileFormatsList.FindExt(Ext: string): TDGLSoundFileClass;
var
  i: Integer;
begin
  Ext   := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with PSoundFileFormat(Items[i])^ do
      if (Extension = Ext) or ('.' + Extension = Ext) then
      begin
        Result := SoundFileClass;
        Exit;
      end;
  Result := nil;
end;

procedure TDGLSoundFileFormatsList.Remove(AClass: TDGLSoundFileClass);
var
  i: Integer;
  p: PSoundFileFormat;
begin
  for i := Count - 1 downto 0 do
  begin
    p := PSoundFileFormat(Items[i]);
    if p^.SoundFileClass.InheritsFrom(AClass) then
    begin
      Dispose(p);
      Delete(i);
    end;
  end;
end;

procedure TDGLSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TDGLSoundFileClass; out Descriptions, Filters: string);
var
  c, i: Integer;
  p:    PSoundFileFormat;
begin
  Descriptions := '';
  Filters      := '';
  c            := 0;
  for i        := Count - 1 downto 0 do
  begin
    p := PSoundFileFormat(Items[i]);
    if p^.SoundFileClass.InheritsFrom(SoundFileClass) and (p^.Extension <> '') then
      with p^ do
      begin
        if c <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters      := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(c);
      end;
  end;
  if c > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [glsAllFilter, Filters, Descriptions]);
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

initialization



finalization

FreeAndNil(vSoundFileFormats);

end.
