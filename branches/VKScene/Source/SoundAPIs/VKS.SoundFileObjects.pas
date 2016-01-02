//
// This unit is part of the GLScene Project   
//
{: VKS.SoundFileFormat<p>

	Support classes for loading various fileformats.<p>
   These classes work together like vector file formats or Delphi's TGraphic classes.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>17/11/09 - DaStr - Improved Unix compatibility
                             (thanks Predator) (BugtrackerID = 2893580)
      <li>13/07/09 - DanB - replaced sAllFilter with glsAllFilter (for FPC)
      <li>30/05/09 - DanB - TVKSoundSampling.WaveFormat now returns correct nBlockAlign, cbSize.
      <li>16/10/08 - UweR - Compatibility fix for Delphi 2009
      <li>07/06/07 - DaStr - Added $I GLScene.inc
      <li>26/01/05 - JAJ - Removed leak formed by never freeing vSoundFileFormats.
                            Reported by Dikoe Kenguru.
      <li>16/03/01 - Egg - TVKWAVFile.Capabilities
      <li>16/07/00 - Egg - Made use of new TDataFile class
      <li>09/06/00 - Egg - Added WAVDataSize
      <li>04/06/00 - Egg - Creation
	</ul></font>
}
unit VKS.SoundFileObjects;

interface

{$I VKScene.inc}

uses
  System.Classes,{$IFDEF MSWINDOWS}Winapi.MMSystem,{$ENDIF}
  VKS.ApplicationFileIO, VKS.CrossPlatform;

type

	// TVKSoundSampling
	//
   {: Defines a sound sampling quality. }
	TVKSoundSampling = class (TPersistent)
	   private
	      { Private Declarations }
         FOwner : TPersistent;
         FFrequency : Integer;
         FNbChannels : Integer;
         FBitsPerSample : Integer;

	   protected
	      { Protected Declarations }
         function GetOwner : TPersistent; override;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TPersistent);
         destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function BytesPerSec : Integer;
         function BytesPerSample : Integer;

        {$IFDEF MSWINDOWS}
         function WaveFormat : TWaveFormatEx;
        {$ENDIF}
	   published
	      { Published Declarations }
         {: Sampling frequency in Hz (= samples per sec) }
         property Frequency : Integer read FFrequency write FFrequency default 22050;
         {: Nb of sampling channels.<p>
            1 = mono, 2 = stereo, etc. }
         property NbChannels : Integer read FNbChannels write FNbChannels default 1;
         {: Nb of bits per sample.<p>
            Common values are 8 and 16 bits. }
         property BitsPerSample : Integer read FBitsPerSample write FBitsPerSample default 8;
	end;

   // TVKSoundFile
   //
   {: Abstract base class for different Sound file formats.<p>
      The actual implementation for these files (WAV, RAW...) must be done
      seperately. The concept for TVKSoundFile is very similar to TGraphic
      (see Delphi Help).<p>
      Default implementation for LoadFromFile/SaveToFile are to directly call the
      relevent stream-based methods, ie. you will just have to override the stream
      methods in most cases. }
   TVKSoundFile = class (TDataFile)
      private
         { Private Declarations }
         FSampling : TVKSoundSampling;

      protected
         { Protected Declarations }
         procedure SetSampling(const val : TVKSoundSampling);

      public
         { Public Declarations }
	      constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure PlayOnWaveOut; dynamic;

         {: Returns a pointer to the sample data viewed as an in-memory WAV File. }
	      function WAVData : Pointer; virtual; abstract;
         {: Returns the size (in bytes) of the WAVData. }
         function WAVDataSize : Integer; virtual; abstract;
         {: Returns a pointer to the sample data viewed as an in-memory PCM buffer. }
	      function PCMData : Pointer; virtual; abstract;
         {: Length of PCM data, in bytes. }
	      function LengthInBytes : Integer; virtual; abstract;
         {: Nb of intensity samples in the sample. }
	      function LengthInSamples : Integer;
         {: Length of play of the sample at nominal speed in seconds. }
	      function LengthInSec : Single;

         property Sampling : TVKSoundSampling read FSampling write SetSampling;
   end;

   TVKSoundFileClass = class of TVKSoundFile;

   // TVKSoundFileFormat
   //
   TVKSoundFileFormat = record
      SoundFileClass : TVKSoundFileClass;
      Extension      : String;
      Description    : String;
      DescResID      : Integer;
   end;
   PSoundFileFormat = ^TVKSoundFileFormat;

   // TVKSoundFileFormatsList
   //
   TVKSoundFileFormatsList = class(TList)
      public
         { Public Declarations }
         destructor Destroy; override;
         procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TVKSoundFileClass);
         function FindExt(Ext: string): TVKSoundFileClass;
         procedure Remove(AClass: TVKSoundFileClass);
         procedure BuildFilterStrings(SoundFileClass: TVKSoundFileClass; out Descriptions, Filters: string);
   end;

function GetGLSoundFileFormats : TVKSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TVKSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TVKSoundFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

var
   vSoundFileFormats : TVKSoundFileFormatsList;

// GeTVKSoundFileFormats
//
function GetGLSoundFileFormats : TVKSoundFileFormatsList;
begin
   if not Assigned(vSoundFileFormats)then
      vSoundFileFormats := TVKSoundFileFormatsList.Create;
   Result := vSoundFileFormats;
end;

// RegisterSoundFileFormat
//
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TVKSoundFileClass);
begin
   RegisterClass(AClass);
	GetGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterSoundFileClass
//
procedure UnregisterSoundFileClass(AClass: TVKSoundFileClass);
begin
	if Assigned(vSoundFileFormats) then
		vSoundFileFormats.Remove(AClass);
end;

// ------------------
// ------------------ TVKSoundSampling ------------------
// ------------------

// Create
//
constructor TVKSoundSampling.Create(AOwner: TPersistent);
begin
	inherited Create;
   FOwner:=AOwner;
   FFrequency:=22050;
   FNbChannels:=1;
   FBitsPerSample:=8;
end;

// Destroy
//
destructor TVKSoundSampling.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TVKSoundSampling.Assign(Source: TPersistent);
begin
   if Source is TVKSoundSampling then begin
      FFrequency:=TVKSoundSampling(Source).Frequency;
      FNbChannels:=TVKSoundSampling(Source).NbChannels;
      FBitsPerSample:=TVKSoundSampling(Source).BitsPerSample;
   end else inherited;
end;

// GetOwner
//
function TVKSoundSampling.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// BytesPerSec
//
function TVKSoundSampling.BytesPerSec : Integer;
begin
   Result:=(FFrequency*FBitsPerSample*FNbChannels) shr 3;
end;

// BytesPerSample
//
function TVKSoundSampling.BytesPerSample : Integer;
begin
   Result:=FBitsPerSample shr 3;
end;

{$IFDEF MSWINDOWS}
// WaveFormat
//
function TVKSoundSampling.WaveFormat : TWaveFormatEx;
begin
   Result.nSamplesPerSec:=Frequency;
   Result.nChannels:=NbChannels;
   Result.wFormatTag:=Wave_Format_PCM;
   Result.nAvgBytesPerSec:=BytesPerSec;
   Result.wBitsPerSample:=BitsPerSample;
   Result.nBlockAlign:=NbChannels*BytesPerSample;
   Result.cbSize:=0;
end;
{$ENDIF}

// ------------------
// ------------------ TVKSoundFile ------------------
// ------------------

// Create
//
constructor TVKSoundFile.Create(AOwner: TPersistent);
begin
   inherited;
   FSampling:=TVKSoundSampling.Create(Self);
end;

// Destroy
//
destructor TVKSoundFile.Destroy;
begin
   FSampling.Free;
   inherited;
end;

// SetSampling
//
procedure TVKSoundFile.SetSampling(const val : TVKSoundSampling);
begin
   FSampling.Assign(val);
end;

// PlayOnWaveOut
//
procedure TVKSoundFile.PlayOnWaveOut;
begin
//   GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

// LengthInSamples
//
function TVKSoundFile.LengthInSamples : Integer;
var
   d : Integer;
begin
   d:=Sampling.BytesPerSample*Sampling.NbChannels;
   if d>0 then
   	Result:=LengthInBytes div d
   else Result:=0;
end;

// LengthInSec
//
function TVKSoundFile.LengthInSec : Single;
begin
	Result:=LengthInBytes/Sampling.BytesPerSec;
end;

// ------------------
// ------------------ TVKSoundFileFormatsList ------------------
// ------------------

// Destroy
//
destructor TVKSoundFileFormatsList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do Dispose(PSoundFileFormat(Items[i]));
   inherited;
end;

// Add
//
procedure TVKSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TVKSoundFileClass);
var
   newRec: PSoundFileFormat;
begin
   New(newRec);
   with newRec^ do begin
      Extension := AnsiLowerCase(Ext);
      SoundFileClass := AClass;
      Description := Desc;
      DescResID := DescID;
   end;
   inherited Add(NewRec);
end;

// FindExt
//
function TVKSoundFileFormatsList.FindExt(Ext: string): TVKSoundFileClass;
var
   i : Integer;
begin
   Ext := AnsiLowerCase(Ext);
   for I := Count-1 downto 0 do with PSoundFileFormat(Items[I])^ do
      if (Extension = Ext) or ('.'+Extension = Ext) then begin
         Result := SoundFileClass;
         Exit;
      end;
   Result := nil;
end;

// Remove
//
procedure TVKSoundFileFormatsList.Remove(AClass: TVKSoundFileClass);
var
   i : Integer;
   p : PSoundFileFormat;
begin
   for I := Count-1 downto 0 do begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(AClass) then begin
         Dispose(P);
         Delete(I);
      end;
   end;
end;

// BuildFilterStrings
//
procedure TVKSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TVKSoundFileClass;
                                                    out Descriptions, Filters: string);
var
   c, i : Integer;
   p    : PSoundFileFormat;
begin
   Descriptions := '';
   Filters := '';
   C := 0;
   for I := Count-1 downto 0 do begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(SoundFileClass) and (P^.Extension <> '') then
         with P^ do begin
            if C <> 0 then begin
               Descriptions := Descriptions+'|';
               Filters := Filters+';';
            end;
            if (Description = '') and (DescResID <> 0) then
               Description := LoadStr(DescResID);
            FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s',
                   [Descriptions, Description, Extension]);
            FmtStr(Filters, '%s*.%s', [Filters, Extension]);
            Inc(C);
         end;
   end;
   if C > 1 then
      FmtStr(Descriptions, '%s (%s)|%1:s|%s', [glsAllFilter, Filters, Descriptions]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

  FreeAndNil(vSoundFileFormats);

end.

