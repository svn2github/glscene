//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Support classes for loading various fileformats. 
   These classes work together like vector file formats or Delphi's TGraphic classes. 

}
unit VXS.SoundFileObjects;

interface

{$I VXScene.inc}

uses
  System.Classes, System.SysUtils,
  {$IFDEF MSWINDOWS}Winapi.MMSystem,{$ENDIF}
  VXS.ApplicationFileIO, VXS.CrossPlatform;

type

	// TVXSoundSampling
	//
   { Defines a sound sampling quality. }
	TVXSoundSampling = class (TPersistent)
	   private
	      
         FOwner : TPersistent;
         FFrequency : Integer;
         FNbChannels : Integer;
         FBitsPerSample : Integer;

	   protected
	      
         function GetOwner : TPersistent; override;

	   public
	      
	      constructor Create(AOwner: TPersistent);
         destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         function BytesPerSec : Integer;
         function BytesPerSample : Integer;

        {$IFDEF MSWINDOWS}
         function WaveFormat : TWaveFormatEx;
        {$ENDIF}
	   published
	      
         { Sampling frequency in Hz (= samples per sec) }
         property Frequency : Integer read FFrequency write FFrequency default 22050;
         { Nb of sampling channels. 
            1 = mono, 2 = stereo, etc. }
         property NbChannels : Integer read FNbChannels write FNbChannels default 1;
         { Nb of bits per sample. 
            Common values are 8 and 16 bits. }
         property BitsPerSample : Integer read FBitsPerSample write FBitsPerSample default 8;
	end;

   // TVXSoundFile
   //
   { Abstract base class for different Sound file formats. 
      The actual implementation for these files (WAV, RAW...) must be done
      seperately. The concept for TVXSoundFile is very similar to TGraphic
      (see Delphi Help). 
      Default implementation for LoadFromFile/SaveToFile are to directly call the
      relevent stream-based methods, ie. you will just have to override the stream
      methods in most cases. }
   TVXSoundFile = class (TVXDataFile)
      private
         
         FSampling : TVXSoundSampling;

      protected
         
         procedure SetSampling(const val : TVXSoundSampling);

      public
         
	      constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure PlayOnWaveOut; virtual;

         { Returns a pointer to the sample data viewed as an in-memory WAV File. }
	      function WAVData : Pointer; virtual; abstract;
         { Returns the size (in bytes) of the WAVData. }
         function WAVDataSize : Integer; virtual; abstract;
         { Returns a pointer to the sample data viewed as an in-memory PCM buffer. }
	      function PCMData : Pointer; virtual; abstract;
         { Length of PCM data, in bytes. }
	      function LengthInBytes : Integer; virtual; abstract;
         { Nb of intensity samples in the sample. }
	      function LengthInSamples : Integer;
         { Length of play of the sample at nominal speed in seconds. }
	      function LengthInSec : Single;

         property Sampling : TVXSoundSampling read FSampling write SetSampling;
   end;

   TVXSoundFileClass = class of TVXSoundFile;

   // TVXSoundFileFormat
   //
   TVXSoundFileFormat = record
      SoundFileClass : TVXSoundFileClass;
      Extension      : String;
      Description    : String;
      DescResID      : Integer;
   end;
   PSoundFileFormat = ^TVXSoundFileFormat;

   // TVXSoundFileFormatsList
   //
   TVXSoundFileFormatsList = class(TList)
      public
         
         destructor Destroy; override;
         procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TVXSoundFileClass);
         function FindExt(Ext: string): TVXSoundFileClass;
         procedure Remove(AClass: TVXSoundFileClass);
         procedure BuildFilterStrings(SoundFileClass: TVXSoundFileClass; out Descriptions, Filters: string);
   end;

function GetGLSoundFileFormats : TVXSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TVXSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TVXSoundFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vSoundFileFormats : TVXSoundFileFormatsList;

// GeTVXSoundFileFormats
//
function GetGLSoundFileFormats : TVXSoundFileFormatsList;
begin
   if not Assigned(vSoundFileFormats)then
      vSoundFileFormats := TVXSoundFileFormatsList.Create;
   Result := vSoundFileFormats;
end;

// RegisterSoundFileFormat
//
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TVXSoundFileClass);
begin
   RegisterClass(AClass);
	GetGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

// UnregisterSoundFileClass
//
procedure UnregisterSoundFileClass(AClass: TVXSoundFileClass);
begin
	if Assigned(vSoundFileFormats) then
		vSoundFileFormats.Remove(AClass);
end;

// ------------------
// ------------------ TVXSoundSampling ------------------
// ------------------

// Create
//
constructor TVXSoundSampling.Create(AOwner: TPersistent);
begin
	inherited Create;
   FOwner:=AOwner;
   FFrequency:=22050;
   FNbChannels:=1;
   FBitsPerSample:=8;
end;

// Destroy
//
destructor TVXSoundSampling.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TVXSoundSampling.Assign(Source: TPersistent);
begin
   if Source is TVXSoundSampling then begin
      FFrequency:=TVXSoundSampling(Source).Frequency;
      FNbChannels:=TVXSoundSampling(Source).NbChannels;
      FBitsPerSample:=TVXSoundSampling(Source).BitsPerSample;
   end else inherited;
end;

// GetOwner
//
function TVXSoundSampling.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// BytesPerSec
//
function TVXSoundSampling.BytesPerSec : Integer;
begin
   Result:=(FFrequency*FBitsPerSample*FNbChannels) shr 3;
end;

// BytesPerSample
//
function TVXSoundSampling.BytesPerSample : Integer;
begin
   Result:=FBitsPerSample shr 3;
end;

{$IFDEF MSWINDOWS}
// WaveFormat
//
function TVXSoundSampling.WaveFormat : TWaveFormatEx;
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
// ------------------ TVXSoundFile ------------------
// ------------------

// Create
//
constructor TVXSoundFile.Create(AOwner: TPersistent);
begin
   inherited;
   FSampling:=TVXSoundSampling.Create(Self);
end;

// Destroy
//
destructor TVXSoundFile.Destroy;
begin
   FSampling.Free;
   inherited;
end;

// SetSampling
//
procedure TVXSoundFile.SetSampling(const val : TVXSoundSampling);
begin
   FSampling.Assign(val);
end;

// PlayOnWaveOut
//
procedure TVXSoundFile.PlayOnWaveOut;
begin
//   GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

// LengthInSamples
//
function TVXSoundFile.LengthInSamples : Integer;
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
function TVXSoundFile.LengthInSec : Single;
begin
	Result:=LengthInBytes/Sampling.BytesPerSec;
end;

// ------------------
// ------------------ TVXSoundFileFormatsList ------------------
// ------------------

// Destroy
//
destructor TVXSoundFileFormatsList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do Dispose(PSoundFileFormat(Items[i]));
   inherited;
end;

// Add
//
procedure TVXSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TVXSoundFileClass);
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
function TVXSoundFileFormatsList.FindExt(Ext: string): TVXSoundFileClass;
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
procedure TVXSoundFileFormatsList.Remove(AClass: TVXSoundFileClass);
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
procedure TVXSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TVXSoundFileClass;
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

