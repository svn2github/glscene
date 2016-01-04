//
// This unit is part of the DGLengine Project, http://glscene.org
//
{ : GLSMOpenAL
  @HTML (
  <p>
  OpenAL based sound-manager (http://www.openal.org).<br>
  OpenAL drivers can be download from the OpenAL site or your soundcard
  manufacturer's website.</p>
  <p>
  Unsupported feature(s) :
  <ul>
    <li>Accepts only simple *uncompressed* WAV files (8/16 bits, mono/stereo) and Ogg Format</li>
    <li>Dynamic loading/unloading</li>
    <li>Global 3D parameters</li>
    <li>Environments</li>
    <li>CPUUsagePercent</li>
    <li>No system in place to limit number of sources playing simultaneously,
        can crash if too playing at once.</li>
  </ul></p>

  <p>
  <b>History: </b><font size=-1><ul>
  <li>01/01/16 - JD - Imported from GLScene
  </ul></font></p>
  )
}
unit DGLSMOpenAL;

interface

{$I DGLEngine.inc}

uses
  System.Classes, System.SysUtils,
  Vcl.Forms, Vcl.Dialogs,
  // GLS
  DGLScene, DGLSound, DGLSoundFileObjects;

type
  // ****************************************************************************************
  // TDGLSMOpenAL
  //
  TDGLSMOpenAL = class(TDGLSoundManager)
  private
    FActivated: Boolean;
  protected
    { Protected Declarations }
    function DoActivate: Boolean; override;
    procedure DoDeActivate; override;
    procedure NotifyMasterVolumeChange; override;
    procedure Notify3DFactorsChanged; override;
    procedure NotifyEnvironmentChanged; override;

    procedure KillSource(aSource: TDGLBaseSoundSource); override;
    procedure UpdateSource(aSource: TDGLBaseSoundSource); override;
    procedure MuteSource(aSource: TDGLBaseSoundSource; muted: Boolean); override;
    procedure PauseSource(aSource: TDGLBaseSoundSource; paused: Boolean); override;

    function GetDefaultFrequency(aSource: TDGLBaseSoundSource): Integer;

    function GetALFormat(sampling: TDGLSoundSampling): Integer;

  public
    { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateSources; override;
    function EAXSupported: Boolean; override;
  end;

  EOpenALError = Exception;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses
  DGLVectorMaths, DGLOpenAL {al, alut, alTypes};


// ------------------
{ Helpers Functions }
{$IFDEF GLS_REGION}{$REGION 'Helpers Functions'}{$ENDIF}

// checks for an error and raises an exception if necessary
procedure CheckOpenALError;
var
  error: Integer;
begin
  error := alGetError;
  if error <> AL_NO_ERROR then
    raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $' + IntToHex(error, 4) + ')');
end;

// clears the error-states
procedure ClearOpenALError;
begin
  alGetError;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}


// ------------------
{ TDGLSMOpenAL }
{$IFDEF GLS_REGION}{$REGION 'TDGLSMOpenAL'}{$ENDIF}

constructor TDGLSMOpenAL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDGLSMOpenAL.Destroy;
begin
  inherited Destroy;
end;

function TDGLSMOpenAL.DoActivate: Boolean;
var
  dummy: array of PALbyte;
begin
  Result := false;

  // Setup OpenAL
  if not InitOpenAL() then
    Exit;
  dummy := nil;
  alutInit(nil, dummy);
  CheckOpenALError;
  alDistanceModel(AL_INVERSE_DISTANCE);
  CheckOpenALError;
  ReadOpenALExtensions();

  // Set any global states
  FActivated := true;
  NotifyMasterVolumeChange;
  Notify3DFactorsChanged;
  if Environment <> seDefault then
    NotifyEnvironmentChanged;

  Result := true;
end;

procedure TDGLSMOpenAL.DoDeActivate;
var
  i: Integer;
begin
  FActivated := false;
  for i      := 0 to Sources.Count - 1 do
  begin
    Sources[i].Sample.ManagerTag := 0;
  end;
  alutExit;
end;

procedure TDGLSMOpenAL.NotifyMasterVolumeChange;
begin
  if FActivated then
  begin
    alListenerf(AL_GAIN, MasterVolume);
  end;
end;

procedure TDGLSMOpenAL.Notify3DFactorsChanged;
begin
  if FActivated then
  begin
    alDopplerFactor(DopplerFactor);
  end;
end;

procedure TDGLSMOpenAL.NotifyEnvironmentChanged;
begin
  if FActivated then
  begin
    // check extension is available + update
    if EAXSupported then
    begin
      // nothing yet
    end;
  end;
end;

procedure TDGLSMOpenAL.KillSource(aSource: TDGLBaseSoundSource);
var
  i, currentBufferTag, bufferCount: Integer;
begin
  if aSource.ManagerTag <> 0 then
  begin
    alSourceStop(aSource.ManagerTag);
    alDeleteSources(1, PALuint(@aSource.ManagerTag));
    aSource.ManagerTag := 0;

    // We can't just delete buffer, because other sources may be using it
    // so we count how many sources are using, then delete if it's the only one
    // using.
    // Same for ASource.Sample.ManagerTag, we set to zero once it's no longer
    // being used by any other sources

    currentBufferTag := aSource.Sample.ManagerTag;
    bufferCount      := 0;
    if currentBufferTag <> 0 then
    begin
      for i := 0 to Sources.Count - 1 do
      begin
        if Sources[i].Sample.ManagerTag = currentBufferTag then
        begin
          bufferCount := bufferCount + 1;
        end;
      end;
      if bufferCount = 1 then
      begin
        alDeleteBuffers(1, PALuint(@aSource.Sample.ManagerTag));
        aSource.Sample.ManagerTag := 0;
      end;
    end;
  end;
end;

procedure TDGLSMOpenAL.UpdateSource(aSource: TDGLBaseSoundSource);
var
  a: TALint;
begin
  // Clear any errors we may enter into procedure with
  ClearOpenALError;

  // Create an OpenAL source object if needed, and put ID into aSource.ManagerTag
  if aSource.ManagerTag = 0 then
  begin
    alGenSources(1, PALuint(@aSource.ManagerTag));
    CheckOpenALError;
  end
  else
  begin
    // Check to see if source has stopped, if so free it as limited number of sources allowed
    alGetSourcei(aSource.ManagerTag, AL_SOURCE_STATE, @a);
    CheckOpenALError;
    if a = AL_STOPPED then
    begin
      aSource.Free;
      Exit;
    end;
  end;

  // if sscTransformation in aSource.Changes then begin
  alSourcefv(aSource.ManagerTag, AL_POSITION, PALFloat(aSource.Origin.Position.asAddress));
  CheckOpenALError;
  alSourcefv(aSource.ManagerTag, AL_DIRECTION, PALFloat(aSource.Origin.Direction.asAddress));
  CheckOpenALError;
  // end;

  if aSource.SoundName <> '' then
  begin

    // If the sample doesn't have a reference to an OpenAL buffer
    // we need to create a buffer, and load the sample data into it
    if (aSource.Sample.ManagerTag = 0) and Assigned(aSource.Sample.Data) then
    begin
      alGenBuffers(1, PALuint(@aSource.Sample.ManagerTag));
      CheckOpenALError;
      // fill buffer (once buffer filled, can't fill buffer again, unless no other sources playing)
      alBufferData(aSource.Sample.ManagerTag, GetALFormat(aSource.Sample.sampling), aSource.Sample.Data.PCMData, aSource.Sample.Data.LengthInBytes, aSource.Sample.Data.sampling.Frequency);
      CheckOpenALError;

    end;

    if (sscSample in aSource.Changes) and Assigned(aSource.Sample.Data) then
    begin
      // Associate buffer with source, buffer may have either been recently
      // created, or already existing if being used by another source
      alSourcei(aSource.ManagerTag, AL_BUFFER, aSource.Sample.ManagerTag);
      CheckOpenALError;

      // If NbLoops>1 the source will constantly loop the sample, otherwise only play once
      alSourcei(aSource.ManagerTag, AL_LOOPING, Integer(aSource.NbLoops > 1));
      CheckOpenALError;

      // Start the source playing!
      alSourcePlay(aSource.ManagerTag);
      CheckOpenALError;
    end;
  end;

  if sscStatus in aSource.Changes then
  begin
    alSourcef(aSource.ManagerTag, AL_PITCH, 1.0);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_GAIN, 1.0);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_MAX_DISTANCE, aSource.MaxDistance);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_ROLLOFF_FACTOR, 1.0);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_REFERENCE_DISTANCE, aSource.MinDistance);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_CONE_INNER_ANGLE, aSource.InsideConeAngle);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_CONE_OUTER_ANGLE, aSource.OutsideConeAngle);
    CheckOpenALError;
    alSourcef(aSource.ManagerTag, AL_CONE_OUTER_GAIN, aSource.ConeOutsideVolume);
  end;
  inherited UpdateSource(aSource);
end;

procedure TDGLSMOpenAL.MuteSource(aSource: TDGLBaseSoundSource; muted: Boolean);
begin
  if muted then
    alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 0.0)
  else
    alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 1.0);
end;

procedure TDGLSMOpenAL.PauseSource(aSource: TDGLBaseSoundSource; paused: Boolean);
begin
  if not paused then
  begin
    alSourceRewind(aSource.ManagerTag);
    alSourcePlay(aSource.ManagerTag);
  end
  else
    alSourcePause(aSource.ManagerTag);
end;

procedure TDGLSMOpenAL.UpdateSources;
var
  pos, dir, up, vel: TVector;
  DirUp:             array [0 .. 5] of TALfloat; // orientation
begin
  ListenerCoordinates(pos, vel, dir, up);
  alListenerfv(AL_POSITION, PALFloat(@pos));
  alListenerfv(AL_VELOCITY, PALFloat(@vel));

  DirUp[0] := dir.V[0];
  DirUp[1] := dir.V[1];
  DirUp[2] := dir.V[2];
  DirUp[3] := up.V[0];
  DirUp[4] := up.V[1];
  DirUp[5] := up.V[2];
  alListenerfv(AL_ORIENTATION, PALFloat(@DirUp));

  inherited;
end;

function TDGLSMOpenAL.EAXSupported: Boolean;
begin
  Result := alIsExtensionPresent(PAnsiChar('EAX2.0'));
end;

function TDGLSMOpenAL.GetDefaultFrequency(aSource: TDGLBaseSoundSource): Integer;
begin
  Result := -1;
end;

function TDGLSMOpenAL.GetALFormat(sampling: TDGLSoundSampling): Integer;
begin
  Result := 0;

  // mono
  if sampling.NbChannels = 1 then
    case sampling.BitsPerSample of
      8:
        Result := AL_FORMAT_MONO8;
      16:
        Result := AL_FORMAT_MONO16;
    end
  else
    case sampling.BitsPerSample of // stereo
      8:
        Result := AL_FORMAT_STEREO8;
      16:
        Result := AL_FORMAT_STEREO16;
    end;
end;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

end.
