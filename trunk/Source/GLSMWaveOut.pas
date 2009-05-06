//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLSMWaveOut<p>

	Basic sound manager based on WinMM <p>

	<b>History : </b><font size=-1><ul>
	   <li>24/04/09 - DanB - Creation, split from GLSound.pas, to remove windows dependency
	</ul></font>
}
unit GLSMWaveOut;

interface

uses Classes, GLSound, MMSystem, GLSoundFileObjects;

type

	// TGLSMWaveOut
	//
   {: Basic sound manager based on WinMM <i>waveOut</i> function.<p>
      This manager has NO 3D miximing capacity, this is merely a default manager
      that should work on any windows based system, and help showcasing/testing
      basic GLSS core functionality.<p>
      Apart from 3D, mute, pause, priority and volume are ignored too, and only
      sampling conversions supported by the windows ACM driver are supported
      (ie. no 4bits samples playback etc.). }
	TGLSMWaveOut = class (TGLSoundManager)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

      published
	      { Published Declarations }
         property MaxChannels default 4;
	end;

procedure PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        sampling : TGLSoundSampling); overload;
function PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        waveFormat : TWaveFormatEx) : HWaveOut; overload;

implementation

uses SysUtils;

procedure _waveOutCallBack2(hwo : HWAVEOUT; uMsg : Cardinal;
                           dwInstance, dwParam1, dwParam2 : Integer); stdcall;
begin
   if uMsg=WOM_DONE then
      waveOutClose(hwo);
end;

// PlayOnWaveOut (waveformat)
//
function PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                       waveFormat : TWaveFormatEx) : HWaveOut;
var
   hwo : hwaveout;
   wh : wavehdr;
   mmres : MMRESULT;
begin
   mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @waveFormat, Cardinal(@_waveOutCallBack2),
                      0, CALLBACK_FUNCTION);
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   wh.dwBufferLength:=lengthInBytes;
   wh.lpData:=pcmData;
   wh.dwFlags:=0;
   wh.dwLoops:=1;
   wh.lpNext:=nil;
   mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
   Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
   Result:=hwo;
end;

// PlayOnWaveOut (sampling)
//
procedure PlayOnWaveOut(pcmData : Pointer; lengthInBytes : Integer;
                        sampling : TGLSoundSampling);
var
   wfx : TWaveFormatEx;
begin
   wfx:=sampling.WaveFormat;
   PlayOnWaveOut(pcmData, lengthInBytes, wfx);
end;

// ------------------
// ------------------ TGLSMWaveOut ------------------
// ------------------

// Create
//
constructor TGLSMWaveOut.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=4;
end;

// Destroy
//
destructor TGLSMWaveOut.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMWaveOut.DoActivate : Boolean;
begin
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMWaveOut.DoDeActivate;
var
   i : Integer;
begin
   for i:=Sources.Count-1 downto 0 do
      KillSource(Sources[i]);
end;

// KillSource
//
procedure TGLSMWaveOut.KillSource(aSource : TGLBaseSoundSource);
begin
   if aSource.ManagerTag<>0 then begin
      waveOutClose(aSource.ManagerTag);
      aSource.ManagerTag:=0;
   end;
end;

procedure _waveOutCallBack(hwo : HWAVEOUT; uMsg : Cardinal;
                           dwInstance, dwParam1, dwParam2 : Integer); stdcall;
begin
   if uMsg=WOM_DONE then begin
      waveOutClose(hwo);
      TGLSoundSource(dwInstance).ManagerTag:=-1;
   end;
end;

// UpdateSource
//
procedure TGLSMWaveOut.UpdateSource(aSource : TGLBaseSoundSource);
var
   i, n : Integer;
   wfx : TWaveFormatEx;
   smp : TGLSoundSample;
   wh : wavehdr;
   mmres : MMRESULT;
   hwo : hwaveout;
begin
   // count nb of playing sources and delete done ones
   n:=0;
   for i:=Sources.Count-1 downto 0 do
      if Sources[i].ManagerTag>0 then
         Inc(n)
      else if Sources[i].ManagerTag=-1 then
         Sources.Delete(i);
	// start sources if some capacity remains, and forget the others
   for i:=Sources.Count-1 downto 0 do if Sources[i].ManagerTag=0 then begin
      if n<MaxChannels then begin
         smp:=Sources[i].Sample;
         if Assigned(smp) and (smp.Data<>nil) then begin
            wfx:=smp.Data.Sampling.WaveFormat;
            mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @wfx,
                               Cardinal(@_waveOutCallBack), Integer(Sources[i]),
                               CALLBACK_FUNCTION);
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            wh.dwBufferLength:=smp.LengthInBytes;
            wh.lpData:=smp.Data.PCMData;
            wh.dwLoops:=Sources[i].NbLoops;
            if wh.dwLoops>1 then
               wh.dwFlags:=WHDR_BEGINLOOP+WHDR_ENDLOOP
            else wh.dwFlags:=0;
            wh.lpNext:=nil;
            mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            Sources[i].ManagerTag:=hwo;
            Inc(n);
			end else
				Sources.Delete(i);
		end else
			Sources.Delete(i);
	end;
end;

initialization

  RegisterClasses([TGLSMWaveOut]);

end.
