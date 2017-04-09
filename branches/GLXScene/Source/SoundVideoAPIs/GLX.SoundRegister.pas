//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Sounds

  History:
    01/12/15 - PW - Creation.
}
unit GLX.SoundRegister;

interface

uses
  System.Classes,
  GLX.SMBASS,
  GLX.SMFMOD,
  GLX.SMOpenAL,
  GLX.WaveOut;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene',[TGLSMBASS,TGLSMFMOD,TGLSMOpenAL,TGLSMWaveOut]);
end;

end.
