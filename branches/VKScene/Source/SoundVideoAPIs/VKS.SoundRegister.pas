//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Sounds

  History:
    01/12/15 - PW - Creation.
}
unit VKS.SoundRegister;

interface

uses
  System.Classes,
  VKS.SMBASS,
  VKS.SMFMOD,
  VKS.SMOpenAL,
  VKS.WaveOut;

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
  RegisterComponents('GLScene',[TVKSMBASS,TVKSMFMOD,TVKSMOpenAL,TVKSMWaveOut]);
end;

end.
