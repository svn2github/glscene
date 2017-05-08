//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
  Design time registration code for the Sounds

  History:
    01/12/15 - PW - Creation.
}
unit VXS.SoundRegister;

interface

uses
  System.Classes,
  VXS.SMBASS,
  VXS.SMFMOD,
  VXS.SMOpenAL,
  VXS.WaveOut;

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
  RegisterComponents('VXScene',[TVXSMBASS,TVXSMFMOD,TVXSMOpenAL,TVXSMWaveOut]);
end;

end.
