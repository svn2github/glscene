//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLSS_BASS_BCB5.res");
USERES("..\Source\SoundAPIs\GLSceneVCLBass.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("GLSceneBCB5.bpi");
USEUNIT("..\Source\SoundAPIs\Bass.pas");
USEUNIT("..\Source\GLSMBASS.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
