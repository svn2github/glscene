//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLSS_FMOD_BCB5.res");
USERES("..\Source\SoundAPIs\GLSceneVCLFMod.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("GLSceneBCB5.bpi");
USEUNIT("..\Source\SoundAPIs\fmod.pas");
USEUNIT("..\Source\GLSMFMOD.pas");
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
