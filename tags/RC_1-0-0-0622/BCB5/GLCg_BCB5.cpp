//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLCg_BCB5.res");
USERES("..\Source\CgShaders\GLSceneVCLCg.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("GLSceneBCB5.bpi");
USELIB("..\Source\CgShaders\cg.lib");
USELIB("..\Source\CgShaders\cgGL.lib");
USEUNIT("..\Source\CgShaders\cg.pas");
USEUNIT("..\Source\CgShaders\cgGL.pas");
USEUNIT("..\Source\CgShaders\GLCgShader.pas");
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
