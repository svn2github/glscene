//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLS_ODE_BCB5.res");
USERES("..\Source\PhysicsAPIs\GLSceneVCLODE.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEPACKAGE("GLSceneBCB5.bpi");
USEUNIT("..\Source\PhysicsAPIs\dynode.pas");
USEUNIT("..\Source\PhysicsAPIs\dynodegl.pas");
USEUNIT("..\Source\PhysicsAPIs\moduleloader.pas");
USEUNIT("..\Source\GLODESkeletonColliders.pas");
USEUNIT("..\Source\GLODECustomColliders.pas");
USEUNIT("..\Source\GLODEManager.pas");
USEUNIT("..\Source\DesignTime\GLODERegister.pas");
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
