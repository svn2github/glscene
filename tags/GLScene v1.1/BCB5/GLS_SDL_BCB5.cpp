//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GLS_SDL_BCB5.res");
USERES("..\Source\GameAPIs\GLSceneVCLSDL.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("GLSceneBCB5.bpi");
USELIB("..\Source\GameAPIs\msvcrt.lib");
USELIB("..\Source\GameAPIs\sdl.lib");
USEUNIT("..\Source\GameAPIs\sdl.pas");
USEUNIT("..\Source\GameAPIs\SDLWindow.pas");
USEUNIT("..\Source\Platform\GLSDLContext.pas");
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
