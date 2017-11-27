//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//   
{
  The history is logged in a former GLS version of the unit.
}
{$DEFINE VXScene}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE USE_GRAPHICS32}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE USE_OPTIMIZATIONS}

// Activate assembly routines - Warning! Some components not working!!!
{.$DEFINE USE_ASM}

// Activate OpenVX debug mode
{.$DEFINE USE_DEBUG}

// Activate Logging
{.$DEFINE USE_LOGGING}


// Activate Multithread support
{.$DEFINE USE_MULTITHREAD}

// Enable service rendering context, which automatically activate USE_MULTITHREAD
// This work only on windows platform and with modern compiler (XE2 and FPC or higher}
{.$DEFINE USE_SERVICE_CONTEXT}

{$IFDEF USE_SERVICE_CONTEXT}
  // Activate Multithread support
  {$DEFINE USE_MULTITHREAD}
{$ENDIF}

// Activate support of Windows OS
{$DEFINE MSWINDOWS}

// Activate support of Mac OS
{.$DEFINE MACOS}

// Activate support of iOS
{.$DEFINE iOS}

// Activate support of Android
{.$DEFINE ANDROID}

// Activate support of Linux
{.$DEFINE LINUX}

// Activate support X Window System
{.$DEFINE X11_SUPPORT}


//
// Platform Detection
//

// by default, GLX is not supported on windows, although windows implementations
// do exist
{$IFDEF MSWINDOWS}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF VKS_X11_SUPPORT}
  {$DEFINE SUPPORT_WGL}
{$ENDIF}

{$IFDEF LINUX}
  {$UNDEF SUPPORT_WGL}
  {$UNDEF USE_ASM}
  {$DEFINE UNIX}
  {$DEFINE SUPPORT_GLX}
  {$DEFINE VKS_X11_SUPPORT}
{$ENDIF}


