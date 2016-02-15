//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
//   

{$DEFINE GLSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE GLS_Graphics32_SUPPORT}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE GLS_OPTIMIZATIONS}

// De-Activate assembly routines in GLS.VectorGeometry.pas
{.$DEFINE GEOMETRY_NO_ASM}

// De-Activate assembly routines in other GLS units.
{.$DEFINE GLS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE GLS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE GLS_NO_REGIONS}

// Activate Logging
{.$DEFINE GLS_LOGGING}

// Activate OpenGL debug mode
{.$DEFINE GLS_OPENGL_DEBUG}

// Enable service rendering context, which automatically activate GLS_MULTITHREAD
// This work only on windows platform and with modern compiler (XE2 and FPC or higher}
{$DEFINE GLS_SERVICE_CONTEXT}

// Activate Multithread support
{$DEFINE GLS_MULTITHREAD}

{$IFDEF GLS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

// Activate support of MSWindows
{.$DEFINE MSWINDOWS}

// Activate support of Mac OS
{.$DEFINE MACOS}

// Activate support of iOS
{.$DEFINE iOS}

// Activate support of Android
{.$DEFINE ANDROID}

// Activate support of Linux
{.$DEFINE LINUX}

// Activate support X Window System
{.$DEFINE GLS_X11_SUPPORT}

// Activate OpenGL ES
{.$DEFINE GLS_OPENGL_ES}

//
// Platform Detection
//
{$IFDEF WIN64}
  {$DEFINE GLS_NO_ASM}
  {$DEFINE GEOMETRY_NO_ASM}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE GEOMETRY_NO_ASM}
  {$DEFINE UNIX}
{$ENDIF}

// .NET
{$IFDEF CLR}
  {$DEFINE GEOMETRY_NO_ASM}
  {$UNSAFECODE ON}
{$ENDIF}

// by default, GLX is not supported on windows, although windows implementations
// do exist
{$IFDEF MSWINDOWS}
  {$DEFINE SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF GLS_X11_SUPPORT}
{$ENDIF}

{$IFDEF Linux}
  {$UNDEF SUPPORT_WGL}
  {$DEFINE SUPPORT_GLX}
  {$DEFINE GLS_X11_SUPPORT}
{$ENDIF}

{$IFDEF DARWIN}
  {$UNDEF SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF GLS_X11_SUPPORT}
{$ENDIF}

{$UNDEF GLS_UNSUPPORTED}

{$IFDEF GLS_UNSUPPORTED}
  Error: unsupported version, requires at least Delphi XE2
{$ENDIF}

{$IFNDEF GLS_NO_INLINE}
  {$DEFINE GLS_INLINE}
{$ENDIF}

{$IFNDEF GLS_NO_REGIONS}
  {$DEFINE GLS_REGIONS}
{$ENDIF}
