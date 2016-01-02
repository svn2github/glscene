//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
// History:
// 02/01/16 - PW - Imported from GLScene for VCL
//

{$DEFINE GLSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE GLS_Graphics32_SUPPORT}

// Activate combined support for Delphi and C++Builder
{$DEFINE GLS_DELPHI_OR_CPPB}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE GLS_OPTIMIZATIONS}

// Enable all "Unsafe Type" / "Unsafe Code" compiler warnings
// May be useful when debugging.
{.$DEFINE GLS_ENABLE_UNSAFE_WARNINGS}

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

// Activate Multithread support
{.$DEFINE GLS_MULTITHREAD}

// Enable service rendering context, which automatically activate GLS_MULTITHREAD
// This work only on windows platform and with modern compiler (D2009 and FPC}
{$DEFINE GLS_SERVICE_CONTEXT}

// Activate support of MSWindows
{.$DEFINE MSWINDOWS}

// Activate support of Mac OS
{.$DEFINE MACOS}

// Activate support of iOS
{.$DEFINE iOS}

// Activate support of Android
{.$DEFINE ANDROID}

// Activate support of Linux
{.$DEFINE MSWINDOWS}

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

{$IFDEF GLS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

// If it's not FPC, determine if C++ builder, or Delphi
// There no extended type on all non-x86 platforms.
{.$DEFINE GLS_PLATFORM_HAS_EXTENDED}
{$IFDEF GLS_DELPHI_OR_CPPB}
   {$DEFINE GLS_CPPB}
   // C++Builder requires hpp/obj files to use Delphi components in run-time packages.
   {$ObjExportAll On}
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

{$IFNDEF GLS_NO_INLINE}
  {$DEFINE GLS_INLINE}
{$ENDIF}

{$IFNDEF GLS_NO_REGIONS}
  {$DEFINE GLS_REGIONS}
{$ENDIF}

{$IFDEF GLS_SERVICE_CONTEXT}
  {$DEFINE GLS_MULTITHREAD}
{$ENDIF}

