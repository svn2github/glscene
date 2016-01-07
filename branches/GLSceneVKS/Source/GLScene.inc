//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
//   

{$DEFINE VKSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE VKS_Graphics32_SUPPORT}

// Activate combined support for Delphi and C++Builder
{$DEFINE VKS_DELPHI_OR_CPPB}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE VKS_OPTIMIZATIONS}

// Enable all "Unsafe Type" / "Unsafe Code" compiler warnings
// May be useful when debugging.
{.$DEFINE VKS_ENABLE_UNSAFE_WARNINGS}

// De-Activate assembly routines in GLS.VectorGeometry.pas
{.$DEFINE GEOMETRY_NO_ASM}

// De-Activate assembly routines in other GLS units.
{.$DEFINE VKS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE VKS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE VKS_NO_REGIONS}

// Activate Logging
{.$DEFINE VKS_LOGGING}

// Activate OpenGL debug mode
{.$DEFINE VKS_OPENGL_DEBUG}

// Activate Multithread support
{.$DEFINE VKS_MULTITHREAD}

// Enable service rendering context, which automatically activate VKS_MULTITHREAD
{$DEFINE VKS_SERVICE_CONTEXT}

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
{.$DEFINE VKS_X11_SUPPORT}

// Activate OpenGL ES
{.$DEFINE VKS_OPENGL_ES}

//
// Platform Detection
//
{$IFDEF WIN64}
  {$DEFINE VKS_NO_ASM}
  {$DEFINE GEOMETRY_NO_ASM}
{$ENDIF}

{$IFDEF VKS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

// If it's not FPC, determine if C++ builder, or Delphi
// There no extended type on all non-x86 platforms.
{.$DEFINE VKS_PLATFORM_HAS_EXTENDED}
{$IFDEF VKS_DELPHI_OR_CPPB}
   {$DEFINE VKS_CPPB}
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
  {$UNDEF VKS_X11_SUPPORT}
{$ENDIF}

{$IFNDEF VKS_NO_INLINE}
  {$DEFINE VKS_INLINE}
{$ENDIF}

{$IFNDEF VKS_NO_REGIONS}
  {$DEFINE VKS_REGIONS}
{$ENDIF}

{$IFDEF VKS_SERVICE_CONTEXT}
  {$DEFINE VKS_MULTITHREAD}
{$ENDIF}

