//
// VKScene project, http://glscene.org
//   

{$DEFINE VKSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE VKS_Graphics32_SUPPORT}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE VKS_OPTIMIZATIONS}

// De-Activate assembly routines in VKS.VectorGeometry.pas
{.$DEFINE GEOMETRY_NO_ASM}

// De-Activate assembly routines in other GLS units.
{.$DEFINE VKS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE VKS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE VKS_NO_REGIONS}

// Activate Logging
{.$DEFINE VKS_LOGGING}

// Activate Vulkan debug mode
{.$DEFINE VKS_DEBUG}

// Activate Multithread support
{.$DEFINE VKS_MULTITHREAD}

// Enable service rendering context, which automatically activate VKS_MULTITHREAD
// This work only on windows platform and with modern compiler (XE2 and FPC or higher}
{$DEFINE VKS_SERVICE_CONTEXT}

{$IFDEF VKS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

{$IFDEF VKS_SERVICE_CONTEXT}
  // Activate Multithread support
  {$DEFINE VKS_MULTITHREAD}
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
{.$DEFINE VKS_X11_SUPPORT}

// Activate Vulkan ES
{.$DEFINE VKS_ES}

//
// Platform Detection
//
{$IFDEF WIN64}
  {$DEFINE VKS_NO_ASM}
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
  {$UNDEF VKS_X11_SUPPORT}
{$ENDIF}

{$IFDEF LINUX}
  {$UNDEF SUPPORT_WGL}
  {$DEFINE SUPPORT_GLX}
  {$DEFINE VKS_X11_SUPPORT}
{$ENDIF}

{$IFDEF DARWIN}
  {$UNDEF SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
  {$UNDEF VKS_X11_SUPPORT}
{$ENDIF}

{$UNDEF VKS_UNSUPPORTED}

{$IFDEF VKS_UNSUPPORTED}
  Error: unsupported version, requires at least RAD XE2
{$ENDIF}

{$IFNDEF VKS_NO_INLINE}
  {$DEFINE VKS_INLINE}
{$ENDIF}

{$IFNDEF VKS_NO_REGIONS}
  {$DEFINE VKS_REGIONS}
{$ENDIF}

{$DEFINE VKS_DEPRECATED}
{
  This defines if the compiler should use deprecated stuff or not.
  by default headers use deprecated stuff.
}

