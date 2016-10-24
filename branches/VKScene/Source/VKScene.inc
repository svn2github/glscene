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

// Activate assembly routines
{.$DEFINE VKS_ASM}

// Activate inline directive
{.$DEFINE VKS_INLINE}

// De-Activate regions
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
  {$UNDEF VKS_ASM}
  {$DEFINE UNIX}
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

{$IFDEF VKS_ASM}
  {$IFDEF VKS_INLINE}
    {$DEFINE VKS_INLINE_VICE_ASM}
  {$ENDIF}
{$ENDIF}

{$IFNDEF VKS_NO_REGIONS}
  {$DEFINE VKS_REGIONS}
{$ENDIF}

{$DEFINE VKS_DEPRECATED}
{
  This defines if the compiler should use deprecated stuff or not.
  by default headers use deprecated stuff.
}

