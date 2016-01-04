//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
// History:
// 01/01/16 - JD - Imported and updated From GLScene
//

{$DEFINE DGLENGINE}

//
// Additionnal support & dependencies
//

// Activate support for png images (using pngimage.pas library)
// GLS_PngImage_SUPPORT is automatically defined if Delphi>=2009 (see below)
// Also, Define "Auto" Resize To Nearest 8 Multiple Size
// PNG is supported by Delphi since Dephi 2009
{$DEFINE GLS_PngImage_SUPPORT}
{$DEFINE GLS_PngImage_RESIZENEAREST}


// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE GLS_OPTIMIZATIONS}

// Enable all "Unsafe Type" / "Unsafe Code" compiler warnings
// May be useful when debugging.
{$DEFINE GLS_ENABLE_UNSAFE_WARNINGS}

// Activate Logging
{$DEFINE GLS_LOGGING}

// Activate OpenGL debug mode
{.$DEFINE GLS_OPENGL_DEBUG}

// De-Activate assembly routines in  GLS units.
{.$DEFINE GLS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE GLS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE GLS_NO_REGIONS}

// Activate support X Window System
{.$DEFINE GLS_X11_SUPPORT}

// Activate OpenGL ES
{.$DEFINE GLS_OPENGL_ES}


//
// Platform Detection
//
{$IFDEF WIN64}
  {$DEFINE GLS_NO_ASM}
  
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    // MSWINDOWS wasn't defined in Delphi 5, but WIN32 was
    // so need to define MSWINDOWS here to provide Delphi 5 support
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}


 {$DEFINE MULTITHREADOPENGL}


{$IFDEF LINUX}
  {$DEFINE GLS_NO_ASM}
  {$DEFINE UNIX}
{$ENDIF}

// .NET
{$IFDEF CLR}
  {$DEFINE GLS_NO_ASM}
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



