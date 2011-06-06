// prevent pasdoc from parsing this file
{$IFNDEF PASDOC}

// compiler/IDE dependent config
{$IFDEF FPC}
  {$H+} // use AnsiString instead of ShortString as String-type (default in Delphi)

  // if -dDEBUG is specified on the command-line, FPC uses some default
  // compiler-flags specified in fpc.cfg -> use -dDEBUG_MODE instead
  {$IFDEF DEBUG_MODE}
    {$DEFINE DEBUG}
  {$ENDIF}

  {$DEFINE HasInline}
{$ELSE}
  {$DEFINE Delphi}

  // Delphi version numbers (ignore Delphi < 7 and Delphi 8 (VER160))

  {$IF Defined(VER180)} // Delphi 2006 (=10)
    {$DEFINE DELPHI_10}
    {$DEFINE DELPHI_7_UP}
    {$DEFINE DELPHI_9_UP}
    {$DEFINE DELPHI_10_UP}
  {$ELSEIF Defined(VER170)} // Delphi 2005 (=9)
    {$DEFINE DELPHI_9}
    {$DEFINE DELPHI_7_UP}
    {$DEFINE DELPHI_9_UP}    
  {$ELSEIF Defined(VER150)}
    {$DEFINE DELPHI_7}
    {$DEFINE DELPHI_7_UP}
  {$ELSE} // unsupported
    {$WARN ERROR 'Unsupported compiler version'}
  {$IFEND}

  // inline directive introduced with Delphi 2005
  {$IFDEF DELPHI_9_UP}
    {$DEFINE HasInline}
  {$ENDIF}
{$ENDIF}


// platform dependent config
{$IF Defined(MSWINDOWS)}
  // include defines but no constants
  {$I config-win.inc}

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$IFDEF DEBUG}
    // windows apps are either GUI- or console-apps. Console-apps will open
    // an additional console-window for output. For development only!
    {$DEFINE CONSOLE}
  {$ENDIF}
{$ELSEIF Defined(DARWIN)}
  // include defines but no constants
  {$I config-darwin.inc}

  // enable debug-mode. For development only!
  {.$DEFINE DEBUG}
  {$DEFINE CONSOLE}
  {.$DEFINE HaveBASS}
  {$DEFINE UTF8_FILENAMES}
{$ELSEIF Defined(UNIX)}
  // include defines but no constants
  {$IF Defined(FREEBSD)}
    {$I config-darwin.inc}
  {$ELSEIF Defined(Linux)}
    {$I config-darwin.inc}
  {$IFEND}

  // use "configure --enable-debug", "make debug" or
  // the command-line parameter "-debug" instead of defining DEBUG directly
  {.$DEFINE DEBUG}
  // linux apps are always console-apps so leave this defined.
  {$DEFINE CONSOLE}
{$IFEND}

// ffmpeg config
{$IFDEF HaveFFmpeg}
  {$DEFINE UseFFmpegDecoder}
  {$DEFINE UseFFmpegResample}
  {$DEFINE UseFFmpegVideo}
  {$IFDEF HaveSWScale}
    {$DEFINE UseSWScale}
  {$ENDIF}
{$ENDIF}

{$IFDEF HaveLibsamplerate}
  {$DEFINE UseSRCResample}
{$ENDIF}

// specify some useful defines

{$IF Defined(UseFFmpegVideo) or Defined(UseFFmpegDecoder)}
  {$DEFINE UseFFmpeg}
{$IFEND}

{$ENDIF PASDOC}