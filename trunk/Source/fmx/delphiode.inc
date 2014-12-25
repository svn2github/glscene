{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{       Global Conditional Definitions for JEDI-SDL cross-compilation          }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{                                                                              }
{ Portions created by Prof. Abimbola Olowofoyeku are                           }
{ Copyright (C) 2000 - 2100 Prof. Abimbola Olowofoyeku.                        }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{  Prof. Abimbola Olowofoyeku <http://www.bigfoot.com/~African_Chief/>         }
{  Dominqiue Louis <Dominique@SavageSoftware.com.au>                           }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{ This code has been copied from...                                            }
{ Global Conditional Definitions for Chief's UNZIP package                     }
{ By Prof. Abimbola Olowofoyeku (The African Chief)                            }
{          http://www.bigfoot.com/~African_Chief/                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL.so          }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   2003-04-03   DL - Initial addition                                         }
{                                                                              }
{   2003-04-07   DL - Added Macro ON derective for FPC and OpenGL and removed  }
{                WEAKPACKAGE derective. WEAKPACKAGE should be set when         }
{                appropriate.                                                  }
{                                                                              }
{   2003-04-23 - DL : under instruction from Alexey Barkovoy I have added      }
{                          better TMT Pascal support and under instruction     }
{                          from Prof. Abimbola Olowofoyeku (The African Chief) }
{                          I have added better Gnu Pascal support              }
{                                                                              }
{   2004-01-19 - DL : Under instruction from Marco van de Voort, I have added  }
{                     Better FPC support for FreeBSD.                          }
{                                                                              }
{******************************************************************************}

{.$define Debug}           { uncomment for debugging }

{$IFNDEF FPC}
  {$IFDEF __GPC__}
    {$I-}
    {$W-}
    {$X+}
  {$ELSE} {__GPC__}
    {$IFDEF Debug}
      {$F+,D+,Q-,L+,R+,I-,S+,Y+,A+}
    {$ELSE}
      {$F+,Q-,R-,S-,I-,A+}
    {$ENDIF}
  {$ENDIF} {__GPC__}
{$ELSE}  {FPC}
  {$M+}
{$ENDIF} {FPC}

{$IFDEF Delphi}
  {$DEFINE Windows}
  {$DEFINE USE_STDCALL}
{$ENDIF Delphi}


{$IFDEF __GPC__}
  {$UNDEF UseWin}
  {$UNDEF USE_STDCALL}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE NOCRT}
  {$DEFINE cdecl attribute(cdecl)}
{$ENDIF}

{$IFDEF __TMT__}
  {$DEFINE OS_BigMem}
  {$DEFINE NO_EXPORTS}
  {$DEFINE __OS_DOS__}
  {$DEFINE UseAT}
  {$IFNDEF MSDOS}
    {$DEFINE USE_STDCALL}
  {$ENDIF}

  {$IFDEF __WIN32__}
    {$DEFINE Win32}
    {$DEFINE UseWin}
    {$DEFINE NOCRT}
    {$DEFINE Win32}
    {$IFNDEF __CON__}
      {$DEFINE Windows}
    {$ENDIF}
  {$ENDIF}

  {$A+}  // Word alignment data
  {$OA+} // Objects and structures align
{$ENDIF}

{$IFDEF Win32}
  {$H-}                    { no long strings, please! }
  {$DEFINE OS_BigMem}
{$ELSE Win32}
  {$IFDEF ver70}
    {$DEFINE assembler}
  {$ENDIF} { use 16-bit assembler! }
{$ENDIF Win32}

{ ************************** dos/dos-like platforms **************}
{$IFDEF Windows}
   {$DEFINE __OS_DOS__}
   {$DEFINE UseWin}
   {$DEFINE MSWINDOWS}
{$ENDIF Delphi}

{$IFDEF UseWin}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF BP_DPMI}
   {$DEFINE Can_Use_DLL}
{$ENDIF}

{$IFDEF USE_STDCALL}
   {$IFNDEF __TMT__}
     {$DEFINE BY_NAME}
   {$ENDIF}
{$ENDIF}

{$IFNDEF ver70}
  {$UNDEF assembler}
{$ENDIF}
