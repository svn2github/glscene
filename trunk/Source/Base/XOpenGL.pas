//
// This unit is part of the GLScene Project, http://glscene.org
//
{: XOpenGL<p>

   "Alternate" OpenGL functions to handle multi-texturing.<p>

   Using this functions allows specifying none/one/multiple ARB multi-texture
   coordinates with standard texture specification call.<p>

   Before using any of the xglTexCoordXxxx fonctions, call one of the
   xglMapTexCoordToXxxx functions to establish the redirectors.<p>

   This unit is Open-Source under MPL<br>
   Copyright 2001 - Eric Grange (egrange@glscene.org)<br>
   http://glscene.org<p>

   <b>History :</b><ul>
      <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
      <li>29/03/10 - Yar - Replaced MULTITHREADOPENGL to GLS_MULTITHREAD (thanks Controler)
      <li>16/03/07 - DaStr - Dropped Kylix support in favor of FPC
                             (thanks Burkhard Carstens) (BugTracekrID=1681585)
      <li>08/07/04 - LR - Removed ../ from the GLScene.inc
      <li>23/05/03 - EG - Support for arbitrary (complex) mappings
      <li>01/02/03 - EG - Added State stack
      <li>01/07/02 - EG - Added mtcmUndefined, fixed initial state
      <li>03/01/02 - EG - Added xglDisableClientState
      <li>26/01/02 - EG - Added xglBegin/EndUpdate mechanism
      <li>21/12/01 - EG - Fixed xglTexCoordPointer and xglEnableClientState
      <li>18/12/01 - EG - Added xglEnableClientState
      <li>24/08/01 - EG - Now supports MULTITHREADOPENGL (same as OpenGL1x)
      <li>17/08/01 - EG - Made declarations Kylix compatible (cdecl vs stdcall)
      <li>16/08/01 - EG - Renamed xglMapTextCoordMode to xglMapTexCoordMode
      <li>14/08/01 - EG - Added xglMapTexCoordToSecond
      <li>21/02/01 - EG - Added TexGen and vertex arrays mappings
   </ul>
}
unit XOpenGL;

interface

{$I GLScene.inc}

uses OpenGLTokens, GLContext;

type
  TMapTexCoordMode = (mtcmUndefined, mtcmNull, mtcmMain, mtcmDual, mtcmSecond,
    mtcmArbitrary);

  {: xglTexCoord functions will be ignored. }
procedure xglMapTexCoordToNull;
{: xglTexCoord functions will define the main texture coordinates. }
procedure xglMapTexCoordToMain;
{: xglTexCoord functions will define the second texture unit coordinates. }
procedure xglMapTexCoordToSecond;
{: xglTexCoord functions will define the two first texture units coordinates. }
procedure xglMapTexCoordToDual;
{: xglTexCoord functions will define the specified texture units coordinates. }
procedure xglMapTexCoordToArbitrary(const units: array of Cardinal); overload;
procedure xglMapTexCoordToArbitrary(const bitWiseUnits: Cardinal); overload;
procedure xglMapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);

{: Defers xglMap calls execution until xglEndUpdate is met.<p>
   Calls to xglBegin/EndUpdate may be nested. }
procedure xglBeginUpdate;
{: Applies xglMap calls if there were any since xglBeginUpdate was invoked.<p>
   Calls to xglBegin/EndUpdate may be nested. }
procedure xglEndUpdate;

{: Saves XOpenGL State on the stack. }
procedure xglPushState;
{: Restores XOpenGL State from the stack. }
procedure xglPopState;

{: Whenever called, 2nd texture units changes will be forbidden to xgl.<p>
   Use this function when you're using the 2nd texture unit for your own
   purposes and don't want XOpenGL to alter it. }
procedure xglForbidSecondTextureUnit;
{: Allow XOpenGL to use the second texture unit again. }
procedure xglAllowSecondTextureUnit;
{: Returns the complex mapping in bitwise form. }
function xglGetBitWiseMapping: Cardinal;

{$IFDEF GLS_MULTITHREAD}
threadvar
{$ELSE}
var
{$ENDIF}

  xglMapTexCoordMode: TMapTexCoordMode;
  vSecondTextureUnitForbidden: Boolean;

  // Explicit texture coordinates specification
  xglTexCoord2f: procedure(s, t: TGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexCoord2fv: procedure(v: PGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexCoord3f: procedure(s, t, r: TGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexCoord3fv: procedure(v: PGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexCoord4f: procedure(s, t, r, q: TGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexCoord4fv: procedure(v: PGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}

  // TexGen texture coordinates specification
  xglTexGenf: procedure(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexGenfv: procedure(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexGeni: procedure(coord, pname: TGLEnum; param: TGLint);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglTexGeniv: procedure(coord, pname: TGLEnum; params: PGLint);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}

  // Vertex Arrays texture coordinates specification
  xglTexCoordPointer: procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei;
    data: pointer);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglEnableClientState: procedure(aarray: TGLEnum);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglDisableClientState: procedure(aarray: TGLEnum);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}

  // Misc
  xglEnable: procedure(cap: TGLEnum);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}
  xglDisable: procedure(cap: TGLEnum);
{$IFDEF Win32}stdcall;
{$ENDIF}{$IFDEF unix}cdecl;
{$ENDIF}

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vUpdCount: Integer;
  vUpdNewMode: TMapTexCoordMode;
  vStateStack: array of TMapTexCoordMode;
  vComplexMapping: array of Cardinal;
  vComplexMappingN: Integer;

  // ------------------------------------------------------------------
  // Multitexturing coordinates duplication functions
  // ------------------------------------------------------------------

  // --------- Complex (arbitrary) mapping

procedure glTexCoord2f_Arbitrary(s, t: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord2f(vComplexMapping[i], s, t);
end;

procedure glTexCoord2fv_Arbitrary(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord2fv(vComplexMapping[i], v);
end;

procedure glTexCoord3f_Arbitrary(s, t, r: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord3f(vComplexMapping[i], s, t, r);
end;

procedure glTexCoord3fv_Arbitrary(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord3fv(vComplexMapping[i], v);
end;

procedure glTexCoord4f_Arbitrary(s, t, r, q: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord4f(vComplexMapping[i], s, t, r, q);
end;

procedure glTexCoord4fv_Arbitrary(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
    GL.MultiTexCoord4fv(vComplexMapping[i], v);
end;

procedure glTexGenf_Arbitrary(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.TexGenf(coord, pname, param);
  end;
end;

procedure glTexGenfv_Arbitrary(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.TexGenfv(coord, pname, params);
  end;
end;

procedure glTexGeni_Arbitrary(coord, pname: TGLEnum; param: TGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.TexGeni(coord, pname, param);
  end;
end;

procedure glTexGeniv_Arbitrary(coord, pname: TGLEnum; params: PGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.TexGeniv(coord, pname, params);
  end;
end;

procedure glEnable_Arbitrary(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.Enable(cap);
  end;
end;

procedure glDisable_Arbitrary(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    CurrentGLContext.GLStates.ActiveTexture := vComplexMapping[i];
    GL.Disable(cap);
  end;
end;

procedure xglTexCoordPointer_Arbitrary(size: TGLint; atype: TGLEnum; stride:
  TGLsizei; data: pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    GL.ClientActiveTexture(vComplexMapping[i]);
    GL.TexCoordPointer(size, atype, stride, data);
  end;
end;

procedure xglEnableClientState_Arbitrary(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    GL.ClientActiveTexture(vComplexMapping[i]);
    GL.EnableClientState(aArray);
  end;
end;

procedure xglDisableClientState_Arbitrary(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to vComplexMappingN do
  begin
    GL.ClientActiveTexture(vComplexMapping[i]);
    GL.DisableClientState(aArray);
  end;
end;

// --------- Second unit Texturing

procedure glTexCoord2f_Second(s, t: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure glTexCoord2fv_Second(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure glTexCoord3f_Second(s, t, r: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure glTexCoord3fv_Second(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure glTexCoord4f_Second(s, t, r, q: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure glTexCoord4fv_Second(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.MultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure glTexGenf_Second(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.TexGenf(coord, pname, param);
end;

procedure glTexGenfv_Second(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.TexGenfv(coord, pname, params);
end;

procedure glTexGeni_Second(coord, pname: TGLEnum; param: TGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.TexGeni(coord, pname, param);
end;

procedure glTexGeniv_Second(coord, pname: TGLEnum; params: PGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.TexGeniv(coord, pname, params);
end;

procedure glEnable_Second(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.Enable(cap);
end;

procedure glDisable_Second(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  CurrentGLContext.GLStates.ActiveTexture := 1;
  GL.Disable(cap);
end;

procedure xglTexCoordPointer_Second(size: TGLint; atype: TGLEnum; stride:
  TGLsizei; data: pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.TexCoordPointer(size, atype, stride, data);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

procedure xglEnableClientState_Second(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.EnableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

procedure xglDisableClientState_Second(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.DisableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

// --------- Dual Texturing

procedure glTexCoord2f_Dual(s, t: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord2f(s, t);
  GL.MultiTexCoord2f(GL_TEXTURE1, s, t);
end;

procedure glTexCoord2fv_Dual(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord2fv(v);
  GL.MultiTexCoord2fv(GL_TEXTURE1, v);
end;

procedure glTexCoord3f_Dual(s, t, r: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord3f(s, t, r);
  GL.MultiTexCoord3f(GL_TEXTURE1, s, t, r);
end;

procedure glTexCoord3fv_Dual(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord3fv(v);
  GL.MultiTexCoord3fv(GL_TEXTURE1, v);
end;

procedure glTexCoord4f_Dual(s, t, r, q: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord4f(s, t, r, q);
  GL.MultiTexCoord4f(GL_TEXTURE1, s, t, r, q);
end;

procedure glTexCoord4fv_Dual(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoord4fv(v);
  GL.MultiTexCoord4fv(GL_TEXTURE1, v);
end;

procedure glTexGenf_Dual(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.TexGenf(coord, pname, param);
    ActiveTexture := 1;
    GL.TexGenf(coord, pname, param);
  end;
end;

procedure glTexGenfv_Dual(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.TexGenfv(coord, pname, params);
    ActiveTexture := 1;
    GL.TexGenfv(coord, pname, params);
  end;
end;

procedure glTexGeni_Dual(coord, pname: TGLEnum; param: TGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.TexGeni(coord, pname, param);
    ActiveTexture := 1;
    GL.TexGeni(coord, pname, param);
  end;
end;

procedure glTexGeniv_Dual(coord, pname: TGLEnum; params: PGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.TexGeniv(coord, pname, params);
    ActiveTexture := 1;
    GL.TexGeniv(coord, pname, params);
  end;
end;

procedure glEnable_Dual(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.Enable(cap);
    ActiveTexture := 1;
    GL.Enable(cap);
  end;
end;

procedure glDisable_Dual(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  with CurrentGLContext.GLStates do
  begin
    ActiveTexture := 0;
    GL.Disable(cap);
    ActiveTexture := 1;
    GL.Disable(cap);
  end;
end;

procedure xglTexCoordPointer_Dual(size: TGLint; atype: TGLEnum; stride:
  TGLsizei; data: pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.TexCoordPointer(size, atype, stride, data);
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.TexCoordPointer(size, atype, stride, data);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

procedure xglEnableClientState_Dual(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.EnableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.EnableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

procedure xglDisableClientState_Dual(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
  GL.DisableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE1);
  GL.DisableClientState(aArray);
  GL.ClientActiveTexture(GL_TEXTURE0);
end;

// --------- Null Texturing

procedure glTexCoord2f_Null(s, t: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexCoord2fv_Null(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexCoord3f_Null(s, t, r: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexCoord3fv_Null(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexCoord4f_Null(s, t, r, q: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexCoord4fv_Null(v: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexGenf_Null(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexGenfv_Null(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexGeni_Null(coord, pname: TGLEnum; param: TGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glTexGeniv_Null(coord, pname: TGLEnum; params: PGLint);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glEnable_Null(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure glDisable_Null(cap: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure xglTexCoordPointer_Null(size: TGLint; atype: TGLEnum; stride:
  TGLsizei; data: pointer);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure xglEnableClientState_Null(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

procedure xglDisableClientState_Null(aArray: TGLEnum);
{$IFDEF Win32} stdcall;
{$ENDIF}{$IFDEF unix} cdecl;
{$ENDIF}
begin
end;

// ------------------------------------------------------------------
// Redirections management functions
// ------------------------------------------------------------------

// xglBeginUpdate
//

procedure xglBeginUpdate;
begin
  if vUpdCount = 0 then
  begin
    vUpdCount := 1;
    vUpdNewMode := xglMapTexCoordMode;
  end
  else
    Inc(vUpdCount);
end;

// xglEndUpdate
//

procedure xglEndUpdate;
begin
  Dec(vUpdCount);
  if (vUpdCount = 0) and (vUpdNewMode <> xglMapTexCoordMode) then
  begin
    case vUpdNewMode of
      mtcmNull: xglMapTexCoordToNull;
      mtcmMain: xglMapTexCoordToMain;
      mtcmDual: xglMapTexCoordToDual;
      mtcmSecond: xglMapTexCoordToSecond;
      mtcmArbitrary: xglMapTexCoordToArbitrary(vComplexMapping);
    else
      Assert(False);
    end;
  end;
end;

// xglPushState
//

procedure xglPushState;
var
  i: Integer;
begin
  Assert(vUpdCount = 0);
  i := Length(vStateStack);
  SetLength(vStateStack, i + 1);
  vStateStack[i] := xglMapTexCoordMode;
end;

// xglPopState
//

procedure xglPopState;
var
  i: Integer;
begin
  Assert(vUpdCount = 0);
  i := Length(vStateStack) - 1;
  Assert(i >= 0);
  case vStateStack[i] of
    mtcmNull: xglMapTexCoordToNull;
    mtcmMain: xglMapTexCoordToMain;
    mtcmDual: xglMapTexCoordToDual;
    mtcmSecond: xglMapTexCoordToSecond;
    mtcmArbitrary: xglMapTexCoordToArbitrary(vComplexMapping);
  else
    Assert(False);
  end;
  SetLength(vStateStack, i);
end;

// xglForbidSecondTextureUnit
//

procedure xglForbidSecondTextureUnit;
begin
  vSecondTextureUnitForbidden := True;
end;

// xglAllowSecondTextureUnit
//

procedure xglAllowSecondTextureUnit;
begin
  vSecondTextureUnitForbidden := False;
end;

// xglMapTexCoordToNull
//

procedure xglMapTexCoordToNull;
begin
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmNull
  else if xglMapTexCoordMode <> mtcmNull then
  begin
    xglMapTexCoordMode := mtcmNull;

    xglTexCoord2f := glTexCoord2f_Null;
    xglTexCoord2fv := glTexCoord2fv_Null;
    xglTexCoord3f := glTexCoord3f_Null;
    xglTexCoord3fv := glTexCoord3fv_Null;
    xglTexCoord4f := glTexCoord4f_Null;
    xglTexCoord4fv := glTexCoord4fv_Null;

    xglTexGenf := glTexGenf_Null;
    xglTexGenfv := glTexGenfv_Null;
    xglTexGeni := glTexGeni_Null;
    xglTexGeniv := glTexGeniv_Null;

    xglTexCoordPointer := xglTexCoordPointer_Null;
    xglEnableClientState := xglEnableClientState_Null;
    xglDisableClientState := xglDisableClientState_Null;

    xglEnable := glEnable_Null;
    xglDisable := glDisable_Null;
  end;
end;

// xglTexCoordMapToMain
//

procedure xglMapTexCoordToMain;
begin
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmMain
  else if xglMapTexCoordMode <> mtcmMain then
  begin
    xglMapTexCoordMode := mtcmMain;

    xglTexCoord2f := GL.TexCoord2f;
    xglTexCoord2fv := GL.TexCoord2fv;
    xglTexCoord3f := GL.TexCoord3f;
    xglTexCoord3fv := GL.TexCoord3fv;
    xglTexCoord4f := GL.TexCoord4f;
    xglTexCoord4fv := GL.TexCoord4fv;

    xglTexGenf := GL.TexGenf;
    xglTexGenfv := GL.TexGenfv;
    xglTexGeni := GL.TexGeni;
    xglTexGeniv := GL.TexGeniv;

    xglTexCoordPointer := GL.TexCoordPointer;
    xglEnableClientState := GL.EnableClientState;
    xglDisableClientState := GL.DisableClientState;

    xglEnable := GL.Enable;
    xglDisable := GL.Disable;
  end;
end;

// xglTexCoordMapToSecond
//

procedure xglMapTexCoordToSecond;
begin
  if vSecondTextureUnitForbidden then
  begin
    xglMapTexCoordToNull;
    Exit;
  end;
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmSecond
  else if xglMapTexCoordMode <> mtcmSecond then
  begin
    xglMapTexCoordMode := mtcmSecond;
    Assert(GL.ARB_multitexture);

    xglTexCoord2f := glTexCoord2f_Second;
    xglTexCoord2fv := glTexCoord2fv_Second;
    xglTexCoord3f := glTexCoord3f_Second;
    xglTexCoord3fv := glTexCoord3fv_Second;
    xglTexCoord4f := glTexCoord4f_Second;
    xglTexCoord4fv := glTexCoord4fv_Second;

    xglTexGenf := glTexGenf_Second;
    xglTexGenfv := glTexGenfv_Second;
    xglTexGeni := glTexGeni_Second;
    xglTexGeniv := glTexGeniv_Second;

    xglTexCoordPointer := xglTexCoordPointer_Second;
    xglEnableClientState := xglEnableClientState_Second;
    xglDisableClientState := xglDisableClientState_Second;

    xglEnable := glEnable_Second;
    xglDisable := glDisable_Second;
  end;
end;

// xglTexCoordMapToDual
//

procedure xglMapTexCoordToDual;
begin
  if vSecondTextureUnitForbidden then
  begin
    xglMapTexCoordToMain;
    Exit;
  end;
  if vUpdCount <> 0 then
    vUpdNewMode := mtcmDual
  else if xglMapTexCoordMode <> mtcmDual then
  begin
    xglMapTexCoordMode := mtcmDual;
    Assert(GL.ARB_multitexture);

    xglTexCoord2f := glTexCoord2f_Dual;
    xglTexCoord2fv := glTexCoord2fv_Dual;
    xglTexCoord3f := glTexCoord3f_Dual;
    xglTexCoord3fv := glTexCoord3fv_Dual;
    xglTexCoord4f := glTexCoord4f_Dual;
    xglTexCoord4fv := glTexCoord4fv_Dual;

    xglTexGenf := glTexGenf_Dual;
    xglTexGenfv := glTexGenfv_Dual;
    xglTexGeni := glTexGeni_Dual;
    xglTexGeniv := glTexGeniv_Dual;

    xglTexCoordPointer := xglTexCoordPointer_Dual;
    xglEnableClientState := xglEnableClientState_Dual;
    xglDisableClientState := xglDisableClientState_Dual;

    xglEnable := glEnable_Dual;
    xglDisable := glDisable_Dual;
  end;
end;

// xglMapTexCoordToArbitrary (array)
//

procedure xglMapTexCoordToArbitrary(const units: array of Cardinal);
var
  i, j, n: Integer;
begin
  n := Length(units);
  SetLength(vComplexMapping, n);
  j := 0;
  vComplexMappingN := n - 1;
  for i := 0 to vComplexMappingN do
  begin
    if (not vSecondTextureUnitForbidden) or (units[i] <> GL_TEXTURE1) then
    begin
      vComplexMapping[j] := units[i];
      Inc(j);
    end;
  end;

  if vUpdCount <> 0 then
    vUpdNewMode := mtcmArbitrary
  else if xglMapTexCoordMode <> mtcmArbitrary then
  begin

    xglMapTexCoordMode := mtcmArbitrary;
    Assert(GL.ARB_multitexture);

    xglTexCoord2f := glTexCoord2f_Arbitrary;
    xglTexCoord2fv := glTexCoord2fv_Arbitrary;
    xglTexCoord3f := glTexCoord3f_Arbitrary;
    xglTexCoord3fv := glTexCoord3fv_Arbitrary;
    xglTexCoord4f := glTexCoord4f_Arbitrary;
    xglTexCoord4fv := glTexCoord4fv_Arbitrary;

    xglTexGenf := glTexGenf_Arbitrary;
    xglTexGenfv := glTexGenfv_Arbitrary;
    xglTexGeni := glTexGeni_Arbitrary;
    xglTexGeniv := glTexGeniv_Arbitrary;

    xglTexCoordPointer := xglTexCoordPointer_Arbitrary;
    xglEnableClientState := xglEnableClientState_Arbitrary;
    xglDisableClientState := xglDisableClientState_Arbitrary;

    xglEnable := glEnable_Arbitrary;
    xglDisable := glDisable_Arbitrary;
  end;
end;

// xglMapTexCoordToArbitrary (bitwise)
//

procedure xglMapTexCoordToArbitrary(const bitWiseUnits: Cardinal);
var
  i, n: Integer;
  units: array of Cardinal;
begin
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
      Inc(n);
  end;
  SetLength(units, n);
  n := 0;
  for i := 0 to 7 do
  begin
    if (bitWiseUnits and (1 shl i)) <> 0 then
    begin
      units[n] := GL_TEXTURE0 + i;
      Inc(n);
    end;
  end;
  xglMapTexCoordToArbitrary(units);
end;

// xglMapTexCoordToArbitrary (bitwise)
//

procedure xglMapTexCoordToArbitraryAdd(const bitWiseUnits: Cardinal);
var
  n: Cardinal;
begin
  n := xglGetBitWiseMapping;
  xglMapTexCoordToArbitrary(n or bitWiseUnits);
end;

// xglGetBitWiseMapping
//

function xglGetBitWiseMapping: Cardinal;
var
  i, n: Cardinal;
  mode: TMapTexCoordMode;
begin
  if vUpdCount > 0 then
    mode := vUpdNewMode
  else
    mode := xglMapTexCoordMode;
  n := 0;
  case mode of
    mtcmMain: n := 1;
    mtcmDual: n := 3;
    mtcmSecond: n := 2;
    mtcmArbitrary:
      begin
        for i := 0 to vComplexMappingN do
          n := n or (1 shl (vComplexMapping[i] - GL_TEXTURE0));
      end;
  else
    Assert(False);
  end;
  Result := n;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  xglMapTexCoordMode := mtcmUndefined;
  xglMapTexCoordToNull;

end.

